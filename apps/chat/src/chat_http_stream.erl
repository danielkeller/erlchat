%%%-------------------------------------------------------------------
%%% @author dan
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2017 3:07 PM
%%%-------------------------------------------------------------------
-module(chat_http_stream).
-author("dan").

-behaviour(gen_statem).

%% API
-export([start_link/0, take_socket/2]).

%% gen_statem callbacks
-export([
    init/1, callback_mode/0,
    handle_event/4,
    terminate/3,
    code_change/4
]).

-define(SERVER, ?MODULE).

-record(request, {method, path, headers, body = <<>>}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_statem:start_link(?MODULE, [], []).

%% Socket must be controlled by calling process
take_socket(Pid, Socket) ->
    gen_tcp:controlling_process(Socket, Pid),
    gen_statem:cast(Pid, {go, Socket}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, no_socket, undefined}.

callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @spec handle_event(EventType, Event, State, Data) ->
%%                   {next_state, NextStateName, NewData} |
%%                   {next_state, NextStateName, NewData, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
handle_event(_EventType, {go, Socket},
    no_socket, _) ->
    inet:setopts(Socket, [{active, once}]),
    {next_state, idle, undefined};

handle_event(info, {http, Socket, {http_request, Method, {abs_path, Path}, _}},
    idle, _) ->
    inet:setopts(Socket, [{active, once}]),
    Data = #request{method=Method, path=cow_http:parse_fullpath(Path), headers=#{}},
    {next_state, headers, Data};

handle_event(info, {http, Socket, _},
    idle, _) ->
    bad_request(Socket);

handle_event(info, {http, Socket, {http_header, _, Name, _, Value}},
    headers, Data) ->
    inet:setopts(Socket, [{active, once}]),
    Parsed = parse_header(Name, Value),
    Headers1 = (Data#request.headers)#{Name => Parsed},
    Data1 = Data#request{headers = Headers1},
    {next_state, headers, Data1};

handle_event(info, {http, Socket, http_eoh},
    headers, Data) ->
    inet:setopts(Socket, [{active, false}, {packet, raw}]),
    handle(Socket, Data),

    case Data#request.headers of
        #{'Content-Length' := Len} -> {next_state, {body, Socket, Len}, Data};
        %% chunked...
        _ -> {next_state, {body, Socket, 0}, Data}
    end;

handle_event({call, From}, {read, some}, {body, Socket, 0}, _) ->
    {next_state, {done, Socket}, undefined, [{reply, From, done}]};

handle_event({call, From}, {read, some}, {body, Socket, Left}, Data) ->
    %% pipelining will break this
    {ok, Bytes} = gen_tcp:recv(Socket, 0),
    {next_state, {body, Socket, Left - byte_size(Bytes)},
        Data,
        [{reply, From, {more, Bytes}}]};

handle_event({call, From}, {read, all}, {body, Socket, Left}, _) ->
    {ok, Bytes} = gen_tcp:recv(Socket, Left),
    {next_state, {done, Socket}, undefined, [{reply, From, Bytes}]};

%% The request handler didn't read all of the data, but returned
handle_event(cast, finish, {body, Socket, Left}, Data) ->
    gen_tcp:recv(Socket, Left),
    inet:setopts(Socket, [{active, once}, {packet, http_bin}]),
    {next_state, idle, Data};

handle_event(cast, finish, {done, Socket}, Data) ->
    inet:setopts(Socket, [{active, once}, {packet, http_bin}]),
    {next_state, idle, Data};

handle_event(info, {http, Socket, {http_error,_}}, _, _) ->
    bad_request(Socket);

handle_event(info, {tcp_closed, _}, _, _) ->
    stop;

handle_event(Type, Event, State, Data) ->
    io:format("chat_http:handle_event(~p, ~p, ~p, ~p)~n",
        [Type, Event, State, Data]),
    keep_state_and_data.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% To get more body data, it might be neccessary to test the pid,
%% and use gen_statem:call/2 conditionally

parse_header('Cookie', Value) -> cow_cookie:parse_cookie(Value);
parse_header('Content-Length', Value) -> cow_http_hd:parse_content_length(Value);
parse_header(_, Value) -> Value.

bad_request(Socket) ->
    gen_tcp:send(Socket, "HTTP/1.1 400 Bad Request\r\n\r\n"),
    gen_tcp:close(Socket),
    stop.

print_whole_body(Http) ->
    print_whole_body(Http,
        gen_statem:call(Http, {read, some})).

print_whole_body(_, done) -> ok;
print_whole_body(Http, {more, Bytes}) ->
    io:format("Chunk: ~ts~n", [Bytes]),
    print_whole_body(Http).

%print_all_body(Http) ->
%    Body = gen_statem:call(Http, {read, all}),
%    io:format("Got: ~ts~n", [Body]).
%
%print_some_body(Http) ->
%    case gen_statem:call(Http, {read, some}) of
%        {more, Bytes} -> io:format("Got some: ~ts~n", [Bytes]);
%        done -> io:format("empty body~n")
%    end.

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)).

reply(Request, Socket) ->
    Content = io_lib:format(<<"Hello from ~p~n">>, [Request]),
    BinContent = iolist_to_binary(Content),

    send(Socket, "HTTP/1.1 200 OK", []),
    send(Socket, "Content-Type: text/plain", []),
    send(Socket, "Content-Length: ~b", [byte_size(BinContent)]),
    send(Socket, "", []),
    gen_tcp:send(Socket, BinContent).

handle(Socket, Request) ->
    Http = self(),

    spawn(fun() ->
        print_whole_body(Http),
        reply(Request, Socket),
        gen_statem:cast(Http, finish)
          end),
    ok.

