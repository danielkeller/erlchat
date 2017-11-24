%%%-------------------------------------------------------------------
%%% @author dan
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2017 3:07 PM
%%%-------------------------------------------------------------------
-module(chat_http).
-author("dan").

-behaviour(gen_statem).

%% API
-export([start_link/0, take_socket/2]).
-export([header/2, body/1, meta/2, meta/3]).
-export_type([view/0, request/0]).

%% gen_statem callbacks
-export([
    init/1, callback_mode/0,
    handle_event/4,
    terminate/3,
    code_change/4
]).

-define(SERVER, ?MODULE).

-record(request, {method, path, query, headers = #{}, meta = #{}, body}).

%%%===================================================================
%%% API
%%%===================================================================
-type response_headers() :: [{iolist(), iolist()}].
-type response_stream() :: fun(() -> 'done' | {'more', iolist(), response_stream()}).
-type response() :: {integer(), response_headers(), iolist() | response_stream()}.
-type view() :: fun((#request{}) -> response()).
-opaque request() :: #request{}.

-spec(header(request(), any()) -> any() | undefined).
header(#request{headers = H}, K) -> maps:get(K, H, undefined).
-spec(body(request()) -> binary()).
body(#request{body = B}) -> B.
-spec(meta(request(), any()) -> any()).
meta(#request{meta = M}, K) -> maps:get(K, M).
-spec(meta(request(), any(), any()) -> request()).
meta(R = #request{meta = M}, K, V) -> R#request{meta = M#{K => V}}.

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

handle_event(info, {http, Socket, {http_request, Method, {abs_path, Fullpath}, _}},
        idle, _) ->
    inet:setopts(Socket, [{active, once}]),
    {Path, Query} = cow_http:parse_fullpath(Fullpath),
    Data = #request{method=Method, path=Path, query=Query},
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
    Body = case Data#request.headers of
        #{'Content-Length' := Len} ->
            inet:setopts(Socket, [{packet, raw}]),
            {ok, Bytes} = gen_tcp:recv(Socket, Len),
            Bytes;
        _ -> <<>>
    end,
    Request = Data#request{body = Body},
    handle(Socket, Request),
    inet:setopts(Socket, [{active, once}, {packet, http_bin}]),
    {next_state, idle, undefined};

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

parse_header('Cookie', Value) -> cow_cookie:parse_cookie(Value);
parse_header('Content-Length', Value) -> cow_http_hd:parse_content_length(Value);
parse_header(_, Value) -> Value.

bad_request(Socket) ->
    gen_tcp:send(Socket, "HTTP/1.1 400 Bad Request\r\n\r\n"),
    gen_tcp:close(Socket),
    stop.

handle(Socket, Request = #request{path=Path, method=Method}) ->
    reply(Socket, chat_routing:route(Method, Path, Request)).

reply(Socket, {Status, Headers, Body}) when is_function(Body) ->
    Response = cow_http:response(Status, 'HTTP/1.1',
        [{<<"Transfer-Encoding">>, <<"chunked">>} | Headers]),
    gen_tcp:send(Socket, Response),
    send_chunks(Socket, Body());

reply(Socket, {Status, Headers, Body}) ->
    BinContent = iolist_to_binary(Body),
    Response = cow_http:response(Status, 'HTTP/1.1',
        [{<<"Content-Length">>, header_len(BinContent)} | Headers]),
    gen_tcp:send(Socket, [Response, BinContent]).

send_chunks(Socket, {more, Data, Fun}) ->
    BinData = iolist_to_binary(Data),
    Chunk = [chunk_len(BinData), "\r\n", BinData, "\r\n"],
    gen_tcp:send(Socket, Chunk),
    send_chunks(Socket, Fun());

send_chunks(Socket, done) ->
    gen_tcp:send(Socket, <<"0\r\n\r\n">>).

header_len(Bytes) -> io_lib:format("~B", [byte_size(Bytes)]).
chunk_len(Bytes) -> io_lib:format("~.16B", [byte_size(Bytes)]).
