-module(chat_acc_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {sock :: port()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(port()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSock) ->
    gen_server:start_link(?MODULE, [ListenSock], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([ListenSock]) ->
    gen_server:cast(self(), accept),
    {ok, #state{sock = ListenSock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(accept, State = #state{sock = ListenSock}) ->
    case gen_tcp:accept(ListenSock, 2000) of
        {ok, Socket} -> chat_http_sup:handle_client(Socket);
        {error, timeout} -> ok
    end,
    gen_server:cast(self(), accept),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
