-module(chat_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, handle_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec(handle_client(port()) -> ok).
handle_client(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, []),
    chat_http:take_socket(Pid, Socket).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: pos_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }}).
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    Child = {http, {chat_http, start_link, []},
        Restart, Shutdown, Type, [chat_http]},

    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
