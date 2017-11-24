-module(chat_acc).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: pos_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }}).
init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSock} = gen_tcp:listen(Port, [
        {active, false}, {reuseaddr, true},
        {packet, http_bin}, {mode, binary}]),

    RestartStrategy = one_for_one,
    MaxRestarts = 60,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 3000,
    Type = worker,

    Child = fun(Name) -> {Name, {chat_acc_worker, start_link, [ListenSock]},
        Restart, Shutdown, Type, [chat_acc_worker]} end,

    {ok, {SupFlags, lists:map(Child, lists:seq(1, 10))}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
