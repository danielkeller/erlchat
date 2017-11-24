-module(chat_chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_or_create_chat/1]).

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

-spec get_or_create_chat(_) -> pid().
get_or_create_chat(Key) ->
    %% This is a double-checked lock pattern...
    case get_chat(Key) of
        undefined -> get_or_start(Key);
        Pid -> Pid
    end.

-spec get_chat(_) -> pid() | 'undefined'.
get_chat(Key) ->
    chat_chat:get(Key).

-spec get_or_start(_) -> pid().
get_or_start(Key) ->
    %% ...because this function is synchronized
    case supervisor:start_child(?SERVER, [Key]) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

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

    AChild = {worker, {chat_chat, start_link, []},
        Restart, Shutdown, Type, [chat_chat]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
