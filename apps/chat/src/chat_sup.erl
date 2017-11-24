%%%-------------------------------------------------------------------
%% @doc chat top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 60,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 3000,
    Type = worker,

    Chat = {chat, {chat_chat_sup, start_link, []},
        Restart, Shutdown, Type, [chat_chat_sup]},

    Server = {server, {chat_http_sup, start_link, []},
        Restart, Shutdown, Type, [chat_http_sup]},

    Acceptor = {acceptor, {chat_acc, start_link, []},
        Restart, Shutdown, Type, [chat_acc]},

    {ok, {SupFlags, [Chat, Server, Acceptor]}}.

%%====================================================================
%% Internal functions
%%====================================================================
