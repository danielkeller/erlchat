%%%-------------------------------------------------------------------
%% @doc chat public API
%% @end
%%%-------------------------------------------------------------------

-module(chat).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, main/1]).

%%====================================================================
%% API
%%====================================================================

start(normal, _StartArgs) ->
    io:format("starting~n"),
    chat_data:init(), %% starts mnesia
    chat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    io:format("stopping~n"),
    ok.

main(Args) ->
    io:format("hello ~p~n", [Args]).

%%====================================================================
%% Internal functions
%%====================================================================
