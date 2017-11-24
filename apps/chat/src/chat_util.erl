
-module(chat_util).

%% API
-export([static_file/1]).

static_file(Path) ->
    {ok, Data} = file:read_file(code:priv_dir(chat) ++ "/" ++ Path),
    Data.