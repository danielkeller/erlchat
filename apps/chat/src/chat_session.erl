-module(chat_session).

%% API
-export([middleware/1, get_session/3]).

%%%===================================================================
%%% API
%%%===================================================================

-spec(middleware(chat_http:view()) -> chat_http:view()).
middleware(Next) ->
    fun (Request) ->
        {Session, New} = cookie(Request),
        Request1 = chat_http:meta(Request, session, Session),

        {S, H, B} = Next(Request1),

        H1 = if
            New -> [{<<"Set-Cookie">>, setcookie(Session)} | H];
            not New -> H
        end,

        {S, H1, B}
    end.

-spec(get_session(chat_http:request(), atom(), any()) -> any()).
get_session(Request, Key, Default) ->
    Session = chat_http:meta(Request, session),
    chat_data:session(Session, Key, Default).

%%Modifies the session and returns the new value
%-spec(modify_session(chat_http:request(), any(), fun((any()) -> Y)) -> Y).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(newcookie() -> binary()).
newcookie() ->
    base64:encode(crypto:strong_rand_bytes(32)).

-spec(cookie(chat_http:request()) -> {binary(), boolean()}).
cookie(Request) ->
    case chat_http:header(Request, 'Cookie') of
        undefined -> {newcookie(), true};
        C ->
            case proplists:lookup(<<"sessionid">>, C) of
                none -> {newcookie(), true};
                {_, K} -> {K, false}
            end
    end.

-spec(setcookie(binary()) -> binary()).
setcookie(Session) ->
    cow_cookie:setcookie(<<"sessionid">>, Session, []).
