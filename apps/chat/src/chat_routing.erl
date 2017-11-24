-module(chat_routing).

-export([route/3]).

-spec(route(atom(), binary(), chat_http:request()) -> chat_http:response()).
route('GET', <<"/chat/", Name:10/bytes, "/", SinceStr/bytes>>, Request) ->
    {Since, _} = string:to_integer(SinceStr),
    View = fun (Req) -> chat_views:messages(Since, Name, Req) end,
    (chat_session:middleware(View))(Request);

route('POST', <<"/chat/", Name:10/bytes>>, Request) ->
    View = fun (Req) -> chat_views:send_message(Name, Req) end,
    (chat_session:middleware(View))(Request);

route('GET', <<"/index.js">>, _) ->
    {200, [{<<"Content-Type">>, <<"text/javscript">>}],
        chat_util:static_file("index.js")};

route('GET', <<"/chat.js">>, _) ->
    {200, [{<<"Content-Type">>, <<"text/javscript">>}],
        chat_util:static_file("chat.js")};

route('GET', <<"/favicon.", _:3/bytes>>, _) ->
    {200, [{<<"Content-Type">>, <<"image/png">>}],
        chat_util:static_file("favicon.png")};

route('GET', <<"/">>, _) ->
    {200, [{<<"Content-Type">>, <<"text/html">>}],
        chat_util:static_file("index.html")};

route('GET', <<"/c/", _:10/bytes>>, Request) ->
    (chat_session:middleware(fun chat_views:chat_page/1))(Request);

route(_, _, _) ->
    {404, [{<<"Content-Type">>, <<"text/html; charset=utf-8">>}],
        <<"<h1>404 Not Found ¯\\_(ツ)_/¯</h1>"/utf8>>}.

%route(M, P, Request) ->
%    {200, [{<<"Content-Type">>, <<"text/plain">>}],
%        io_lib:format(<<"Hello from ~p~p~p~n">>, [M, P, Request])}.

