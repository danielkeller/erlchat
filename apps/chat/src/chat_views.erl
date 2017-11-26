%%%-------------------------------------------------------------------
%%% @author dan
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Nov 2017 11:13 AM
%%%-------------------------------------------------------------------
-module(chat_views).

%% API
-export([chat_page/1, messages/3, send_message/2, old_messages/3]).

string_sanitize(Bytes) ->
    Unquotes = re:replace(Bytes, <<"\"">>, <<"\\\\\"">>, [global]),
    re:replace(Unquotes, <<"}">>, <<"\\\\u007D">>, [global]).

message_to_json({Id, Uid, Msg}) -> [
    <<"{\"i\":">>,
    io_lib:format("~B", [Id]),
    <<",\"u\":">>,
    io_lib:format("~B", [Uid]),
    <<",\"m\":\"">>,
    string_sanitize(Msg),
    <<"\"}">>].

messages_to_json_arr([Msg | Msgs]) ->
    Comma = fun (M) -> [<<",">>, message_to_json(M)] end,
    [
        <<"[">>,
        message_to_json(Msg),
        lists:map(Comma, Msgs),
        <<"]">>
    ];
messages_to_json_arr([]) ->
    <<"[]">>.

next_message(Chat, Since) ->
    Msgs = chat_chat:new_messages(Chat, Since),
    {Latest, _, _} = lists:last(Msgs),
    Formatted = lists:map(fun message_to_json/1, Msgs),
    {more, Formatted, fun() -> next_message(Chat, Latest) end}.

get_uid(Request) ->
    chat_session:get_session(Request, uid, rand:uniform(1 bsl 32)).

chat_page(_) ->
    {200, [{<<"Content-Type">>, <<"text/html">>}],
        chat_util:static_file("chat.html")}.

-spec(messages(integer(), binary(), chat_http:request()) -> chat_http:response()).
messages(Since, Name, _) ->
    Chat = chat_chat_sup:get_or_create_chat(Name),
    Next = fun() -> next_message(Chat, Since) end,
    {200, [{<<"Content-Type">>, <<"application/json">>}], Next}.

send_message(Name, Request) ->
    Chat = chat_chat_sup:get_or_create_chat(Name),
    Uid = get_uid(Request),
    chat_chat:send_message(Chat, Uid, chat_http:body(Request)),
    {200, [], ""}.

-spec(old_messages(integer(), binary(), chat_http:request()) -> chat_http:response()).
old_messages(Before, Name, _) ->
    Chat = chat_chat_sup:get_or_create_chat(Name),
    Msgs = chat_chat:old_messages(Chat, Before),
    Formatted = messages_to_json_arr(Msgs),
    {200, [{<<"Content-Type">>, <<"application/json">>}], Formatted}.
