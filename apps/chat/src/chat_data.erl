-module(chat_data).

%% API
-export([init/0, migrate/1, reverse/1, session/3,
    get_chat/1, save_message/4, get_messages/2, get_messages/3, get_latest_key/1]).

-record(chat_session, {
    key :: {binary(), atom()},
    value :: any()
}).

-record(chat_chat, {
    key :: pos_integer(),
    id :: binary()
}).

-record(chat_message, {
    key :: {pos_integer(), pos_integer()},
    uid :: pos_integer(),
    text :: binary()
}).

-spec(session(binary(), atom(), any()) -> any()).

session(Session, Key, Default) ->
    mnesia:activity(transaction,
        fun() -> do_session(Session, Key, Default) end).

do_session(Session, Key, Default) ->
    K = {Session, Key},
    case mnesia:read(chat_session, K) of
        [] ->
            mnesia:write(#chat_session{key=K, value=Default}),
            Default;
        [#chat_session{value=V}] ->
            V
    end.


output_message({chat_message, {_, K}, U, T}) -> {K, U, T}.

get_chat(Id) ->
    mnesia:activity(transaction,
        fun() -> do_get_chat(Id) end).

do_get_chat(Id) ->
    case mnesia:index_read(chat_chat, Id, #chat_chat.id) of
        [] ->
            Key = erlang:system_time(),
            mnesia:write(#chat_chat{key=Key, id=Id}),
            Key;
        [#chat_chat{key=Key}] ->
            Key
    end.

do_save_message(ChatKey, Key, Uid, Text) ->
    Msg = #chat_message{key={ChatKey, Key}, uid=Uid, text=Text},
    mnesia:write(Msg),
    output_message(Msg).

save_message(ChatKey, Key, Uid, Text) ->
    mnesia:activity(transaction,
        fun() -> do_save_message(ChatKey, Key, Uid, Text) end).

do_get_messages(ChatKey, Guard) ->
    MatchHead = #chat_message{key={ChatKey, '$1'}, _='_'},
    Msgs = mnesia:select(chat_message, [{MatchHead, Guard, ['$_']}]),
    lists:map(fun output_message/1, Msgs).

get_messages(ChatKey, Since) ->
    Guard = [{'>', '$1', Since}],
    mnesia:activity(transaction,
        fun() -> do_get_messages(ChatKey, Guard) end).

get_messages(ChatKey, Since, Before) ->
    Guard = [{'>', '$1', Since}, {'<', '$1', Before}],
    mnesia:activity(transaction,
        fun() -> do_get_messages(ChatKey, Guard) end).

do_get_latest_key(ChatKey) ->
    %% Select the first message after
    MatchHead = #chat_message{key={'$1', '_'}, _='_'},
    Guard = {'>', '$1', ChatKey},
    Result = mnesia:select(chat_message, [{MatchHead, [Guard], ['$_']}], 1, read),
    Key = case Result of
        '$end_of_table' ->
            mnesia:last(chat_message);
        {[#chat_message{key=NextKey}], _} ->
            mnesia:prev(chat_message, NextKey)
    end,
    case Key of
        {ChatKey, MsgKey} -> MsgKey;
        _ -> 0
    end.

get_latest_key(ChatKey) ->
    mnesia:activity(transaction,
        fun() -> do_get_latest_key(ChatKey) end).

init() ->
    %% Don't check if it succeeded
    application:stop(mnesia),
    mnesia:create_schema([node()]),
    application:ensure_all_started(mnesia, permanent).

migrate(v02tov03) ->
    io:format("migrating~n"),
    {atomic, ok} = mnesia:create_table(chat_session, [
        {attributes, record_info(fields, chat_session)},
        {disc_copies, [node()]}]),
    ok = mnesia:wait_for_tables([
        chat_session],
        5000),
    ok;

migrate(v01tov02) ->
    io:format("migrating~n"),
    {atomic, ok} = mnesia:create_table(chat_chat, [
        {attributes, record_info(fields, chat_chat)},
        {index, [#chat_chat.id]},
        {disc_copies, [node()]}]),
    {atomic, ok} = mnesia:create_table(chat_message, [
        {attributes, record_info(fields, chat_message)},
        {disc_copies, [node()]},
        {type, ordered_set}]),
    ok = mnesia:wait_for_tables([
        chat_message, chat_chat],
        5000),
    ok.

reverse(v03tov02) ->
    io:format("migrating~n"),
    {atomic, ok} = mnesia:delete_table(chat_session),
    ok;

reverse(v02tov01) ->
    io:format("migrating~n"),
    {atomic, ok} = mnesia:delete_table(chat_chat),
    {atomic, ok} = mnesia:delete_table(chat_message),
    ok.
