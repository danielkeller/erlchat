-module(chat_chat).

-behaviour(gen_server).

%% API
-export([start_link/1, get/1, new_messages/2, old_messages/2, send_message/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(REG(Key), {n, l, {?MODULE, Key}}).
-define(SERVER(Key), {via, gproc, ?REG(Key)}).

-record(state, {id, seq, pending=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(_) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Key) ->
    gen_server:start_link(?SERVER(Key), ?MODULE, [Key], []).

-spec get(_) -> pid() | 'undefined'.
get(Key) ->
    gproc:where(?REG(Key)).

-spec(new_messages(pid(), non_neg_integer()) -> nonempty_list(_)).
new_messages(Pid, Since) ->
    gen_server:call(Pid, {new_messages, Since}, infinity).

-spec(old_messages(pid(), non_neg_integer()) -> list(_)).
old_messages(Pid, Before) ->
    gen_server:call(Pid, {old_messages, Before}, infinity).

send_message(Pid, Uid, Msg) ->
    gen_server:cast(Pid, {send_message, Uid, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Key]) ->
    Id = chat_data:get_chat(Key),
    Seq = chat_data:get_latest_key(Id) + 1,
    {ok, #state{id=Id, seq=Seq}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call({new_messages, Since}, From,
        State = #state{pending = Pending, seq = Next}) ->
    Start = max(Since, Next - 6),
    Existing = chat_data:get_messages(State#state.id, Start),
    case Existing of
        [] -> {noreply, State#state{pending = [From | Pending]}};
        _ -> {reply, Existing, State}
    end;

handle_call({old_messages, Before}, _From, State) ->
    Since = Before - 6,
    Msgs = chat_data:get_messages(State#state.id, Since, Before),
    {reply, Msgs, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({send_message, Uid, Msg}, State) ->
    Key = State#state.seq,
    NewMsg = chat_data:save_message(State#state.id, Key, Uid, Msg),
    [gen_server:reply(From, [NewMsg]) || From <- State#state.pending],
    {noreply, State#state{pending=[], seq=Key+1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

