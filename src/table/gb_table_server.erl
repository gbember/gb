%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gb_table_server).
-author("lihuachao").

-behaviour(gen_server).

-include("gb_table.hrl").
%% API
-export([start/2, start_link/2]).
-export([lock/4, unlock/3]).
-export([get/2, gets/2, set/3, sets/2, delete/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-define(SERVER, ?MODULE).

-record(state, {
    table_name :: atom(),%%表名
    table_mod :: atom(),
    table_data :: tuple(),
    table_lock :: gb_table_lock:table_lock()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%      启动一个table server
%% @end
%%--------------------------------------------------------------------
-spec start(TableName, Options) -> {ok, Pid} | {error, Reason} when
    TableName :: atom(),
    Options :: [gb_table_lib:table_option()],
    Pid :: pid(),
    Reason :: term().
start(TableName, Options) ->
    supervisor:start_child(gb_table_sup, [TableName, Options]).

%%--------------------------------------------------------------------
%% @doc
%%      启动一个table server
%% @end
%%--------------------------------------------------------------------
-spec start_link(TableName, Options) -> {ok, Pid} | {error, Reason} when
    TableName :: atom(),
    Options :: [gb_table_lib:table_option()],
    Pid :: pid(),
    Reason :: term().
start_link(TableName, Options) ->
    NOptions = gb_table_lib:parse_options(Options),
    gen_server:start_link({local, TableName}, ?MODULE, [TableName, NOptions], []).

%%%-------------------------------------------------------------------
%%% @doc
%%%     锁
%%% @end
%%%-------------------------------------------------------------------
-spec lock(TableName, Key, Ref, Ms) -> no_return() | 'ok' when
    TableName :: atom(),
    Key :: term(),
    Ref :: term(),
    Ms :: integer().
lock(TableName, Key, Ref, Ms) ->
    gen_server:call(TableName, {'lock', Key, Ref, Ms}, ?TABLE_KEY_LOCK_TIME).

%%%-------------------------------------------------------------------
%%% @doc
%%%     解锁
%%% @end
%%%-------------------------------------------------------------------
-spec unlock(TableName, Key, Ref) -> no_return() | 'ok' when
    TableName :: atom(),
    Key :: term(),
    Ref :: term().
unlock(TableName, Key, Ref) ->
    erlang:send(TableName, {'unlock', Key, Ref}).

%%%-------------------------------------------------------------------
%%% @doc
%%%        获取单个Key的值
%%% @end
%%%-------------------------------------------------------------------
-spec get(TableName, Key) -> Value when
    TableName :: atom(),
    Key :: term(),
    Value :: term().
get(TableName, Key) ->
    {TableMod, TableData} = gb_table_var:get(TableName),
    TableMod:get(TableData, Key).

%%%-------------------------------------------------------------------
%%% @doc
%%%        获取多个Key的值(返回的值列表顺序和Key的顺序一致)
%%% @end
%%%-------------------------------------------------------------------
-spec gets(TableName, Keys) -> Values when
    TableName :: atom(),
    Keys :: [term()],
    Values :: [term()].
gets(TableName, Keys) ->
    {TableMod, TableData} = gb_table_var:get(TableName),
    TableMod:gets(TableData, Keys).

%%%-------------------------------------------------------------------
%%% @doc
%%%     设置单个值
%%% @end
%%%-------------------------------------------------------------------
-spec set(TableName, Key, Value) -> 'ok' when
    TableName :: atom(),
    Key :: term(),
    Value :: term().
set(TableName, Key, Value) ->
    {TableMod, TableData} = gb_table_var:get(TableName),
    TableMod:set(TableData, Key, Value).

%%%-------------------------------------------------------------------
%%% @doc
%%%     设置单个值
%%% @end
%%%-------------------------------------------------------------------
-spec sets(TableName, KVList) -> 'ok' when
    TableName :: atom(),
    KVList :: [{term(), term()}].
sets(TableName, KVList) ->
    {TableMod, TableData} = gb_table_var:get(TableName),
    TableMod:set(TableData, KVList).

%%%-------------------------------------------------------------------
%%% @doc
%%%     删除单个值
%%% @end
%%%-------------------------------------------------------------------
-spec delete(TableName, Key) -> 'ok' when
    TableName :: atom(),
    Key :: term().
delete(TableName, Key) ->
    {TableMod, TableData} = gb_table_var:get(TableName),
    TableMod:delete(TableData, Key).

%%%-------------------------------------------------------------------
%%% @doc
%%%     锁
%%% @end
%%%-------------------------------------------------------------------
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()}.
init([TableName, Options]) ->
    erlang:process_flag('trap_exit', true),
    {_, Mod} = lists:keyfind(?TABLE_MOD, 1, Options),
    TableData = Mod:init(Options),
    TableLock = gb_table_lock:init(),
    gb_table_var:set(TableName, {Mod, TableData}),
    {ok, #state{table_name = TableName, table_mod = Mod, table_data = TableData, table_lock = TableLock}}.

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
handle_call({'lock', Key, Ref, Ms}, From, #state{table_lock = TableLock} = State) ->
    R = gb_table_lock:lock(TableLock, Key, Ref, From, Ms),
    if
        R =:= 'lock' ->
            {reply, ok, State};
        true ->
            {noreply, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_cast(_Request, State) ->
    {noreply, State}.

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
handle_info({'unlock', Key, Ref}, #state{table_lock = TableLock} = State) ->
    case gb_table_lock:unlock(TableLock, Key, Ref) of
        {ok, From} ->
            gen_server:reply(From, 'ok');
        _ ->
            ok
    end,
    {noreply, State};
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
