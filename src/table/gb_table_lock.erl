%%%-------------------------------------------------------------------
%%% @doc
%%%     数据库表键锁模块
%%% @end
%%% Created : 04. 九月 2016 13:17
%%%-------------------------------------------------------------------
-module(gb_table_lock).
-author("lihuacaho").

-include("gb_table.hrl").
-record(table_lock, {
    locks :: ets:tab(),%%当前锁
    waits :: ets:tab(), %%当前等待锁
    timeouts :: ets:tab()
}).

%% API
-export([
    init/0,
    lock/5,
    unlock/3,
    timeout/1
]).
-export_type([table_lock/0]).

-type table_lock() :: #table_lock{}.

%%%-------------------------------------------------------------------
%%% @doc
%%%     数据库表键锁模块数据结构初始化
%%% @end
%%%-------------------------------------------------------------------
-spec init() -> table_lock().
init() ->
    Locks = ets:new('table_lock_locks', []),
    Waits = ets:new('table_lock_waits', []),
    Timeouts = ets:new('table_lock_timeouts', [ordered_set]),
    #table_lock{locks = Locks, waits = Waits, timeouts = Timeouts}.

%%%-------------------------------------------------------------------
%%% @doc
%%%     表锁键操作
%%%     返回 lock:锁成功 wait:需要等待
%%% @end
%%%-------------------------------------------------------------------
-spec lock(TableLock, Key, Ref, From, Ms) -> 'lock' | 'wait' when
    TableLock :: table_lock(),
    Key :: term(),
    Ref :: term(),
    From :: {pid(), term()},
    Ms :: integer().
lock(#table_lock{locks = Locks, waits = Waits, timeouts = Timeouts}, Key, Ref, From, Ms) ->
    TimeoutMs = Ms + ?TABLE_KEY_LOCK_TIMEOUT,
    case ets:lookup(Locks, Key) of
        [] ->
            insert_lock(Locks, Timeouts, Key, Ref, From, TimeoutMs),
            'lock';
        _ ->
            insert_wait(Waits, Key, Ref, From, TimeoutMs),
            'wait'
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     表锁键解锁操作
%%%     返回 ok:解锁成功 {ok, {pid(), term()}}:新锁等待call进程
%%% @end
%%%-------------------------------------------------------------------
-spec unlock(TableLock, Key, Ref) -> 'ok' | {'ok', From} when
    TableLock :: table_lock(),
    Key :: term(),
    Ref :: term(),
    From :: {pid(), term()}.%%新锁等待call进程
unlock(#table_lock{locks = Locks, waits = Waits, timeouts = Timeouts}, Key, Ref) ->
    case ets:lookup(Locks, Key) of
        [{_, Ref, _From, TimeoutMs}] ->
            unlock_(Locks, Waits, Timeouts, Key, Ref, TimeoutMs);
        _ ->
            'ok'
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     锁超时检查
%%% @end
%%%-------------------------------------------------------------------
-spec timeout(TableLock) -> {TimeoutFromList, LockFromList} when
    TableLock :: table_lock(),
    TimeoutFromList :: [{pid(), term()}],%%超时的
    LockFromList :: [{pid(), term()}].%%新锁住的
timeout(#table_lock{locks = Locks, waits = Waits, timeouts = Timeouts}) ->
    First = ets:first(Timeouts),
    timeout_(Locks, Waits, Timeouts, gb_time_lib:now_mili(), [], [], First).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%     插入键锁数据
%%% @end
%%%-------------------------------------------------------------------
-spec insert_lock(Locks, Timeouts, Key, Ref, From, TimeoutMs) -> 'ok' when
    Locks :: ets:tab(),
    Timeouts :: ets:tab(),
    Key :: term(),
    Ref :: term(),
    From :: {pid(), term()},
    TimeoutMs :: integer().
insert_lock(Locks, Timeouts, Key, Ref, From, TimeoutMs) ->
    ets:insert(Locks, {Key, Ref, From, TimeoutMs}),
    ets:insert(Timeouts, {{TimeoutMs, Key}, 0}),
    'ok'.

%%%-------------------------------------------------------------------
%%% @doc
%%%     插入等待键锁数据
%%% @end
%%%-------------------------------------------------------------------
-spec insert_wait(Waits, Key, Ref, From, TimeoutMs) -> 'ok' when
    Waits :: ets:tab(),
    Key :: term(),
    Ref :: term(),
    From :: {pid(), term()},
    TimeoutMs :: integer().
insert_wait(Waits, Key, Ref, From, TimeoutMs) ->
    Queue =
        case ets:lookup(Waits, Key) of
            [] ->
                queue:new();
            [{_, Q}] ->
                Q
        end,
    ets:insert(Waits, {Key, queue:in({Ref, From, TimeoutMs}, Queue)}),
    'ok'.

%%%-------------------------------------------------------------------
%%% @doc
%%%     表锁键解锁操作
%%%     返回 ok:解锁成功 {ok, {pid(), term()}}:新锁等待call进程
%%% @end
%%%-------------------------------------------------------------------
-spec unlock_(Locks, Waits, Timeouts, Key, Ref, TimeoutMs) -> 'ok' | {'ok', From} when
    Locks :: ets:tab(),
    Waits :: ets:tab(),
    Timeouts :: ets:tab(),
    Key :: term(),
    Ref :: term(),
    TimeoutMs :: integer(),
    From :: {pid(), term()}.
unlock_(Locks, Waits, Timeouts, Key, Ref, TimeoutMs) ->
    ets:delete(Timeouts, {TimeoutMs, Key}),
    case ets:lookup(Waits, Key) of
        [] ->
            ets:delete(Locks, Key),
            'ok';
        [{_, Queue}] ->
            {{value, {Ref, F, NTimeoutMs}}, NQueue} = queue:out(Queue),
            ets:insert(Waits, {Key, NQueue}),
            insert_lock(Locks, Timeouts, Key, Ref, F, NTimeoutMs),
            {'lock', F}
    end.


%%%-------------------------------------------------------------------
%%% @doc
%%%     锁超时检查
%%% @end
%%%-------------------------------------------------------------------
-spec timeout_(Locks, Waits, Timeouts, NowMs, TimeoutFromList, LockFromList, Key) -> {TimeoutFromList, LockFromList} when
    Locks :: ets:tab(),
    Waits :: ets:tab(),
    Timeouts :: ets:tab(),
    NowMs :: integer(),
    Key :: {integer(), term()} | '$end_of_table',
    TimeoutFromList :: [{pid(), term()}],%%超时的
    LockFromList :: [{pid(), term()}].%%新锁住的
timeout_(_Locks, _Waits, _Timeouts, _NowMs, TAcc, LAcc, '$end_of_table') ->
    {TAcc, LAcc};
timeout_(Locks, Waits, Timeouts, NowMs, TAcc, LAcc, {TimeoutMs, Key} = K) ->
    if
        NowMs >= TimeoutMs ->
            ets:delete(Timeouts, K),
            [{_, _, From, _}] = ets:lookup(Locks, Key),
            ets:delete(Locks, Key),
            NTAcc1 = [From | TAcc],
            case ets:lookup(Waits, Key) of
                [] ->
                    timeout_(Locks, Waits, Timeouts, NowMs, NTAcc1, LAcc, ets:next(Timeouts, K));
                [{_, Queue}] ->
                    {NTAcc, NLAcc} = qtimeout_(Locks, Waits, Timeouts, NowMs, Key, NTAcc1, LAcc, Queue),
                    timeout_(Locks, Waits, Timeouts, NowMs, NTAcc, NLAcc, ets:next(Timeouts, K))
            end;
        true ->
            {TAcc, LAcc}
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%     等待锁转换成锁
%%% @end
%%%-------------------------------------------------------------------
-spec qtimeout_(Locks, Waits, Timeouts, NowMs, Key, TAcc, LAcc, Queue) -> {TAcc, LAcc} when
    Locks :: ets:tab(),
    Waits :: ets:tab(),
    Timeouts :: ets:tab(),
    NowMs :: integer(),
    Key :: {integer(), term()},
    TAcc :: [{pid(), term()}],%%超时的
    LAcc :: [{pid(), term()}],%%新锁住的
    Queue :: queue:queue({term(),{pid(),term()},integer()}).
qtimeout_(Locks, Waits, Timeouts, NowMs, Key, TAcc, LAcc, Queue) ->
    case queue:is_empty(Queue) of
        true ->
            ets:delete(Timeouts, Key),
            {TAcc, LAcc};
        false ->
            {{'value', {Ref, From, TimeoutMs}}, NQueue} = queue:out(Queue),
            if
                NowMs >= TimeoutMs ->
                    case queue:is_empty(NQueue) of
                        true ->
                            ets:delete(Timeouts, Key);
                        false ->
                            ok
                    end,
                    qtimeout_(Locks, Waits, Timeouts, NowMs, Key, [From | TAcc], LAcc, NQueue);
                true ->
                    insert_lock(Locks, Timeouts, Key, Ref, From, TimeoutMs),
                    ets:insert(Waits, {Key, NQueue}),
                    {TAcc, [From | LAcc]}
            end
    end.