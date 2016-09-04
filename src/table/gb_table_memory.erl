%%%-------------------------------------------------------------------
%%% @doc
%%%     内存缓存模块
%%% @end
%%% Created : 04. 九月 2016 18:26
%%%-------------------------------------------------------------------
-module(gb_table_memory).
-author("lihuachao").

-include("gb_table.hrl").
%% API
-export([init/1]).
-export([get/2, gets/2, set/3, sets/2, delete/2]).


-record(table_memory, {
    records :: ets:tab()%%缓存数据
}).

-export_type([table_memory/0]).

-type table_memory() :: #table_memory{}.


%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-spec init(Opts) -> TableMemory when
    Opts :: [gb_table_lib:table_option()],
    TableMemory :: table_memory().
init(Opts) when is_list(Opts) ->
    {_, KSortType} = lists:keyfind(?TABLE_KEY_SORT, 1, Opts),
    Records = ets:new('table_memory_records', [KSortType, public, {read_concurrency, true}]),
    #table_memory{records = Records}.

%%%-------------------------------------------------------------------
%%% @doc
%%%        从缓存中获取单个Key的值
%%% @end
%%%-------------------------------------------------------------------
-spec get(TableMemory, Key) -> Value when
    TableMemory :: table_memory(),
    Key :: term(),
    Value :: term().
get(#table_memory{records = Records}, Key) ->
    case ets:lookup(Records, Key) of
        [] ->
            'none';
        [{_, Value}] ->
            Value
    end.

%%%-------------------------------------------------------------------
%%% @doc
%%%        从缓存中获取多个Key的值(返回的值列表顺序和Key的顺序一致)
%%% @end
%%%-------------------------------------------------------------------
-spec gets(TableMemory, Keys) -> Values when
    TableMemory :: table_memory(),
    Keys :: [term()],
    Values :: [term()].
gets(#table_memory{records = Records}, Keys) ->
    Fun = fun(Key) ->
        case ets:lookup(Records, Key) of
            [] ->
                'none';
            [{_, Value}] ->
                Value
        end
    end,
    lists:map(Fun, Keys).

%%%-------------------------------------------------------------------
%%% @doc
%%%     设置单个值
%%% @end
%%%-------------------------------------------------------------------
-spec set(TableMemory, Key, Value) -> 'ok' when
    TableMemory :: table_memory(),
    Key :: term(),
    Value :: term().
set(#table_memory{records = Records}, Key, Value) ->
    ets:insert(Records, {Key, Value}),
    'ok'.


%%%-------------------------------------------------------------------
%%% @doc
%%%     设置多个值
%%% @end
%%%-------------------------------------------------------------------
-spec sets(TableMemory, KVList) -> 'ok' when
    TableMemory :: table_memory(),
    KVList :: [{term(), term()}].
sets(#table_memory{records = Records}, KVList) ->
    ets:insert(Records, KVList),
    'ok'.

%%%-------------------------------------------------------------------
%%% @doc
%%%     删除单个值
%%% @end
%%%-------------------------------------------------------------------
-spec delete(TableMemory, Key) -> 'ok' when
    TableMemory :: table_memory(),
    Key :: term().
delete(#table_memory{records = Records}, Key) ->
    ets:delete(Records, Key),
    ok.



