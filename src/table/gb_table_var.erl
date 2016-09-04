%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gb_table_var).
-author("lihuachao").

%% API
-export([init/0, get/1, set/2]).
-define(TABLE_VAR, gb_table_var).
%%%-------------------------------------------------------------------
%%% @doc
%%%     初始化
%%% @end
%%%-------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    ets:new(?TABLE_VAR, [set, public, named_table, {read_concurrency, true}]),
    ok.

-spec get(Key) -> Value when
    Key :: term(),
    Value :: term().
get(Key) ->
    case ets:lookup(?TABLE_VAR, Key) of
        [] ->
            'none';
        [{_, V}] ->
            V
    end.

-spec set(Key, Value) -> 'ok' when
    Key :: term(),
    Value :: term().
set(Key, Value) ->
    ets:insert(?TABLE_VAR, {Key, Value}),
    ok.
