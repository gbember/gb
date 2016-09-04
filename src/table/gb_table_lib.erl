%%%-------------------------------------------------------------------
%%% @doc
%%%     table的一些公共处理方法
%%% @end
%%%-------------------------------------------------------------------
-module(gb_table_lib).
-author("lihuachao").
-include("gb_table.hrl").
%% API
-export([parse_options/1]).

-export_type([table_option/0]).

-type table_option() ::
{?TABLE_MAX_MEMORY, integer()} |%%缓存最大字节数 0表示没有限制
{?TABLE_MAX_NUM, integer()} |%%缓存最大条数 0表示没有限制
{?TABLE_CHECK_TIMEOUT, integer()}| %%超时检查间隔 单位毫秒
{?TABLE_KEY_SORT, 0 | 1} |%%索引表是否排序 0:不排序  1:由小向大排序
{?TABLE_MOD, atom()}.%%数据处理模块

%%%-------------------------------------------------------------------
%%% @doc
%%%     默认值填充
%%% @end
%%%-------------------------------------------------------------------
-spec parse_options(Opts :: [table_option()]) -> [table_option()].
parse_options(Opts) when is_list(Opts) ->
    lists:map(fun({?TABLE_KEY_SORT, V}) ->
        if
            V =:= 0 ->
                {?TABLE_KEY_SORT, 'set'};
            true ->
                {?TABLE_KEY_SORT, 'ordered_set'}
        end;
        ({Key, Value}) ->
            {Key, gb_lib:get_value(Opts, Key, Value)}
    end, ?TABLE_DEFAULT_OPS).
