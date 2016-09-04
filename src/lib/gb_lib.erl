%%%-------------------------------------------------------------------
%%% @doc
%%%     提供一些通用的公用方法
%%% @end
%%%-------------------------------------------------------------------
-module(gb_lib).
-author("lihuachao").

-export([get_value/3]).

%%%-------------------------------------------------------------------
%%% @doc
%%%     从KV列表中查找Key的值  没有找到返回默认值
%%% @end
%%%-------------------------------------------------------------------
-spec get_value(KVList,Key,Default) -> term() when
    KVList :: [{term(),term()}],
    Key :: term(),
    Default :: term().
get_value(KVList,Key,Default)->
    case lists:keyfind(Key,1,KVList) of
        false->
            Default;
        {_,V}->
            V
    end.