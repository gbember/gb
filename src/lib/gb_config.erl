%%%-------------------------------------------------------------------
%%% @doc
%%%     配置文件读取
%%% @end
%%% Created : 05. 九月 2016 22:18
%%%-------------------------------------------------------------------
-module(gb_config).
-author("lihuachao").

%% API
-export([parse_config_dir/1]).

%%%-------------------------------------------------------------------
%%% @doc
%%%     解析配置文件夹
%%% @end
%%%-------------------------------------------------------------------
-spec parse_config_dir(string()) -> 'ok'.
parse_config_dir(Dir) ->
    {ok, Filenames} = file:list_dir(Dir),
    lists:foreach(fun(Filename) ->
        F = filename:join(Dir, Filename),
        case filelib:is_dir(F) of
            true ->
                parse_config_dir(F);
            false ->
                case filename:extension(Filename) =:= ".cfg" of
                    true ->
                        parse_config_file(F);
                    false ->
                        ok
                end
        end
    end, Filenames),
    'ok'.

%%%-------------------------------------------------------------------
%%% @doc
%%%     解析配置文件
%%% @end
%%%-------------------------------------------------------------------
-spec parse_config_file(string()) -> 'ok'.
parse_config_file(FileName) ->
    {CTableName, Opst, List} = file:consult(FileName),
    ets:new(CTableName, [public, named_table | Opst]),
    case ets:insert_new(CTableName, List) of
        true ->
            ok;
        false ->
            lager:error("config key duplicate: ~p~n", [CTableName])
    end,
    'ok'.
