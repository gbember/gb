%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gb_table_app).
-author("lihuachao").

-behaviour(application).
-include("gb_table.hrl").
%% Application callbacks
-export([start/2,
    stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
        StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    case gb_table_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
