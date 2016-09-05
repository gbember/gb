%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gb_app).
-author("lihuachao").

-behaviour(application).
%% Application callbacks
-export([start/2,
    stop/1]).
-export([start/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%     启动application 用于快捷调用
%%% @end
%%%-------------------------------------------------------------------
-spec start() -> 'ok' | {'error', term()}.
start() ->
    lager:start(),
    application:start(gb).

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
    case gb_sup:start_link() of
        {ok, Pid} ->
            lager:info("gb application started~n"),
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
