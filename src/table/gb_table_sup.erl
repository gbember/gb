%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gb_table_sup).
-author("lihuachao").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]
    }} |
    ignore |
    {error, Reason :: term()}).
init([]) ->
    SupFlags = {'simple_one_for_one', 4, 3600},
    AChild = {gb_table_server, {gb_table_server, start_link, []},
        permanent, 10000, worker, [gb_table_server]},
    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
