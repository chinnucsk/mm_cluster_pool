
-module(mm_cluster_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case config:get(master_node) of
    {ok, true}->
        log:info("Init pool master node..."),
        {ok, ClusterName} = config:get(cluster_pool_name),
        pool:start(ClusterName),
        Mods = [?CHILD(cluster_srv_sock, worker)],
        ok;
    _Else->
        Mods = [?CHILD(cluster_client_sock, worker)],
        io:format("Starting slave node...~n")
    end,
    {ok, { {one_for_one, 5, 10}, Mods} }.

