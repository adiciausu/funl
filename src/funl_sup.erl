%% @private
-module(funl_sup).
-behaviour(supervisor).
-export([start_link/1]).

%% supervisor callbacks.
-export([init/1]).

start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).

%% supervisor.
init([Options]) ->
    Procs = [
        {funl_queue, {funl_queue, start_link, []},
            permanent, 5000, worker, [funl_queue]},
        {funl_consumer, {funl_consumer, start_link, [Options, request]},
            permanent, 5000, worker, [funl_consumer]}
    ],
    
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Procs}}.