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
        {funl_pending, {funl_queue, start_link, [pending]},
            permanent, 5000, worker, [funl_queue]},
%%        {funl_queue_dead, {funl_queue, start_link, [dead]},
%%            permanent, 5000, worker, [funl_queue]},
        {funl_consumer_pending, {funl_consumer, start_link, [Options, "pending"]},
            permanent, 5000, worker, [funl_consumer, funl_queue_consumer_pending]},
        {funl_consumer_dead, {funl_consumer, start_link, [Options, "dead"]},
            permanent, 5000, worker, [funl_consumer, funl_queue_consumer_dead]}
    ],
    
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Procs}}.