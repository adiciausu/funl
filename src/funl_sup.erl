%% @private
-module(funl_sup).
-behaviour(supervisor).
-export([start_link/0]).

%% supervisor callbacks.
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.
init([]) ->
    Procs = [{funl_queue_consumer, {funl_queue_consumer, start_link, []},
        permanent, 5000, worker, [funl_queue_consumer]}],
    
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Procs}}.