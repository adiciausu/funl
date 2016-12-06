-module(funl_uid).

%% API
-export([generate/1, timestamp/0]).

generate(Time) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    Rand = random:uniform(), %% to make sure it's unique
    Time * 1000000000 + Rand * 100000000. %% concatenate them, remove

timestamp() ->
    {Mega, Sec, Micro} = now(),
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.
