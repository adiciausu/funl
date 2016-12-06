-module(funl_uid).

%% API
-export([generate/1, generate/0]).

generate() ->
    Time = erlang:system_time(), %%to make sure requests are processed in oreder ;)
    generate(Time).

generate(Time) ->
    Rand = crypto:rand_uniform(1000000000, 9999999999), %% to make sure it's unique
    Time * 1000000000 + Rand. %% concatenate them, remove