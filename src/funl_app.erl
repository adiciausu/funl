-module(funl_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.
start(_Type, _Args) ->
    Options = funl_options_factory:create_from_file("conf.yml"),
    io:format("Loaded config: ~p~n", [Options]),
    
    start_http_listener(),
    funl_sup:start_link(Options).

start_http_listener() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', funl_http_listener, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
            ]).
stop(_State) ->
    ok.


