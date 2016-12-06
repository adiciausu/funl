-module(funl_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.

start(_Type, _Args) ->
    spawn(fun start_test_backend/0),
    Options = funl_options_factory:create_from_file("/Users/adi/dev/erlang/funl/conf.yml"),
    io:format("Loaded config: ~p~n", [Options]),
    
    start_http_listener(),
    funl_sup:start_link(Options).

start_http_listener() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', funl_http_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]}
    ]).

start_test_backend() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/fail", funl_handler_test_fail, []},
            {"/ok", funl_handler_test_ok, []},
            {"/redirect", funl_handler_test_fail_redirect, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(httptest, 100, [{port, 8081}], [
        {env, [{dispatch, Dispatch}]}
    ]).

stop(_State) ->
    ok.


