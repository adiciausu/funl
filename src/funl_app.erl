-module(funl_app).
-behaviour(application).
-include("../include/funl_options.hrl").

%% API.
-export([start/2, stop/1]).

%% API.
start(_Type, _Args) ->
    Options = funl_options_factory:create_from_file("conf.yml"),
    lager:info("Loaded config: ~p", [Options]),
    
    start_http_listener(Options),
    funl_sup:start_link(Options).

start_http_listener(#options{listen_on_port = Port}) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', funl_http_listener, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
            ]).
stop(_State) ->
    ok.


