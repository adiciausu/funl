-module(funl_app).
-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.

start(_Type, _Args) ->
  Options = funl_options_factory:create_from_file("/Users/adi/dev/erlang/funl/conf.yml"),
  io:format("Loaded config: ~p~n", [Options]),
  funl_queue_consumer:start("pending", Options),
  start_http_listener(),
  funl_sup:start_link().

start_http_listener() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {'_', funl_handler_pending, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]).

stop(_State) ->
  ok.


