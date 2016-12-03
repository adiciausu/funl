%% @private
-module(funl_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  Queue = "pending",
  Endpoint = "http://stackoverflow.com/squestions/4103731/is-it-possible-to-use-record-name-as-a-parameter-in-erlang",
  funl_queue_consumer:start(Queue, Endpoint),
  start_http_listener().

start_http_listener() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {'_', funl_handler_pending, []}
    ]}
  ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
    {env, [{dispatch, Dispatch}]}
  ]),

  funl_sup:start_link().

stop(_State) ->
  ok.