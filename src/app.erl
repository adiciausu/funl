%% @private
-module(app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", toppage_handler, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
    env => #{dispatch => Dispatch}
  }),
  sup:start_link().

stop(_State) ->
  ok.