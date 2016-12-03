%% @private
-module(funl_app).
-include("funl_options.hrl").
-behaviour(application).

%% API.
-export([start/2, stop/1]).

%% API.

start(_Type, _Args) ->
  Options = parse_config_file("/Users/adi/dev/erlang/conf.yml"),
  io:format("Loaded config: ~p~n", [Options]),
  funl_queue_consumer:start("pending", Options),
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

parse_config_file(Filepath) ->
  Options = #options{},
  [ConfOptions] = yamerl_constr:file(Filepath),
  parse_config_options(ConfOptions, Options).

parse_config_options([{Key, Value} | Rest], Options) ->
  case Key of
    "max_errors_until_declare_dead" -> Options2 = Options#options{max_errors_until_declare_dead = Value};
    "max_redirects_until_declared_error" -> Options2 = Options#options{max_redirects_until_declared_error = Value};
    "endpoint" -> Options2 = Options#options{endpoint = Value};
    "route_strategy" -> Options2 = Options#options{route_strategy = Value}
  end,
  parse_config_options(Rest, Options2);

parse_config_options([], Options) ->
  Options.



