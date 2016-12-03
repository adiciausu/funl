-module(funl_options_factory).
-include("funl_options.hrl").

%% API
-export([create_from_file/1, create_from_list/2]).

create_from_file(Filepath) ->
  Options = #options{},
  [ConfOptions] = yamerl_constr:file(Filepath),
  create_from_list(ConfOptions, Options).

create_from_list([{Key, Value} | Rest], Options) ->
  case Key of
    "max_errors_until_declare_dead" -> Options2 = Options#options{max_errors_until_declare_dead = Value};
    "max_redirects_until_declared_error" -> Options2 = Options#options{max_redirects_until_declared_error = Value};
    "endpoint" -> Options2 = Options#options{endpoint = Value};
    "route_strategy" -> Options2 = Options#options{route_strategy = Value};
    "delay_factor" -> Options2 = Options#options{delay_factor = Value}
  end,
  create_from_list(Rest, Options2);

create_from_list([], Options) ->
  Options.

