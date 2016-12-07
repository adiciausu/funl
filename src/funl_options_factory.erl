-module(funl_options_factory).
-include("../include/funl_options.hrl").

%% API
-export([create_from_file/1, create_from_list/2]).

create_from_file(Filepath) ->
  Options = #options{},
  [ConfOptions] = yamerl_constr:file(Filepath),
  create_from_list(ConfOptions, Options).

create_from_list([{Key, Value} | Rest], Options) ->
  case Key of
    "max_errors" -> Options2 = Options#options{max_errors = Value};
    "max_redirects" -> Options2 = Options#options{max_redirects = Value};
    "endpoint" -> Options2 = Options#options{endpoint = Value};
    "route_strategy" -> Options2 = Options#options{route_strategy = list_to_atom(Value)};
    "delay_factor" -> Options2 = Options#options{delay_factor = Value};
    "backend_max_req_per_sec" -> Options2 = Options#options{backend_max_req_per_sec = Value}
  end,
  create_from_list(Rest, Options2);

create_from_list([], Options) ->
  Options.

