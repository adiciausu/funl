-module(funl_handler_test_fail_redirect).

-include("../funl_request.hrl").

-export([init/2]).

init(Req, Opts) ->
  Req2 = cowboy_req:reply(301, [
    {<<"Location">>, <<"http://127.0.0.1:8081/redirect">>}
  ], <<"Redirecting to...">>, Req),
  {ok, Req2, Opts}.
