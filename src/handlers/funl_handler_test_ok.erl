-module(funl_handler_test_ok).

-include("../funl_request.hrl").

-export([init/2]).

init(Req, Opts) ->
  Req2 = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Request ok!">>, Req),
  {ok, Req2, Opts}.
