-module(funl_handler_test_fail).
-include("../funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
  Req2 = cowboy_req:reply(404, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Some error!">>, Req),
  {ok, Req2, Opts}.