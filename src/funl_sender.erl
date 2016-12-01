-module(funl_handler).

-export([init/2]).

init(Req, Opts) ->
  tinymq:push("pending_requests", Req),
  Req2 = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Hello world!">>, Req),
  {ok, Req2, Opts}.