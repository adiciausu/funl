-module(funl_handler).

-include("request.hrl").

-export([init/2]).

init(Req, Opts) ->
  tinymq:push("pending_requests", #funl_request{errCount=0, request=Req}),
  Req2 = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Hello world!">>, Req),
  {ok, Req2, Opts}.