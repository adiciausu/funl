-module(funl_handler_pending).

-include("../funl_request.hrl").

-export([init/2]).

init(Req, Opts) ->
  tinymq:push("pending", #request{wrappedRequest = Req}),
  Req2 = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/plain">>}
  ], <<"Request queued!">>, Req),
  {ok, Req2, Opts}.