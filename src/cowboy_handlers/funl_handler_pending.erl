-module(funl_handler_pending).
-include("../../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    Uid = uuid:to_string(uuid:v4()),
    funl_queue:enq(#request{wrapped_request = Req, id = Uid}),

    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.