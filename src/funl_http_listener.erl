-module(funl_http_listener).
-include("../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    Request = #request{wrapped_request = Req, received_at = funl_uid:timestamp()},
    funl_timed_queue:enq(Request, funl_uid:timestamp()),
    
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.



    
    
    