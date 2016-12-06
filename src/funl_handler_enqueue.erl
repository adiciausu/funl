-module(funl_handler_enqueue).
-include("../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    Request = #request{wrapped_request = Req, received_at = erlang:system_time()},
    funl_timed_queue:enq(Request, erlang:system_time()),
    
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.



    
    
    