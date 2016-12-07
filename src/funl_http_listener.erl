-module(funl_http_listener).
-include("../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    Request = #request{wrapped_request = Req, received_at = funl_uid:timestamp()},
    Ttl = cowboy_req:header(<<"x-funl-ttl">>, Req),
    case is_binary(Ttl) of
        true -> funl_timed_queue:enq(Request#request{ttl = binary_to_integer(Ttl)}, funl_uid:timestamp());
        _ -> funl_timed_queue:enq(Request, funl_uid:timestamp())
    end,
    
    
    
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.



    
    
    