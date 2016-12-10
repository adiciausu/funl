-module(funl_http_listener).
-include("../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    funl_queue:enq(wrap_request(Req), funl_uid:timestamp()),
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.

wrap_request(Req) ->
    Request = #request{wrapped_request = Req, received_at = funl_uid:timestamp()},
    Ttl = cowboy_req:header(<<"x-funl-ttl">>, Req),
    case is_binary(Ttl) of
        true ->
            case re:run(Ttl, "^[0-9]*$") of
                nomatch -> Request;
                {match, _} -> Request#request{ttl = binary_to_integer(Ttl)}
            end;
        _ -> Request
    end.

    
      


    
    
    