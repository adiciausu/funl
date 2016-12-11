-module(funl_http_listener).
-include("../include/funl_request.hrl").
-export([init/2]).

init(Req, Opts) ->
    funl_queue:enq(funl_request_factory:create_from_cowboy_request(Req), funl_uid:timestamp()),
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Request queued!">>, Req),
    {ok, Req2, Opts}.


    
      


    
    
    