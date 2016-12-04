-module(funl_request_queue).
-include("../../include/funl_request.hrl").


%% API
-export([]).

init(Name) ->
    mnesia:create_table(Name,
        [{attributes, record_info(fields, request)}]).

enq(#request{} = Req) ->
    T = fun() ->
        mnesia:write(Req)
        end,
    mnesia:transaction(T),
    ok.

peek(End) ->
    ok.

deq(ReqId) ->
    T = fun() ->
        mnesia:delete_object(ReqId)
        end,
    mnesia:transaction(T),
    ok.
