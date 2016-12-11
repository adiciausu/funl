-module(funl_request_factory).
-include("../include/funl_request.hrl").

%% API
-export([create_from_cowboy_request/1]).

create_from_cowboy_request(CowboyReq) ->
    Method = list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(CowboyReq)))),
    {ok, Body, _} = cowboy_req:body(CowboyReq),
    Path = binary_to_list(cowboy_req:path(CowboyReq)),
    Qs = binary_to_list(cowboy_req:qs(CowboyReq)),
    #request{received_at = funl_uid:timestamp(), headers = headers(CowboyReq), method = Method, body = Body,
       relative_url =  string:concat(Path, Qs), ttl = getTtl(CowboyReq)}.

headers(CowboyReq) ->
    Headers = cowboy_req:headers(CowboyReq),
    headers(Headers, []).
headers([], Acc) -> Acc;
headers([{Key, Value} = Header | Rest], Acc) ->
    case Key of
        <<"host">> -> headers(Rest, lists:append(Acc, [{<<"X-Forwarded-For">>, Value}]));
        _ -> headers(Rest, lists:append(Acc, [Header]))
    end.

getTtl(CowboyReq) ->
    Ttl = cowboy_req:header(<<"x-funl-ttl">>, CowboyReq),
    case is_binary(Ttl) of
        true ->
            case re:run(Ttl, "^[0-9]*$") of
                nomatch -> unset;
                {match, _} -> binary_to_integer(Ttl)
            end;
        _ -> unset
    end.

