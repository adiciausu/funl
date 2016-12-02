-module(funl_retry_client).
-include("funl_request.hrl").
-export([send/1]).
-define(should_stop(FunlReq), element(2, FunlReq) == 10).

send(FunlRequest) ->
  Timer = erlang:send_after(1, self(), FunlRequest),
  send(FunlRequest, Timer).

send(FunlRequest, OldTimer) ->
  erlang:cancel_timer(OldTimer),
  Host = "http://stackoverflow.com/squestions/4103731/is-it-possible-to-use-record-name-as-a-parameter-in-erlang",
  Response = ibrowse:send_req(Host, [], get),
  ProcessingInfo = process_response(Response, FunlRequest),
  io:format("~n~p~n", [ProcessingInfo]),
  case ProcessingInfo of
    {done, _} ->
      io:format("~nRequest 200 OK ~n"),
      ok;
    {dead, FunlRequest} ->
      {ok, _} = tinymq:push("pending_requests", FunlRequest),
      io:format("~nRequest ~n~p~n readded to queue~n", [FunlRequest]),
      ok;
    {retry, FunlRequest} ->
      Timer = erlang:send_after(4000, self(), FunlRequest),
      io:format("~n[#~B]Request ~n~p~n readded to pending because of response ~n~p~n",
        [FunlRequest#funl_request.errCount + 1, FunlRequest#funl_request.request, Response]),
      {noreply, Timer}
  end.

process_response({_, "200", _, FunlRequest}, _) ->
  NewRequest = FunlRequest#funl_request{state = done, errCount = 0},
  {done, NewRequest};
process_response(_, FunlRequest) when ?should_stop(FunlRequest) ->
  NewRequest = FunlRequest#funl_request{state = dead, errCount = 0},
  {dead, NewRequest};
process_response(_, FunlRequest) ->
  NewErrCount = FunlRequest#funl_request.errCount + 1,
  NewRequest = FunlRequest#funl_request{errCount = NewErrCount, state = retry},
  {retry, NewRequest}.
