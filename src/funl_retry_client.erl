-module(funl_retry_client).
-include("funl_request.hrl").
-export([send/2]).
-define(should_stop(FunlReq), element(2, FunlReq) == 5).

send(FunlRequest, Host) ->
  Response = ibrowse:send_req(Host, [], get),
  handle_response(Response, FunlRequest).

handle_response({ok, "302", Head, _Body}, FunlRequest) ->
  io:format("Following redirect...~n"),
  case (lists:keyfind("Location", 1, Head)) of
    false ->
      {dead, FunlRequest};
    {"Location", RedirectUrl} ->
      erlang:display(RedirectUrl),
      send(FunlRequest, RedirectUrl),
      {done, FunlRequest}
  end;

handle_response({ok, "200", _Head, _Body}, FunlRequest) ->
  io:format("~nRequest handled succesfuly ~n"),
  NewRequest = FunlRequest#funl_request{state = done, errCount = 0},

  {done, NewRequest};

handle_response(_, FunlRequest) when ?should_stop(FunlRequest) ->
  NewRequest = FunlRequest#funl_request{state = dead, errCount = 0},
  WrappedRequest = FunlRequest#funl_request.wrappedRequest,
  {ok, _} = tinymq:push("dead", FunlRequest),
  io:format("~nRequest [~s]~s moved to dead requests queue~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest)]),

  {dead, NewRequest};

handle_response({ok, Status, _Head, _Body}, FunlRequest) ->
  NewErrCount = FunlRequest#funl_request.errCount + 1,
  NewFunlRequest = FunlRequest#funl_request{errCount = NewErrCount, state = retry},
  WrappedRequest = NewFunlRequest#funl_request.wrappedRequest,
  io:format("~nRequest [~s]~s failed ~B times with status code ~s", [cowboy_req:method(WrappedRequest),
    cowboy_req:url(WrappedRequest), NewFunlRequest#funl_request.errCount, Status]),
  erlang:start_timer(1000 * NewFunlRequest#funl_request.errCount, self(), NewFunlRequest),

  {retry, NewFunlRequest}.
