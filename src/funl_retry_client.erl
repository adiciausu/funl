-module(funl_retry_client).
-include("funl_request.hrl").
-include("funl_retry_options.hrl").
-export([send/2]).

send(FunlRequest, Endpoint) ->
  Options = #retry_options{},
  send(FunlRequest, Endpoint, Options).

send(FunlRequest, Host, Options) ->
  Response = ibrowse:send_req(Host, [], get),
  handle_response(Response, FunlRequest, Options).

handle_response({ok, "200", _Head, _Body}, FunlRequest, _Options) ->
  WrappedRequest = FunlRequest#funl_request.wrappedRequest,
  io:format("[Done] (~s)~s ~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest)]),
  NewRequest = FunlRequest#funl_request{state = done, errCount = 0},
  {done, NewRequest};

handle_response({ok, StatusCode, _Head, _Body}, Req, Options)
  when "301" == StatusCode ; "302" == StatusCode,
  Req#funl_request.redirectCount == Options#retry_options.maxRedirectCount ->

  WrappedReq = Req#funl_request.wrappedRequest,
  NewReq = #funl_request{wrappedRequest = WrappedReq, state = dead},
  {ok, _} = tinymq:push("dead", NewReq),
  io:format("[Dead#to_many_redirects] (~s)~s ~n", [cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
  {dead, NewReq};

handle_response({ok, StatusCode, Head, _Body}, FunlRequest, _Options) when "301" == StatusCode ; "302" == StatusCode ->
  WrappedRequest = FunlRequest#funl_request.wrappedRequest,
  NewFunlRequest = #funl_request{wrappedRequest = WrappedRequest,
    redirectCount = 1 + FunlRequest#funl_request.redirectCount, state = redirecting},
  case (lists:keyfind("Location", 1, Head)) of
    false ->
      {dead, FunlRequest};
    {"Location", RedirectUrl} ->
      io:format("[Redirecting] (~s)~s ... ~n ... to ~s ~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest),
        RedirectUrl]),
      send(NewFunlRequest, RedirectUrl),
      {done, FunlRequest}
  end;

handle_response(_, Req, Options) when (Req#funl_request.errCount == Options#retry_options.maxErrCount) ->
  WrappedRequest = Req#funl_request.wrappedRequest,
  NewRequest = #funl_request{wrappedRequest = WrappedRequest},
  {ok, _} = tinymq:push("dead", NewRequest),
  io:format("[Dead#to_many_errors] (~s)~s ~n", [cowboy_req:method(WrappedRequest),
    cowboy_req:url(WrappedRequest)]),
  {dead, NewRequest};

handle_response({ok, Status, _Head, _Body}, FunlRequest, _Options) ->
  WrappedRequest = FunlRequest#funl_request.wrappedRequest,
  NewErrCount = FunlRequest#funl_request.errCount + 1,
  NewFunlRequest = FunlRequest#funl_request{errCount = NewErrCount, state = retrying, wrappedRequest = WrappedRequest},
  io:format("[Retrying#~B] (~s)~s -> status code ~s~n", [NewFunlRequest#funl_request.errCount,
    cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest), Status]),
  erlang:start_timer(1000 * NewFunlRequest#funl_request.errCount, self(), NewFunlRequest),
  {retrying, NewFunlRequest}.
