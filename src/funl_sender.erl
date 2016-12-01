-module(funl_sender).

-include("request.hrl").

-export([start/0, consume/1]).

-define(should_stop(FunlReq), element(2, FunlReq) == 10).

start() ->
  Timestamp = tinymq:now("pending_requests"),
  spawn(funl_sender, consume, [Timestamp]).

consume(Timestamp) ->
  tinymq:subscribe("pending_requests",
    Timestamp,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
    %% hack for problem with tinymq enclosing FunlRequest in array recursively
    {_From, NewTimestamp, [FunlRequest | _]} ->
      retry_request(FunlRequest),
      consume(NewTimestamp);
    {_From, NewTimestamp, FunlRequest} ->
      retry_request(FunlRequest),
      consume(NewTimestamp)
  end.

retry_request({funl_request, ErrCount, Request}) ->
  Response = ibrowse:send_req("http://127.0.0.1:8081", cowboy_req:headers(Request), post),
  ok = process(Response, {funl_request, ErrCount, Request}),
  ok.

process({_, "200", _, _}, _) ->
  io:format("~nRequest 200 OK ~n"),
  ok;
process(_, FunlRequest) when ?should_stop(FunlRequest) ->
  io:format("~nRequest ~n~p~n discarded~n", [FunlRequest]),
  ok;
process(Response, {funl_request, ErrCount, Request}) ->
  {ok, _} = tinymq:push("pending_requests", {funl_request, ErrCount + 1, Request}),
  io:format("~n[#~B]Request ~n~p~n readded to pending because of response ~n~p~n",
    [ErrCount, Request, Response]),
  ok.
