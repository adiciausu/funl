-module(funl_sender).
-export([start/0, consume/1]).

start() ->
  Now = tinymq:now("pending_requests"),
  spawn(funl_sender, consume, [Now]).

consume(StartDate) ->
  tinymq:subscribe("pending_requests",
    StartDate,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
    {_From, _Timestamp, Request} ->
%%      io:format("Received request: ~p~n", [Request]),
      Response = ibrowse:send_req("http://google.ro/", [], get),
%%      io:format("Received response: ~p~n", [Response]),
      process(Response, Request),
      consume(StartDate)
  end.

process({_, "200", _, _}, _) ->
  ok;
process(Response, Request) ->
  {ok, _} = tinymq:push("pending_requests", Request),
  io:format("Request ~n~p~n readded to pending because of response ~n~p~n", [Request, Response]),
  ok.