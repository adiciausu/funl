-module(funl_queue_consumer).

-export([start/0, consume/1]).

start() ->
  Timestamp = tinymq:now("pending_requests"),
  spawn(funl_queue_consumer, consume, [Timestamp]).

consume(Timestamp) ->
  tinymq:subscribe("pending_requests",
    Timestamp,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
  %% hack for problem with tinymq enclosing FunlRequest in array recursively
    {_From, NewTimestamp, [FunlRequest | _]} ->
      funl_retry_client:send(FunlRequest),
      consume(NewTimestamp);
    {_From, NewTimestamp, FunlRequest} ->
      funl_retry_client:send(FunlRequest),
      consume(NewTimestamp)
  end.
