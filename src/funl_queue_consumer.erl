-module(funl_queue_consumer).

-export([start/2, consume/3]).

start(Queue, Endpoint) ->
  Timestamp = tinymq:now(Queue),
  spawn(funl_queue_consumer, consume, [Timestamp, Queue, Endpoint]).

consume(Timestamp, Queue, Endpoint) ->
  tinymq:subscribe(Queue,
    Timestamp,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
  %% hack for problem with tinymq enclosing FunlRequest in array recursively
    {_From, NewTimestamp, [FunlRequest | _]} ->
      funl_retry_client:send(FunlRequest, Endpoint),
      consume(NewTimestamp, Queue, Endpoint);
    {_From, NewTimestamp, FunlRequest} ->
      funl_retry_client:send(FunlRequest, Endpoint),
      consume(NewTimestamp, Queue, Endpoint)
  end.
