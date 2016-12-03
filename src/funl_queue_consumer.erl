-module(funl_queue_consumer).

-export([start/2, consume/3]).

start(Queue, Options) ->
  Timestamp = tinymq:now(Queue),
  spawn(funl_queue_consumer, consume, [Timestamp, Queue, Options]).

consume(Timestamp, Queue, Options) ->
  tinymq:subscribe(Queue,
    Timestamp,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
  %% hack for problem with tinymq enclosing FunlRequest in array recursively
    {_From, NewTimestamp, [Req | _]} ->
      funl_retry_client:send(Req, Options),
      consume(NewTimestamp, Queue, Options);
    {_From, NewTimestamp, Req} ->
      funl_retry_client:send(Req, Options),
      consume(NewTimestamp, Queue, Options)
  end.
