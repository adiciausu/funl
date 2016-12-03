-module(funl_queue_consumer).

-export([start/1, consume/3]).

start(Queue) ->
  Timestamp = tinymq:now(Queue),
  Host = "http://sstackoverflow.com/questions/4103731/is-it-possible-to-use-record-name-as-a-parameter-in-erlang",
  spawn(funl_queue_consumer, consume, [Timestamp, Queue, Host]).

consume(Timestamp, Queue, Host) ->
  tinymq:subscribe(Queue,
    Timestamp,     % The 'now' atom or a Timestamp
    self()   % the process that will recieve the messages
  ),

  receive
  %% hack for problem with tinymq enclosing FunlRequest in array recursively
    {_From, NewTimestamp, [FunlRequest | _]} ->
      funl_retry_client:send(FunlRequest, Host),
      consume(NewTimestamp, Queue, Host);
    {_From, NewTimestamp, FunlRequest} ->
      funl_retry_client:send(FunlRequest, Host),
      consume(NewTimestamp, Queue, Host)
  end.
