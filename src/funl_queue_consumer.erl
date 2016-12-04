-module(funl_queue_consumer).
-include("../include/funl_options.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Queue = "pending",
    Options = #options{},
    Timestamp = tinymq:now(Queue),
    consume(Timestamp, Queue, Options),
    {ok, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
