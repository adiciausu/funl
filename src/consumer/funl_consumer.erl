-module(funl_consumer).
-include("../../include/funl_options.hrl").
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).

start_link(Options, Queue) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options, Queue], []).

init([Options, Queue]) ->
    consume(Queue, Options),
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

%%    MUST REFACTOR THIS
consume(Queue, Options) ->
    {_, _, Time} = os:timestamp(),
    Reqs = funl_queue:peek(Time),
    consume(Queue, Options, Reqs).

consume(Queue, Options, [Req | _Reqs]) ->
    funl_retry_client:send(Req, Options),
    consume(Queue, Options, _Reqs);
consume(Queue, Options, []) ->
    consume(Queue, Options).