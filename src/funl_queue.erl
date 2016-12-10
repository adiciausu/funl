-module(funl_queue).
-include("../include/funl_queue_item.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0, enq/2, deq/0]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enq(Req, UnlockTime) ->
    gen_server:call(?MODULE, {enq, Req, UnlockTime}).
deq() ->
    gen_server:call(?MODULE, {deq}).
init([]) ->
    ok = funl_timed_queue:start({disc_copies, [node()]}),
    {ok, #state{}}.

handle_call({enq, Item, UnlockTime}, _From, State) ->
    ok = funl_timed_queue:enq(Item, UnlockTime),
    {reply, sucess, State};

handle_call({deq}, _From, State) ->
    Data = funl_timed_queue:deq(),
    {reply, Data, State};

handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
