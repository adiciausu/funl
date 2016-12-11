-module(funl_queue_balancer).
-include("../include/funl_queue_item.hrl").
-include("../include/funl_options.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    options :: tuple(),
    timer :: any(),
    delay :: non_neg_integer()
}).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

init([Options]) ->
    NextRoundDelay = calculate_delay(Options),
    io:format("Balance round at ~B seconds~n", [NextRoundDelay]),
    Timer = erlang:send_after(1, self(), balance),
    {ok, #state{options = Options, timer = Timer, delay = 1000 * NextRoundDelay}}.

handle_info(balance, #state{options = #options{requst_queue_buffer_size = BufferSizeMins, backend_max_req = MaxReq} = Options,
    timer = Timer, delay = Delay} = State) ->
    erlang:cancel_timer(Timer),
    Size = funl_mnesia_queue:size(),
    MaxSize = MaxReq * 3600 * BufferSizeMins,
    Count = Size - MaxSize,
    erlang:display(Count),
    ok = do_balance(Count),
    NewTimer = erlang:send_after(Delay, self(), balance),
    {noreply, State#state{timer = NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

calculate_delay(#options{requst_queue_buffer_size = BufferSizeMins, requst_queue_balance_allowed_margin = Margin}) ->
    BufferSizeMins div Margin.

do_balance(Count) when Count > 0 ->
    Items = funl_mnesia_queue:rev_deq(Count),
    io:format("Queued ~B items to disk~n", [length(Items)]),
    ok = funl_disk_queue:enq(Items);
do_balance(Count) when Count < 0 ->
    FileRowCount = Count * -1,
    QueueItems = funl_disk_queue:deq(FileRowCount),
    io:format("Dequed ~B items from disk~n", [length(QueueItems)]),
    ok;
do_balance(Count) when Count == 0 ->
    ok.