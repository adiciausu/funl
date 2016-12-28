-module(funl_queue_balancer).
-include("../include/funl_queue_item.hrl").
-include("../include/funl_request.hrl").
-include("../include/funl_options.hrl").
-behaviour(gen_server).
-define(BatchPercent, 10). %move 10% of requests to disk
-define(delay, 10). %seconds
-define(maxMemory, 1000000000). %bytes

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
    lager:info("Balance round at ~B seconds", [?delay]),
    Timer = erlang:send_after(1, self(), balance),
    {ok, #state{options = Options, timer = Timer, delay = 1000 * ?delay}}.

handle_info(balance, #state{timer = Timer, delay = Delay, options = Options} = State) ->
    erlang:cancel_timer(Timer),
    funl_alert:check_max_queued_req(Options),
    Size = funl_mnesia_queue:size() * erlang:system_info(wordsize),
    Free = ?maxMemory - Size,
    lager:info("[Balancer] Memory buffer free size ~f Mb ", [Free / 1000000]),
    ok = do_balance(Free),
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

do_balance(Available) when Available < 0 ->
    BatchSize = round(funl_mnesia_queue:count() / ?BatchPercent),
    Items = funl_mnesia_queue:rev_deq(BatchSize),
    lager:info("Queued ~B items to disk", [length(Items)]),
    ok = funl_disk_queue:enq(Items);
do_balance(Available) when Available > ?maxMemory * ?BatchPercent / 100 ->
    QueueItems = funl_disk_queue:deq(),
    case length(QueueItems) > 0 of
        true ->
            memory_bulk_enq(QueueItems),
            lager:info("Dequed ~B items from disk", [length(QueueItems)]);
        false -> ok
    end,
    ok;
do_balance(_Count) ->
    lager:info("Nothing to balance"),
    ok.

memory_bulk_enq([]) ->
    ok;
memory_bulk_enq([#queue_item{next_iteration = NextIter, item = WrappedItem} | Items]) ->
    ok = funl_mnesia_queue:enq(WrappedItem, NextIter),
    memory_bulk_enq(Items).