-module(funl_timed_queue).
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
-record(queue_item, {
    id :: non_neg_integer(),
    next_iteration :: non_neg_integer(),
    item :: tuple()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enq(Req, UnlockTime) ->
    gen_server:call(?MODULE, {enq, Req, UnlockTime}).
deq() ->
    gen_server:call(?MODULE, {deq}).
init([]) ->
    {atomic, ok} = mnesia:create_table(queue_item, [
        {ram_copies, [node()]},
        {type, ordered_set},
        {attributes, record_info(fields, queue_item)}]
    ),
    {ok, #state{}}.

handle_call({enq, Item, UnlockTime}, _From, State) ->
    QueuedItem = #queue_item{id = funl_uid:generate(UnlockTime), next_iteration = UnlockTime, item = Item},
    ok = mnesia:dirty_write(QueuedItem),
    {reply, sucess, State};

handle_call({deq}, _From, State) ->
    T = fun() ->
        case mnesia:read(queue_item, mnesia:first(queue_item), write) of
            [] -> [];
            [#queue_item{next_iteration = UnlockTime, id = Id, item = Item}] ->
                IsUnlocked = UnlockTime < funl_uid:timestamp(),
                case IsUnlocked of
                    true ->
                        ok = mnesia:delete({queue_item, Id}),
                        Item;
                    false -> []
                end
        end
        end,
    {atomic, Data} = mnesia:transaction(T),
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
