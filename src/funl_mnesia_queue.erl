-module(funl_mnesia_queue).
-include("../include/funl_queue_item.hrl").
-include_lib("stdlib/include/qlc.hrl").
-export([enq/2, deq/0, start/1, size/0, count/0, rev_deq/1]).

start({mem_and_disk, DiscCopies}) ->
    mnesia:create_schema(DiscCopies),
    mnesia:start(),
    ok = case mnesia:create_table(queue_item, [
        {disc_copies, DiscCopies},
        {type, ordered_set},
        {attributes, record_info(fields, queue_item)} ]) of
             {atomic, ok} -> ok;
             {aborted, {already_exists, _Tab}} -> ok
         end,
        mnesia:wait_for_tables([queue_item], 30000).

enq(Item, UnlockTime) ->
    QueuedItem = #queue_item{id = funl_uid:generate(UnlockTime), next_iteration = UnlockTime, item = Item},
    ok = mnesia:dirty_write(QueuedItem).

deq() ->
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
    Data.

size() ->
    mnesia:table_info(queue_item, memory).

count() ->
    mnesia:table_info(queue_item, size).

rev_deq(Count) ->
    T = fun() ->
        QH = mnesia:table(queue_item),
        SortedQH = qlc:sort(QH, {order, descending}),
        QC = qlc:cursor(SortedQH),
        Items = qlc:next_answers(QC, Count),
        ok = delete(Items),
        Items
        end,
    {atomic, Data} = mnesia:transaction(T),
    Data.

delete([]) ->
    ok;
delete([#queue_item{id = Id} | Items]) ->
    ok = mnesia:delete({queue_item, Id}),
    delete(Items).
    