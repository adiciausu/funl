-module(funl_timed_queue).
-include("../include/funl_queue_item.hrl").
-export([enq/2, deq/0, start/1]).

start({disc_copies, DiskCopies}) ->
    ok = case mnesia:create_table(queue_item, [
        {disc_copies, DiskCopies},
        {type, ordered_set},
        {attributes, record_info(fields, queue_item)}]) of
             {atomic, ok} -> ok;
             {aborted, {already_exists, _Tab}} -> ok
         end.

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
