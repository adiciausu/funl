-module(funl_disk_queue).
-define(DiskFile, "priv/disk_buffer.data").
-define(TmpDiskFile, "priv/disk_buffer_tmp.data").
-define(NewLineToken, "__NEW_LINE__").

-export([enq/1, deq/1]).

enq(Items) ->
    ok = filelib:ensure_dir(?DiskFile),
    {ok, BufferIoDevice} = file:open(?DiskFile, [append]),
    enq(Items, BufferIoDevice).

enq([], BufferIoDevice) ->
    file:close(BufferIoDevice),
    ok;
enq([Item | Rest], BufferIoDevice) ->
    BinaryItem = serialize(Item),
    ok = file:write(BufferIoDevice, io_lib:fwrite("~s~n", [BinaryItem])),
    enq(Rest, BufferIoDevice).

deq(Count) ->
    case filelib:is_file(?DiskFile) of
        true -> deq(?DiskFile, Count);
        false -> []
    end.
deq(FileName, Count) ->
    {ok, IoDevice} = file:open(FileName, read),
    Items = do_deq(IoDevice, Count),
    file:close(IoDevice),
    case (length(Items) < Count) of
        true -> erlang:display(length(Items)), erlang:display(Count), file:delete(FileName);
        false -> ok
    end,
    case filelib:is_file(?TmpDiskFile) of %% get records saved in tmp file (if any)
        true ->
            file:delete(FileName),
            ok = file:rename(?TmpDiskFile, FileName);
        false ->
            ok
    end,
    Items.

do_deq(IoDevice, Count) when Count > 0 ->
    case io:get_line(IoDevice, "") of
        eof -> [];
        QueueItem ->
            lists:append([deserialize(QueueItem)], do_deq(IoDevice, Count - 1))
    end;
do_deq(IoDevice, Count) when Count =< 0 ->
    case io:get_line(IoDevice, "") of
        eof -> [];
        QueueItem ->
            ok = filelib:ensure_dir(?TmpDiskFile), %% write extra records to tmp file
            {ok, TmpIoDevice} = file:open(?TmpDiskFile, [append]),
            ok = file:write(TmpIoDevice, io_lib:fwrite("~s", [QueueItem])),
            do_deq(IoDevice, 0, TmpIoDevice)
    end.
do_deq(IoDevice, _, TmpIoDevice) ->
    case io:get_line(IoDevice, "") of
        eof ->
            ok = file:close(TmpIoDevice),
            [];
        QueueItem ->
            ok = file:write(TmpIoDevice, io_lib:fwrite("~s", [QueueItem])),
            do_deq(IoDevice, 0, TmpIoDevice)
    end.

deserialize(Item) ->
    Item2 = re:replace(Item, ?NewLineToken, "\n", [global]),
    binary_to_term(list_to_binary(Item2)).

serialize(Item) ->
    BinaryTerm = term_to_binary(Item),
    re:replace(BinaryTerm, "\n", ?NewLineToken, [global]).