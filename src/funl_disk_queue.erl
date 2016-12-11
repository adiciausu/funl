-module(funl_disk_queue).
-define(DiskFile, "priv/disk_buffer.data").
-define(TmpDiskFile, "priv/disk_buffer_tmp.data").
-define(NewLineToken, "__NEW_LINE__").

%% API
-export([enq/1, deq/1]).

enq([]) ->
    ok;
enq([Item | Rest]) ->
    ok = filelib:ensure_dir(?DiskFile),
    BinaryItem = deserialize(Item),
    ok = file:write_file(?DiskFile, io_lib:fwrite("~s~n", [BinaryItem]), [append]),
    enq(Rest).

deq(Count) ->
    case filelib:is_file(?DiskFile) of
        true ->
            QueueItems = deq(?DiskFile, Count),
%%            erlang:display(QueueItems),
            QueueItems;
        false ->
            []
    end.
deq(FileName, Count) ->
    {ok, IoDevice} = file:open(FileName, read),
    Items = do_deq(IoDevice, Count),
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
            lists:append([serialize(QueueItem)], do_deq(IoDevice, Count - 1))
    end;
do_deq(IoDevice, Count) when Count =< 0 ->
    case io:get_line(IoDevice, "") of
        eof -> [];
        QueueItem ->
            ok = filelib:ensure_dir(?TmpDiskFile), %% write extra records to tmp file
            ok = file:write_file(?TmpDiskFile, io_lib:fwrite("~s", [QueueItem]), [append]),
            do_deq(IoDevice, 0),
            []
    end.

serialize(Item) ->
    Item2 = re:replace(Item, ?NewLineToken, "\n", [global]),
    binary_to_term(list_to_binary(Item2)).

deserialize(Item) ->
    BinaryTerm = term_to_binary(Item),
    re:replace(BinaryTerm, "\n", ?NewLineToken, [global]).