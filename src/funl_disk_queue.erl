-module(funl_disk_queue).
-define(DataFolder, "data/queue/").
-define(DiskFile, ?DataFolder + "disk_buffer_~B.data").
-define(NewLineToken, "__NEW_LINE__").
-export([enq/1, deq/0]).

enq(Items) ->
    File = io_lib:fwrite(?DiskFile,[ funl_uid:timestamp()]),
    ok = filelib:ensure_dir(File),
    {ok, BufferIoDevice} = file:open(File, [append]),
    ok = enq(Items, BufferIoDevice),
    file:close(BufferIoDevice).

enq([], _) ->
    ok;
enq([Item | Rest], BufferIoDevice) ->
    BinaryItem = serialize(Item),
    ok = file:write(BufferIoDevice, io_lib:fwrite("~s~n", [BinaryItem])),
    enq(Rest, BufferIoDevice).

deq() ->
    ok = filelib:ensure_dir(?DataFolder),
    {ok, Filenames} = file:list_dir(?DataFolder),
    deq(Filenames).

deq([]) ->
    [];
deq([Filename | _]) ->
    ok = filelib:ensure_dir(?DataFolder),
    Path = string:concat(?DataFolder, Filename),
    {ok, IoDevice} = file:open(Path, read),
    Items = do_deq(IoDevice),
    file:close(IoDevice),
    ok = file:delete(Path),
    Items.

do_deq(IoDevice)->
    case io:get_line(IoDevice, "") of
        eof -> [];
        QueueItem ->
            lists:append([deserialize(QueueItem)], do_deq(IoDevice))
    end.

deserialize(Item) ->
    Item2 = re:replace(Item, ?NewLineToken, "\n", [global]),
    binary_to_term(list_to_binary(Item2)).

serialize(Item) ->
    BinaryTerm = term_to_binary(Item),
    re:replace(BinaryTerm, "\n", ?NewLineToken, [global]).