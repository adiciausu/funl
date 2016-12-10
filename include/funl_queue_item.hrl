-record(queue_item, {
    id :: non_neg_integer(),
    next_iteration :: non_neg_integer(),
    item :: tuple()
}).
