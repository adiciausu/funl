-record(request, {
    received_at = 0,
    err_count = 0 :: non_neg_integer(),
    redirect_count = 0 :: non_neg_integer(),
    ttl = 0 :: non_neg_integer(),
    state = pending :: pending | redirecting | retrying | done | dead,
    wrapped_request :: tuple()
}).
