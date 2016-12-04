-record(request, {
    id :: string(),
    next_retry = 0 :: non_neg_integer(),
    err_count = 0 :: non_neg_integer(),
    redirect_count = 0 :: non_neg_integer(),
    state = pending :: pending | redirecting | retrying | done | dead,
    wrapped_request :: tuple()
}).
