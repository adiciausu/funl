-record(request, {
  errCount = 0 :: non_neg_integer(),
  redirectCount = 0 :: non_neg_integer(),
  wrappedRequest :: tuple(),
  state = pending :: pending | redirecting | retrying | done | dead
}).
