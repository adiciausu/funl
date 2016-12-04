-record(request, {
  errCount = 0 :: non_neg_integer(),
  redirectCount = 0 :: non_neg_integer(),
  state = pending :: pending | redirecting | retrying | done | dead,
  wrappedRequest :: tuple()
}).
