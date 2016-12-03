-record(funl_request, {
  errCount :: non_neg_integer(),
  wrappedRequest :: tuple(),
  state = pending :: dead | pending | retry | done
}).
