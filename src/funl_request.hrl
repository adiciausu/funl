-record(funl_request, {
  errCount :: non_neg_integer(),
  request :: tuple(),
  state = pending :: dead | pending | retry | done
}).
