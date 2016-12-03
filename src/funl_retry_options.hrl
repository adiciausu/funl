-record(retry_options, {
  maxErrCount = 3 :: non_neg_integer(),
  maxRedirectCount = 1 :: non_neg_integer()
}).
