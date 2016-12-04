-record(options, {
  max_errors_until_declare_dead = 3 :: non_neg_integer(),
  max_redirects_until_declared_error = 1 :: non_neg_integer(),
  endpoint :: string(),
  route_strategy = all_paths_relative_to_enpoint :: all_to_endpoint | all_paths_relative_to_enpoint,
  delay_factor = 1 :: non_neg_integer()
}).
