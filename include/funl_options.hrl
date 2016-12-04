-record(options, {
  max_errors = 100 :: non_neg_integer(),
  max_redirects = 15 :: non_neg_integer(),
  delay_factor = 5 :: non_neg_integer(),
        
  endpoint = "http://localhost:8081" :: string(),
  route_strategy = all_paths_relative_to_enpoint :: all_to_endpoint | all_paths_relative_to_enpoint
}).
