-record(options, {
    max_errors = 100 :: non_neg_integer(),
    max_redirects = 15 :: non_neg_integer(),
    delay_factor = 5 :: non_neg_integer(),
    default_request_ttl = 0 :: non_neg_integer(),
    dead_status_codes = [] :: list(),
    
    endpoint = "http://localhost:8081" :: string(),
    backend_max_req = 3 :: non_neg_integer(),
    route_strategy = all_paths_relative_to_enpoint :: all_to_endpoint | all_paths_relative_to_enpoint,
    
%%  private, can't be changed by config
    %%in mins, how much request time processing should i buffer, based on backend_max_req
    requst_queue_buffer_size = 10 ::non_neg_integer(),
    %% how many times can i fail and still have requests queued in memory, if the are any to process
    requst_queue_balance_allowed_margin = 5 ::non_neg_integer()
}).
