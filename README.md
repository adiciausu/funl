# Install



# Usage

#### Start funl
```sh
$ path-to-funl-parent-folder/funl/bin/funl start
```

Funl will now listen for http requests and proxy them to your backend endpoint 

#### Config
The config can be found here:
```sh
$ path-to-funl-parent-folder/funl/conf.yml
```
Config reference:
```sh
endpoint: http://mydomain.com/collect-data # (required) the backend where the requests will be routed
route_strategy:  all_paths_relative_to_enpoint  # route requests with path relative to backend (ex: ex.com/test -> ex.com/test)
#route_strategy:  all_to_endpoint # route all requests to the specific endpoint, discard path (ex: ex.com/test -> ex2.com)
backend_max_req: 20 # maximum requests per second sent to the backend (adjust to a limit your system is comfortable with)
listen_on_port: 80 # port to listen for http requests

max_errors: 5 # max number of errors until request declared dead, and alert sent
dead_status_codes: [ "408" ] # list of status codes that mark a request dead with no retry
delay_factor: 2 # increase this to increase the delay of the retries; next_delay = pow(delay_factor, current_error_count) + or - 1%
default_request_ttl: 36000 # how long to keep a request until declaring it dead; in seconds;[integer()| none]; none => never expires; overriden by request X-Funl-Ttl header
max_redirects: 15 # maximum number of redirects until request declared as failed and retried

alert_queued_requests_max_count: 10000000 # alert when queued requests increase over this limit
alert_dead_request: true # alert for each dead request
alert_email_receiver: example@gmail.com # email that will receive alerts
alert_email_relay: smtp.gmail.com 
alert_email_username: example@gmail.com
alert_email_password: password
alert_email_ssl: true # required for smtp.gmail.com
```



