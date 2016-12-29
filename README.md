

### Install
##### The easy way 
[Download funl] and unarchive:

```sh
$ wget https://github.com/adrianciausu/funl/releases/download/0.1.0/funl-0.0.1.tar.gz 
$ tar -xfv funl-0.0.1.tar.gz 
```
##### From Source
Install [erlang] 19 and [rebar3] and then:
```sh
$ git clone https://github.com/adrianciausu/funl.git
$ cd funl
$ rebar3 compile
```
You can find the built app folder here
```sh
$ path-to-parent-folder/funl/_build/rel/funl
```
### Start funl
```sh
$ path-to-parent-folder/funl/bin/funl start
```

Funl will now listen for http requests and proxy them to your backend endpoint 

##### Config
The config can be found here:
```sh
$ path-to-funl-parent-folder/funl/conf.yml
```
Config reference:
```sh
endpoint: http://mydomain.com/collect-data # (required) the backend where the requests will be routed
route_strategy:  all_paths_relative_to_enpoint  # route requests with path relative to backend (ex: ex.com/test -> ex.com/test)
#route_strategy:  all_to_endpoint # route all requests to the specific endpoint, discard path (ex: ex.com/test -> ex2.com)
backend_max_req: 100 # maximum requests per second sent to the backend (adjust to a limit your system is comfortable with)
listen_on_port: 80 # port to listen for http requests

max_errors: 10 # max number of errors until request declared dead, and alert sent
dead_status_codes: [ "404" ] # list of status codes that mark a request dead with no retry
delay_factor: 2 # increase this to increase the delay of the retries; next_delay = pow(delay_factor, current_error_count) + or - 1%
default_request_ttl: 36000 # how long (in seconds) to keep a request until declaring it dead; [integer()| none]; none => never expires; overriden by request X-Funl-Ttl header
max_redirects: 100 # maximum number of redirects until request declared as failed and retried

alert_queued_requests_max_count: 1000 # alert when queued requests increase over this limit
alert_dead_request: true # alert for each dead request
alert_email_receiver: example@gmail.com # email that will receive alerts
alert_email_relay: smtp.gmail.com 
alert_email_username: example@gmail.com
alert_email_password: password
alert_email_ssl: true # required for smtp.gmail.com
```




[rebar3]: <https://www.rebar3.org/>
[erlang]: https://www.erlang.org/
[Download funl]: https://github.com/adrianciausu/funl/releases/download/0.1.0/funl-0.0.1.tar.gz


