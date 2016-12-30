# What is funl?
Funl is a high performance async http proxy that can queue the requests it receives and send them to your backend at a rate defined by you, leveling throughput spikes and retring failed requests with exponential backoff.  

# Use cases 
1. __Collecting big data__
   * You can send all the requests to funl and let your system process them at a desired rate
2. __Microservice communication__
   * Make sure your requests are sent to the other microservice, even if you encounter network (or other transient) errors
3. __Payment systems__
   * Make sure you receive ALL payment requests, even if your payment processing system is down
4. __Message(sms, email, push notification etc.) sending__
   * Make sure your message is delivered, even if you encounter errors, but do not send the message if it is too old (ttl expired)
   
# Main features
1. __High throughput__:  
    * Funl uses [Cowboy] to listen for http requests
    * Take a look at this [benchmark] of cowboy and other popular solutions. (NB: the benchmark was done by a cowboy competitor)
2. __Transient error handling__
    * If funl encounters an error when sending the request to the backend it will retry sending that request with exponential backoff retry logic
    * You can tell funl to stop retring a request after a certain amount of time (ttl) or when a maximum error threshold is reached
    * You can tell funl not to retry a request if certain status codes are encountered in the response ("ex: 404 not found makes no sense to be retried under normal http RFC)
3. __Low memory footprint__
    * Funl will use ~ 1 Gb of memory for storing requests at peak usage, dumping them to disk and reloading them in memory when needed

### Coming up!
1. Backend health check and failure prevention
2. Multiple backends (now it can be done using a load balancer like [HAProxy])

    
# Install (The easy way)
[Download funl] and unarchive:
```sh
$ mkdir funl
$ wget https://github.com/adrianciausu/funl/releases/download/0.1.0/funl-0.0.1.tar.gz 
$ tar -xzvf funl-0.0.1.tar.gz -C funl
```
# Install (From Source)
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
## Start funl
```sh
$ path-to-parent-folder/funl/bin/funl start
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
[benchmark]: http://www.ostinelli.net/a-comparison-between-misultin-mochiweb-cowboy-nodejs-and-tornadoweb/
[HAProxy]: http://www.haproxy.org/
[Cowboy]: https://github.com/ninenines/cowboy
