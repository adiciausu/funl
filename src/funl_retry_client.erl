-module(funl_retry_client).
-include("funl_request.hrl").
-include("funl_options.hrl").
-export([send/2, send/3]).

send(#request{wrappedRequest = WrappedReq} = Req, #options{endpoint = Endpoint, route_strategy = all_paths_relative_to_enpoint} = Options) ->
  NewEndpoint = string:concat(Endpoint, string:concat(binary_to_list(cowboy_req:path(WrappedReq)), binary_to_list(cowboy_req:qs(WrappedReq)))),
  send(Req, Options, NewEndpoint);

send(Req, #options{endpoint = Endpoint, route_strategy = all_to_endpoint} = Options) ->
  send(Req, Options, Endpoint).

send(#request{wrappedRequest = WrappedReq} = Req, Options, Endpoint) ->
  erlang:display(Endpoint),
  Method = list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(WrappedReq)))),
  Headers = cowboy_req:headers(WrappedReq),

  try ibrowse:send_req(Endpoint, Headers, Method) of
    Resp -> handle_response(Resp, Req, Options)
  catch
    _:Error -> handle_response({error, caught, Error}, Req, Options)
  end.

handle_response({ok, "200", _Head, _Body}, Req, _Options) ->
  WrappedRequest = Req#request.wrappedRequest,
  io:format("[Done] (~s)~s ~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest)]),
  {done, Req#request{state = done}};

handle_response(_Resp, Req, Options)
  when Req#request.redirectCount >= Options#options.max_redirects_until_declared_error ->
  WrappedReq = Req#request.wrappedRequest,
  io:format("[#to_many_redirects] (~s)~s, will retry ~n", [cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
  {retrying, do_retry(Req, Options)};

handle_response({ok, StatusCode, Head, _Body}, Req, Opts) when "301" == StatusCode; "302" == StatusCode ->
  NewReq = Req#request{redirectCount = 1 + Req#request.redirectCount, state = redirecting},
  case (lists:keyfind("Location", 1, Head)) of
    false ->
      {retry, do_retry(Req, Opts)};
    {"Location", RedirectUrl} ->
      WrappedRequest = Req#request.wrappedRequest,
      io:format("[Redirecting#~B] (~s)~s ... ~n ... to ~s ~n", [NewReq#request.redirectCount, cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest),
        RedirectUrl]),
      send(NewReq, Opts, RedirectUrl),
      {done, Req}
  end;

handle_response(_, Req, Options) when (Req#request.errCount >= Options#options.max_errors_until_declare_dead) ->
  WrappedRequest = Req#request.wrappedRequest,
  NewRequest = #request{wrappedRequest = WrappedRequest},
  {ok, _} = tinymq:push("dead", NewRequest),
  io:format("[Dead#to_many_errors] (~s)~s ~n", [cowboy_req:method(WrappedRequest),
    cowboy_req:url(WrappedRequest)]),
  {dead, NewRequest};

%% ibrowse errors
handle_response({error, Error}, Req, Opts) ->
  erlang:display(Error),
  {retrying, do_retry(Req, Opts)};

%% http status code error (ex: 503)
handle_response({ok, _ErrorStatusCode, _Head, _Body}, Req, Opts) ->
  erlang:display(_ErrorStatusCode),
  {retrying, do_retry(Req, Opts)};

%% other unknown errors
handle_response({error, caught, Error}, Req, Opts) ->
  erlang:display(Error),
  {retrying, do_retry(Req, Opts)}.

do_retry(Req, Options) ->
  NewErrCount = Req#request.errCount + 1,
  NewReq = Req#request{errCount = NewErrCount, redirectCount = 0, state = retrying},
  Delay = calculate_delay(NewReq, Options),
  erlang:start_timer(Delay, self(),  ),
  WrappedReq = Req#request.wrappedRequest,
  io:format("[Retrying#~B] (~s)~s -> delay:~Bs ~n", [NewReq#request.errCount,
    cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq), round(Delay / 1000)]),
  NewReq.

calculate_delay(#request{errCount = ErrCount}, Options) ->
  Options#options.delay_factor * trunc(1000 * math:pow(2, ErrCount)).