-module(funl_retry_client).
-include("../include/funl_request.hrl").
-include("../include/funl_options.hrl").
-export([send/2]).

send(#request{wrapped_request = WrappedReq} = Req, #options{endpoint = Endpoint, route_strategy = all_paths_relative_to_enpoint} = Options) ->
    NewEndpoint = string:concat(Endpoint, string:concat(binary_to_list(cowboy_req:path(WrappedReq)), binary_to_list(cowboy_req:qs(WrappedReq)))),
    send(Req, Options, NewEndpoint);

send(Req, #options{endpoint = Endpoint, route_strategy = all_to_endpoint} = Options) ->
    send(Req, Options, Endpoint).

send(#request{wrapped_request = WrappedReq} = Req, Options, Endpoint) ->
    erlang:display(Endpoint),
    Method = list_to_atom(string:to_lower(binary_to_list(cowboy_req:method(WrappedReq)))),
    Headers = cowboy_req:headers(WrappedReq),
    
    try ibrowse:send_req(Endpoint, Headers, Method) of
        Resp -> handle_response(Resp, Req, Options)
    catch
        _:Error -> handle_response({error, caught, Error}, Req, Options)
    end.

%%Ok
handle_response({ok, "200", _Head, _Body}, Req, _Options) ->
    WrappedRequest = Req#request.wrapped_request,
    io:format("[Done] (~s)~s ~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest)]),
    {done, Req#request{state = done}};

%% max redirects
handle_response(_Resp, Req, Options)
    when Req#request.redirect_count >= Options#options.max_redirects ->
    WrappedReq = Req#request.wrapped_request,
    io:format("[#to_many_redirects] (~s)~s, will retry ~n", [cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
    {retrying, do_retry(Req, Options)};
%% redirect
handle_response({ok, StatusCode, Head, _Body}, Req, Opts) when "301" == StatusCode; "302" == StatusCode ->
    NewReq = Req#request{redirect_count = 1 + Req#request.redirect_count, state = redirecting},
    case (lists:keyfind("Location", 1, Head)) of
        false ->
            {retry, do_retry(Req, Opts)};
        {"Location", RedirectUrl} ->
            WrappedRequest = Req#request.wrapped_request,
            io:format("[Redirecting#~B] (~s)~s ... ~n ... to ~s ~n", [NewReq#request.redirect_count, cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest),
                RedirectUrl]),
            send(NewReq, Opts, RedirectUrl),
            {done, Req}
    end;
%% max errs
handle_response(_Resp, Req, Options)
    when Req#request.err_count >= Options#options.max_errors ->
    WrappedReq = Req#request.wrapped_request,
    io:format("[#to_many_redirects] (~s)~s, will retry ~n", [cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
    {retrying, do_retry(Req, Options)};
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
    NewErrCount = Req#request.err_count + 1,
    NewReq = Req#request{err_count = NewErrCount, redirect_count = 0, state = retrying},
    Delay = calculate_delay(NewReq, Options),
    funl_timed_queue:enq(NewReq, funl_uid:timestamp() + Delay),
        
    WrappedReq = Req#request.wrapped_request,
    io:format("[Retrying#~B] (~s)~s -> delay:~Bs ~n", [NewReq#request.err_count,
        cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq), trunc(Delay / 1000000)]),
    NewReq.

calculate_delay(#request{err_count = ErrCount}, Options) ->
    1000000 * trunc(math:pow(Options#options.delay_factor, ErrCount)).