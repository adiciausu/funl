-module(funl_request_handler).
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
    {ok, Body, _} = cowboy_req:body(WrappedReq),
    Headers = headers(WrappedReq),
    Resp = ibrowse:send_req(Endpoint, Headers, Method, Body),
    handle_response(Resp, Req, Options).

headers(WrappedReq) ->
    Headers = cowboy_req:headers(WrappedReq),
    headers(Headers, []).
headers([], Acc) -> Acc;
headers([{Key, Value} = Header | Rest], Acc) ->
    case Key of
        <<"host">> -> headers(Rest, lists:append(Acc, [{<<"X-Forwarded-For">>, Value}]));
        _ -> headers(Rest, lists:append(Acc, [Header]))
    end.

%%Ok
handle_response({ok, "200", _Head, _Body}, Req, _Options) ->
    WrappedRequest = Req#request.wrapped_request,
    io:format("[Done] (~s)~s ~n", [cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest)]),
    {done, Req#request{state = done}};

%% http status code error (ex: 503)
handle_response({ok, _ErrorStatusCode, _Head, _Body}, Req, Opts) ->
    erlang:display(_ErrorStatusCode),
    do_retry(Req, Opts);

%% max redirects
handle_response(_Resp, Req, Options)
    when Req#request.redirect_count >= Options#options.max_redirects ->
    WrappedReq = Req#request.wrapped_request,
    io:format("[to_many_redirects#~B] (~s)~s, will retry ~n", [Req#request.redirect_count, cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
    do_retry(Req, Options);

%% redirect
handle_response({ok, StatusCode, Head, _Body}, Req, Opts) when "301" == StatusCode; "302" == StatusCode ->
    NewReq = Req#request{redirect_count = 1 + Req#request.redirect_count, state = redirecting},
    case (lists:keyfind("Location", 1, Head)) of
        false ->
            do_retry(Req, Opts);
        {"Location", RedirectUrl} ->
            WrappedRequest = Req#request.wrapped_request,
            io:format("[Redirecting#~B] (~s)~s ... ~n ... to ~s ~n", [NewReq#request.redirect_count, cowboy_req:method(WrappedRequest), cowboy_req:url(WrappedRequest),
                RedirectUrl]),
            send(NewReq, Opts, RedirectUrl),
            {done, Req}
    end;

%% ibrowse errors
handle_response({error, Error}, Req, Opts) ->
    erlang:display(Error),
    do_retry(Req, Opts).
    
%%internal
do_retry(#request{err_count = ErrCount, wrapped_request = WrappedReq} = Req, #options{max_errors = MaxErr}) when ErrCount == MaxErr ->
    io:format("[to_many_errors#~B] (~s)~s, declared dead ~n", [Req#request.err_count,
        cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq)]),
    declare_dead(Req);
do_retry(Req, Options) ->
    NewErrCount = Req#request.err_count + 1,
    
    NewReq = Req#request{err_count = NewErrCount, redirect_count = 0, state = retrying},
    Delay = calculate_delay(NewReq, Options),
    funl_timed_queue:enq(NewReq, funl_uid:timestamp() + Delay),
    
    WrappedReq = Req#request.wrapped_request,
    io:format("[Retrying#~B] (~s)~s -> delay:~Bs ~n", [NewReq#request.err_count,
        cowboy_req:method(WrappedReq), cowboy_req:url(WrappedReq), trunc(Delay / 1000000)]),
    {retrying, NewReq}.

declare_dead(Req) ->
    {Date, Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    {Hour, Min, Second} = Time,
    LogPath = lists:flatten(io_lib:fwrite("/var/log/funl/~B-~B-~B.dead.log", [Year, Month, Day])),
    ok = filelib:ensure_dir(LogPath),
    ok = file:write_file(LogPath, io_lib:fwrite("~B-~B-~B ~B:~B:~B ~p\n",
        [Year, Month, Day, Hour, Min, Second, Req]), [append]),
    {dead, Req#request{state = dead}}.

calculate_delay(#request{err_count = ErrCount}, Options) ->
    1000000 * trunc(math:pow(Options#options.delay_factor, ErrCount)).
