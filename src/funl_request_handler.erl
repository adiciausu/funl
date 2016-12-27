-module(funl_request_handler).
-include("../include/funl_request.hrl").
-include("../include/funl_options.hrl").
-export([send/2]).

send(#request{relative_url = RelativeUrl} = Req, #options{endpoint = Endpoint, route_strategy = all_paths_relative_to_enpoint} = Options) ->
    NewEndpoint = string:concat(Endpoint, RelativeUrl),
    send(Req, Options, NewEndpoint);
send(Req, #options{endpoint = Endpoint, route_strategy = all_to_endpoint} = Options) ->
    send(Req, Options, Endpoint).

%%check if specific ttl is set
send(#request{ttl = unset, received_at = ReceivedAt} = Req, #options{default_request_ttl = Ttl} = Options, Endpoint) ->
    case check_ttl(ReceivedAt, Ttl) of
        valid -> do_send(Req, Options, Endpoint);
        expired ->
            declare_dead(Req, default_ttl_expired)
    end;
send(#request{ttl = Ttl, received_at = ReceivedAt} = Req, Options, Endpoint) ->
    case check_ttl(ReceivedAt, Ttl) of
        valid -> do_send(Req, Options, Endpoint);
        expired ->
            declare_dead(Req, specific_ttl_expired)
    end.

do_send(#request{method = Method, body = Body, headers = Headers} = Req, Options, Endpoint) ->
    Resp = ibrowse:send_req(Endpoint, Headers, Method, Body),
    handle_response(Resp, Req, Options).

check_ttl(ReceivedAt, Ttl) ->
    Now = funl_uid:timestamp(),
    ExpireTime = ReceivedAt + (Ttl * 1000000),
    case ExpireTime < Now of
        true -> expired;
        _ -> valid
    end.

%%Ok
handle_response({ok, "200", _Head, _Body}, #request{method = Method, relative_url = RelativeUrl} = Req, _Options) ->
    io:format("[Handler] [Done] (~s)~s ~n", [Method, RelativeUrl]),
    {done, Req#request{state = done}};
%% max redirects
handle_response(_Resp, #request{redirect_count = RedirectCount, method = Method, relative_url = RelativeUrl} = Req, Options)
    when Req#request.redirect_count >= Options#options.max_redirects ->
    io:format("[to_many_redirects#~B] (~s)~s, will retry ~n", [RedirectCount, Method, RelativeUrl]),
    requeue(Req, Options);
%% redirect
handle_response({ok, StatusCode, Head, _Body}, Req, Opts) when "301" == StatusCode; "302" == StatusCode ->
    NewReq = Req#request{redirect_count = 1 + Req#request.redirect_count, state = redirecting},
    case (lists:keyfind("Location", 1, Head)) of
        false ->
            requeue(Req, Opts);
        {"Location", RedirectUrl} ->
            io:format("[Redirecting#~B] (~s)~s ... ~n ... to ~s ~n",
                [NewReq#request.redirect_count, NewReq#request.method, NewReq#request.relative_url, RedirectUrl]),
            send(NewReq, Opts, RedirectUrl),
            {done, Req}
    end;
%% http status code error (ex: 503)
handle_response({ok, StatusCode, _Head, _Body}, Req, #options{dead_status_codes = DeadStatusCodes} = Opts) ->
    case lists:member(StatusCode, DeadStatusCodes) of
        true-> declare_dead(Req, list_to_atom(StatusCode));
        false ->requeue(Req, Opts)
    end;
%% ibrowse errors
handle_response({error, Error}, Req, Opts) ->
    erlang:display(Error),
    requeue(Req, Opts).


requeue(#request{err_count = ErrCount} = Req, #options{max_errors = MaxErr}) when ErrCount == MaxErr ->
    declare_dead(Req, errCount);
requeue(Req, Options) ->
    NewErrCount = Req#request.err_count + 1,
    NewReq = Req#request{err_count = NewErrCount, redirect_count = 0, state = retrying},
    Delay = calculate_delay(NewReq, Options),
    funl_queue:enq(NewReq, funl_uid:timestamp() + Delay),
    
    io:format("[Handler] [Requed#~B] (~s)~s -> delay:~Bs ~n", [NewReq#request.err_count,
        NewReq#request.method, NewReq#request.relative_url, trunc(Delay / 1000000)]),
    {retrying, NewReq}.

declare_dead(#request{method = Method, relative_url = RelativeUrl} = Req, Reason) ->
    {Date, Time} = calendar:local_time(),
    {Year, Month, Day} = Date,
    {Hour, Min, Second} = Time,
    LogPath = lists:flatten(io_lib:fwrite("log/~B-~B-~B.dead.log", [Year, Month, Day])),
    ok = filelib:ensure_dir(LogPath),
    ok = file:write_file(LogPath, io_lib:fwrite("~B-~B-~B ~B:~B:~B ~p\n",
        [Year, Month, Day, Hour, Min, Second, Req]), [append]),
    io:format("[Handler] [~s] (~s)~s, declared dead! You can retreive it from ~s ~n", [Reason,
        Method, RelativeUrl, LogPath]),
    {dead, Req#request{state = dead}}.

calculate_delay(#request{err_count = ErrCount}, Options) ->
    Timeunit = 1000000,
    Random = rand:uniform(),
    Random2 = rand:uniform(),
    FinalRand = (Random - Random2) * Timeunit / 10,
    (Timeunit + FinalRand) * trunc(math:pow(Options#options.delay_factor, ErrCount)).
