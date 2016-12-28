-module(funl_alert).
-include("../include/funl_options.hrl").

%% API
-export([check_max_queued_req/1, alert_dead_req/3]).

check_max_queued_req(#options{alert_queued_requests_max_count = MaxReqCount} = Options) ->
    Count = funl_mnesia_queue:count(),
    if Count > MaxReqCount ->
        Body = unicode:characters_to_binary(io_lib:format("Subject: Max queued requests alert!\r\n
                From: Funl alerts\r\n
                To: Admin \r\n\r\n
                Max Queued requests alert threshold (~B) exceded: ~B~n", [MaxReqCount, Count])),
        ok = send_email(Body, Options);
        true -> ok
    end.

alert_dead_req({Reason, Method, RelativeUrl}, LogPath, Options) ->
    Body = unicode:characters_to_binary(io_lib:format("Subject: Dead request!\r\n
                From: Funl alerts\r\n
                To: Admin \r\n\r\n
                (~s)~s, declared dead for reason ~s! You can retreive it from ~s ~n", [Method, RelativeUrl, Reason, LogPath])),
    ok = send_email(Body, Options).


send_email(_, #options{alert_email_receiver = none}) ->
    io:format("Alert sending skipped, no alert email provided");
send_email(Body, #options{alert_email_receiver = AlertEmailReceiver, alert_email_relay = Relay,
    alert_email_username = Username, alert_email_password = Password, alert_email_ssl = Ssl}) ->
    
    gen_smtp_client:send({"funl@localhost", [AlertEmailReceiver], Body},
        [{relay, Relay}, {ssl, Ssl},
            {username, Username},
            {password, Password}],
        fun(A) ->
            erlang:display(A)
        end),
    ok.
