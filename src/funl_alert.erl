-module(funl_alert).
-include("../include/funl_options.hrl").

%% API
-export([check_max_queued_req/1]).

check_max_queued_req(#options{alert_queued_requests_max_count = MaxReqCount} = Options) ->
    Count = funl_mnesia_queue:count(),
    if Count > MaxReqCount ->
        Body = unicode:characters_to_binary(io_lib:format("Subject: Max queued requests alert!\r\n
                From: Funl alerts\r\n
                To: Admin \r\n\r\n
                Max Queued requests alert threshold (~B) exceded: ~B~n", [MaxReqCount, Count])),
        send_email(Body, Options);
        true -> ok
    end.

send_email(Body, #options{alert_email_receiver = AlertEmailReceiver, alert_email_relay = Relay,
    alert_email_username = Username, alert_email_password = Password, alert_email_ssl = Ssl}) ->
    
    gen_smtp_client:send({"funl@localhost", [AlertEmailReceiver], Body},
        [{relay, Relay}, {ssl, Ssl},
            {username, Username},
            {password, Password}],
        fun(A) ->
            erlang:display(A)
        end).
