-module(funl_consumer).
-include("../include/funl_options.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    options :: tuple(),
    timer :: any(),
    delay :: non_neg_integer()
}).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

init([Options]) ->
    Delay = calculate_delay(Options),
    io:format("Consumer delay: ~b milisecs~n", [Delay]),
    Timer = erlang:send_after(1, self(), consume),
    {ok, #state{options = Options, timer = Timer, delay = Delay}}.

handle_info(consume, #state{timer = Timer, delay = Delay, options = Options} = State) ->
    erlang:cancel_timer(Timer),
    case funl_queue:deq() of
        [] ->
            io:format("[Consumer] Queue empty~n"),
            ok;
        Req ->
            funl_request_handler:send(Req, Options)
    end,
    NewTimer = erlang:send_after(Delay, self(), consume),
    {noreply, State#state{timer = NewTimer}};
handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

calculate_delay(#options{backend_max_req = MaxReqS}) ->
    1000 div MaxReqS.