-module(funl_queue).
-include("../../include/funl_request.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1, enq/1, deq/1, peek/1, peek/2]).
-export([init/1,           %% gen_server callbacks
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-record(state, {}).

start_link(Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

enq(Elem) ->
    gen_server:call(?MODULE, {enq, Elem}).

peek(End) ->
    gen_server:call(?MODULE, {peek, End}).

peek(Start, End) ->
    gen_server:call(?MODULE, {peek, {Start, End}}).

deq(Id) ->
    gen_server:call(?MODULE, {deq, Id}).
update(Id, Elem) ->
    gen_server:call(?MODULE, {update, Id}).

init([Name]) ->
    {atomic, ok} = mnesia:create_table(request, [
        {ram_copies, [node()]},
        {attributes, record_info(fields, request)}]
    ),
    {ok, #state{}}.
%%funl_queue:peek(0).
handle_call({peek, End}, _From, State) ->
    T = fun() ->
        MatchHead = #request{next_retry = '$1', _ = '_'},
        Guard = {'<', '$1', End},
        Result = '$_',
        mnesia:select(request, [{MatchHead, [Guard], [Result]}])
        end,
    {atomic, Data} = mnesia:transaction(T),
    {reply, Data, State};
handle_call({peek, Start, End}, _From, State) ->
    T = fun() ->
        MatchHead = #request{next_retry = '$1', _ = '_'},
        Guard = {'<', '$1', End},
        Guard2 = {'>', '$1', Start},
        Result = '$_',
        mnesia:select(request, [{MatchHead, [Guard, Guard2], [Result]}])
        end,
    {atomic, Data} = mnesia:transaction(T),
    {reply, Data, State};
handle_call({enq, Elem}, _From, State) ->
    T = fun() ->
        mnesia:write(Elem)
        end,
    mnesia:transaction(T),
    {reply, sucess, State};
handle_call({deq, Id}, _From, State) ->
    T = fun() ->
        mnesia:delete({request, Id})
        end,
    mnesia:transaction(T),
    {reply, sucess, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

