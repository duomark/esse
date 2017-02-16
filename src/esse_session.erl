%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   An esse_session is a gen_server holding a live socket to a connected
%%%   client. It is spawned as a result of an accept from a esse_listener.
%%%
%%% @since v0.2.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_session).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%%% SSE relay to client external API
-export([send_data_only/2, send_data_event/3, send_seq_event/4, send_retry_freq/2]).

%% Internally exported API, not intended to be called by custom application
-export([start_link/0, new_client/2, get_pid_for_session/1, get_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-type peername       () :: {inet:ip_address(), inet:port_number()}.
-type session_id     () :: uuid:uuid().

-record(session, {
          id       :: session_id(),
          pid      :: pid(),
          peername :: peername()
         }).
          
-record(es_state, {
          num_bytes_sent = 0           :: non_neg_integer(),
          num_items_sent = 0           :: non_neg_integer(),
          num_pings_sent = 0           :: non_neg_integer(),
          peername       = undefined   :: peername()         | undefined,
          session_id     = undefined   :: session_id()       | undefined,
          stream_socket  = undefined   :: gen_tcp:socket()   | undefined,
          start_stream   = undefined   :: pos_integer()      | undefined,
          stop_stream    = undefined   :: pos_integer()      | undefined
         }).

-type state() :: #es_state{}.

%%% Period at which ":" is sent to keep SSE stream active,
%%% Perturbed randomly to avoid thundering herds of timeouts.
keep_alive_time() ->
%%    timer:seconds(5).
    crypto:rand_uniform(timer:seconds(25), timer:seconds(35)).


%%%===================================================================
%%% External relay to client API
%%%===================================================================

-type active_id() :: pid() | session_id() | nonempty_string().
-spec send_data_only  (active_id(),                                  [esse_out:data()]) -> ok.
-spec send_data_event (active_id(),                esse_out:event(), [esse_out:data()]) -> ok.
-spec send_seq_event  (active_id(), esse_out:id(), esse_out:event(), [esse_out:data()]) -> ok.

%%% Transmit default 'message' events using a list of data lines.
send_data_only(Pid, Data) when is_pid(Pid) -> gen_server:cast(Pid,    {send_data_only, Data});
send_data_only(Sid, Data)                  -> send_data_only(get_pid_for_session(Sid), Data).

%%% Transmit an event with a list of data lines.
send_data_event(Pid, Event, Data) when is_pid(Pid) -> gen_server:cast(Pid,    {send_data_event, Event, Data});
send_data_event(Sid, Event, Data)                  -> send_data_event(get_pid_for_session(Sid), Event, Data).

%%% Transmit an event + data, with a unique identifier for the event.
send_seq_event(Pid, Id, Event, Data) when is_pid(Pid) -> gen_server:cast(Pid,    {send_seq_event, Id, Event, Data});
send_seq_event(Sid, Id, Event, Data)                  -> send_seq_event(get_pid_for_session(Sid), Id, Event, Data).


%%% Tell client to change reconnect frequency.
-spec send_retry_freq(active_id(), esse_out:millis()) -> ok.

send_retry_freq(Pid, Millis) when is_pid(Pid) -> gen_server:cast(Pid, {send_retry_freq, Millis});
send_retry_freq(Sid, Millis)                  -> send_retry_freq(get_pid_for_session(Sid), Millis).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link()                        -> {ok, pid()}.
-spec new_client(pid(), gen_tcp:socket()) ->  ok.

start_link() ->
    gen_server:start_link(?MODULE, {}, []).

new_client(Pid, Session_Socket) ->
    gen_server:cast(Pid, {new_client, Session_Socket}).
  

%%% Find the corresponding client process channel for a given client session_id.
-spec get_pid_for_session(session_id() | nonempty_string()) -> pid().

get_pid_for_session(Session_Id) when is_list(Session_Id) ->
    get_pid_for_session(uuid:string_to_uuid(Session_Id));
get_pid_for_session(Session_Id) when is_binary(Session_Id) ->
    ets:lookup_element(esse_sessions, Session_Id, #session.pid).


%%% Report the internal status of the client connection worker (for debugging).
-spec get_status(active_id()) -> proplists:proplist().

get_status(Pid) when is_pid(Pid) -> gen_server:call(Pid, get_status);
get_status(Sid)                  -> get_status(get_pid_for_session(Sid)).
   

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({}) -> {ok, state(), pos_integer()}.

init({}) ->
    {ok, #es_state{}, keep_alive_time()}.


-spec code_change (string(), state(), any()) -> {ok, state()}.
-spec terminate   (atom(),   state())        ->  ok.

code_change (_OldVsn, State, _Extra)  -> {ok, State}.
terminate   ({error, closed}, _State) ->  ok;
terminate   (normal, _State)          ->  ok.
              

%%% All of handle_xxx end with reply/2 or noreply/1 to ensure keep_alive_time() always applies.
-type send_req()   :: {send_data_only,                      [binary()]}
                    | {send_data_event,           binary(), [binary()]}
                    | {send_seq_event,  binary(), binary(), [binary()]}
                    | {send_retry_freq, esse_out:millis()}.
-type new_client() :: {new_client, gen_tcp:socket()}.
-type cast_req()   ::  new_client() | send_req().

-type info_req()   ::  timeout.

-type from()       :: {pid(), reference()}.
-type call_req()   ::  get_status | any().

-spec handle_info(info_req(),         state()) -> {noreply,                     state(), pos_integer()}.
-spec handle_cast(cast_req(),         state()) -> {noreply,                     state(), pos_integer()} | {stop, any(), state()}.
-spec handle_call(call_req(), from(), state()) -> {reply, proplists:proplist(), state(), pos_integer()}
                                                | {reply, {ignored, any()},     state(), pos_integer()}.


%%% Timeout polling is used to send a ':' on an active stream to keep it alive.
handle_info(timeout, #es_state{start_stream=undefined} = State) -> noreply    (State);
handle_info(timeout,                       #es_state{} = State) -> keep_alive (State);  % Stops if TCP closed.

%%% Ignore all other info requests else.
handle_info(Info, #es_state{} = State) ->
    error_logger:warning_msg("Unexpected info ~p ignored~n", [Info]),
    noreply(State).


%%% The initial startup has to cast the Socket after changing the controlling process.
handle_cast ({new_client, Socket}, #es_state{stream_socket=undefined, start_stream=undefined}) -> start_stream(Socket);

%%% Other events are sent using esse_out formatting.
handle_cast ({send_data_only,             Data}, #es_state{} = State) -> send(State, esse_out:data_only      (           Data));
handle_cast ({send_data_event,     Event, Data}, #es_state{} = State) -> send(State, esse_out:data_event     (    Event, Data));
handle_cast ({send_seq_event,  Id, Event, Data}, #es_state{} = State) -> send(State, esse_out:sequence_event (Id, Event, Data));  
handle_cast ({send_retry_freq, Millis},          #es_state{} = State) -> send(State, esse_out:retry(Millis));

%%% Everything else is logged as unexpected.
handle_cast (Msg, #es_state{} = State) ->
    error_logger:warning_msg("Unexpected cast ~p~n", [Msg]),
    noreply(State).


%%% Summary status can be obtained by session_id.
handle_call (get_status, _From, #es_state{} = State) ->
    reply({status, format_state(State)}, State);

%%% All other synchronous requests are ignored.
handle_call (Request, From, #es_state{} = State) ->
    error_logger:warning_msg("Unexpected call ~p ignored from ~p~n", [Request, From]),
    reply({ignored, Request}, State).


%%%===================================================================
%%% Formatting el_state for display or logging
%%%===================================================================
-type run_state() :: normal | terminate.
-type pdict()     :: [{any(), any()}].   % Key/Value process dictionary as a list
-type status()    :: [{data, [{string(), proplists:proplist()}]}].
-spec format_status(run_state(), [pdict() | state()]) -> status().

format_status(_Run_State, [PDict0, #es_state{} = State0]) ->
    PDict = format_pdict(PDict0),
    State = format_state(State0),
    [{data, [{"State", State}] ++ PDict}].

format_pdict(PDict) ->
    case [Elem || Elem = {K,_V} <- PDict, K =/= '$initial_call', K =/= '$ancestors'] of
        []   -> [];
        Data -> [{"PDict", Data}]
    end.

format_state(#es_state{stream_socket=SS, start_stream=Start, stop_stream=Stop} = S) ->
    [
     {stream_socket,  format_stream_socket (SS, Start, Stop)},
     {session_id,     format_session_id    (S#es_state.session_id)},
     {peername,       S#es_state.peername},
     {num_bytes_sent, S#es_state.num_bytes_sent},  % Excluding pings
     {num_items_sent, S#es_state.num_items_sent},  % Count of successful send/2 calls
     {num_pings_sent, S#es_state.num_pings_sent}   % Assume 1 byte per ping
    ].

format_stream_socket(undefined,     _,         _) -> none;
format_stream_socket(Socket,    Start, undefined) -> {Socket, esse_time:calendar_time(Start), "active"};
format_stream_socket(Socket,    Start,      Stop) -> {Socket, esse_time:calendar_time(Start), esse_time:calendar_time(Stop)}.

format_session_id(undefined) -> none;
format_session_id(Sid)       -> uuid:uuid_to_string(Sid).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Save the socket, send the HTTP/1.1 stream headers, then return the new state.
start_stream(Socket) ->
    Session_Id     = uuid:get_v4(),
    {ok, Peername} = inet:peername(Socket),
    Start_Time     = esse_time:timestamp(),
    New_State = #es_state{stream_socket=Socket,  start_stream=Start_Time,
                          session_id=Session_Id, peername=Peername},
    error_logger:info_msg("New session ~s connected to ~p from ~p at ~p",
                          [format_session_id(Session_Id), self(),
                           Peername, esse_time:calendar_time(Start_Time)]),
    case esse_user_agent:receive_request(Socket) of
        {error, _Reason} = Err ->
            error_logger:error_msg("Error receiving: ~p~n", [Err]),
            close(Socket, Err, New_State);
        #{} = Headers ->
            error_logger:info_msg("Got headers:~n~p~n", [Headers]),
            ok = gen_tcp:send(Socket, esse_out:response_headers(ok)),
            Session_Rec = #session{id=Session_Id, pid=self(), peername=Peername},
            ets:insert_new(esse_sessions, Session_Rec),
            send(New_State, notify_session_id_event(Session_Id))
    end.

notify_session_id_event(Session_Id) ->
    [
     esse_out:retry(esse_env:get_retry_frequency()),
     esse_out:data_event(<<"new_session_id">>, [format_session_id(Session_Id)])
    ].

%%% Send a ':' on the socket stream, but stop if there is no client receiving or an error.
keep_alive(#es_state{} = State) -> send(State, <<":">>).

%%% Send an SSE event on the open Socket.
send(#es_state{stream_socket=undefined} = State, _Sse_Data) -> noreply(State);
send(#es_state{stream_socket=Socket}    = State,  Sse_Data) ->
    case gen_tcp:send(Socket, Sse_Data) of
        {error, timeout} -> close(Socket, timeout, State);
        {error, Reason}  -> close(Socket, Reason,  State);
        ok               -> noreply(accum_send_stats(Sse_Data, State))
    end.

accum_send_stats(<<":">>,  #es_state{num_pings_sent=NP} = State) ->
    State#es_state{num_pings_sent=NP+1};
accum_send_stats(Sse_Data, #es_state{num_items_sent=NI, num_bytes_sent=NB} = State) ->
    State#es_state{num_items_sent=NI+1, num_bytes_sent=NB+iolist_size(Sse_Data)}.

close(Socket, Reason, #es_state{session_id=Session_Id} = State0) ->
    State = State0#es_state{stop_stream=esse_time:timestamp()},
    ets:delete(esse_sessions, Session_Id),
    ok = gen_tcp:close(Socket),
    error_logger:info_report([{termination_reason, Reason} | format_state(State)]),
    Cleared_State = State#es_state{session_id=undefined, stream_socket=undefined, peername=undefined},
    {stop, normal, Cleared_State}.

reply   (Reply, New_State) -> {reply, Reply, New_State, keep_alive_time()}.
noreply (       New_State) -> {noreply,      New_State, keep_alive_time()}.
