%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Listener for SSE requests. Each gen_server instance is a single
%%%   accept socket waiting for a client connection. When the connection
%%%   ends, its supervisor will restart it as a new Listen Socket accepter.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%% API
-export([start_link/1, send_data_only/2, send_data_event/3, send_object_event/4]).

%% Internally spawned functions
-export([accept/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(el_state, {
          listen_socket                :: gen_tcp:socket(),
          num_bytes_sent = 0           :: pos_integer(),
          num_items_sent = 0           :: pos_integer(),
          num_pings_sent = 0           :: pos_integer(),
          accepter_pid                 :: pid()            | undefined,
          accepter_mref                :: reference()      | undefined,
          session_id     = undefined   :: uuid:uuid()      | undefined,
          stream_socket  = undefined   :: gen_tcp:socket() | undefined,
          start_stream   = undefined   :: pos_integer()    | undefined,
          stop_stream    = undefined   :: pos_integer()    | undefined,
          start_listen   = timestamp() :: pos_integer()
         }).

-type el_state() :: #el_state{}.

%%% Timestamp for start_listen and start_stream
timestamp() ->
    erlang:system_time(microsecond).

%%% Period at which ":" is sent to keep SSE stream active,
%%% Perturbed randomly to avoid thundering herds of timeouts.
keep_alive_time() ->
%%    timer:seconds(5).
    crypto:rand_uniform(timer:seconds(25), timer:seconds(35)).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link (gen_tcp:socket())        -> {ok, pid()}.
-spec accept     (gen_tcp:socket(), pid()) ->  ok | true.

start_link(Listen_Socket) ->
    gen_server:start_link(?MODULE, {Listen_Socket}, []).

accept(Listen_Socket, Esse_Listener) ->
    error_logger:info_msg("Starting to listen on socket ~p in ~p~n", [Listen_Socket, Esse_Listener]),
    case gen_tcp:accept(Listen_Socket) of
        {ok, Socket} ->
            ok = gen_tcp:controlling_process(Socket, Esse_Listener),
            ok = gen_server:cast(Esse_Listener, {new_client, Socket});
        {error, _Any} = Err ->
            error_logger:error_report([{socket_error, {?MODULE, Esse_Listener}, Err}]),
            exit(Esse_Listener, normal)
    end.


-spec send_data_only    (pid(),                                [sse_out:data()]) -> ok.
-spec send_data_event   (pid(),               sse_out:event(), [sse_out:data()]) -> ok.
-spec send_object_event (pid(), sse_out:id(), sse_out:event(), [sse_out:data()]) -> ok.

send_data_only    (Pid,            Data) -> gen_server:cast(Pid, {send_data_only,               Data}).
send_data_event   (Pid,     Event, Data) -> gen_server:cast(Pid, {send_data_event,       Event, Data}).
send_object_event (Pid, Id, Event, Data) -> gen_server:cast(Pid, {send_object_event, Id, Event, Data}).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init        ({gen_tcp:socket()})          -> {ok, el_state()}.
-spec code_change (string(), el_state(), any()) -> {ok, el_state()}.
-spec terminate   (atom(),   el_state())        ->  ok.

init({Listen_Socket}) ->
    {Pid, Ref} = spawn_monitor(?MODULE, accept, [Listen_Socket, self()]),
    {ok, #el_state{listen_socket=Listen_Socket, accepter_pid=Pid, accepter_mref=Ref}, keep_alive_time()}.

code_change (_OldVsn, State, _Extra)  -> {ok, State}.
terminate   ({error, closed}, _State) -> ok;
terminate   (normal, _State)          -> ok.
              

%%% All of handle_xxx end with reply/2 or noreply/1 to ensure keep_alive_time() always applies.
-type accept()   :: {accept, gen_tcp:socket()}.
-type cast_req() :: accept().

-type down()     :: {'DOWN', reference(), process, pid(), normal | noconnection | noproc}.
-type info_req() :: down() | timeout.

-type from()     :: {pid(), reference()}.
-type call_req() :: any().

-spec handle_info(info_req(),         el_state()) -> {noreply,        el_state()}.
-spec handle_cast(cast_req(),         el_state()) -> {noreply,        el_state()}.
-spec handle_call(call_req(), from(), el_state()) -> {reply,   any(), el_state()}.


%%% Timeout polling is used to send a ':' on an active stream to keep it alive.
handle_info(timeout, #el_state{start_stream=undefined} = State) -> noreply    (State);
handle_info(timeout,                       #el_state{} = State) -> keep_alive (State);  % Stops if TCP closed.

%%% Monitor 'DOWN' message arrives when the Socket Accepter terminates.
handle_info({'DOWN', MRef, process, MPid, normal},  #el_state{} = State) -> noreply(accepter_down(MRef, MPid, State));

%%% Ignore all other info requests else.
handle_info(Info, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected info ~p ignored~n", [Info]),
    noreply(State).


%%% When a client connects, the Socket is saved in the gen_server.
handle_cast ({new_client, Socket}, #el_state{stream_socket=undefined, start_stream=undefined} = State) -> start_stream(Socket, State);

%%% Other events are sent using sse_out formatting.
handle_cast ({send_data_only,               Data}, #el_state{} = State) -> send(State, esse_out:data_only    (           Data));
handle_cast ({send_data_event,       Event, Data}, #el_state{} = State) -> send(State, esse_out:data_event   (    Event, Data));
handle_cast ({send_object_event, Id, Event, Data}, #el_state{} = State) -> send(State, esse_out:object_event (Id, Event, Data));  

%%% Everything else is logged as unexpected.
handle_cast (Msg, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected cast ~p~n", [Msg]),
    noreply(State).


%%% All synchronous requests are ignored.
handle_call (Request, From, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected call ~p ignored from ~p~n", [Request, From]),
    reply({ignored, Request}, State).


%%%===================================================================
%%% Formatting el_state for display or logging
%%%===================================================================
-type run_state() :: normal | terminate.
-type pdict()     :: [{any(), any()}].   % Key/Value process dictionary as a list
-type status()    :: [{data, [{string(), {session_id, string()}}]}].
-spec format_status(status(), [pdict() | el_state()]) -> status().

format_status(_Run_State, [PDict0, #el_state{} = State0]) ->
    PDict = format_pdict(PDict0),
    State = format_state(State0),
    [{data, [{"State", State}] ++ PDict}].

format_pdict(PDict) ->
    case [Elem || Elem = {K,_V} <- PDict, K =/= '$initial_call', K =/= '$ancestors'] of
        []   -> [];
        Data -> [{"PDict", Data}]
    end.

format_state(#el_state{listen_socket=LS, start_listen=SLS,   accepter_pid=AP, accepter_mref=AM,
                       stream_socket=SS, start_stream=Start, stop_stream=Stop} = S) ->
    [
     {listen_socket,  format_listen_socket (LS, SLS, AP, AM)},
     {stream_socket,  format_stream_socket (SS, Start, Stop)},
     {session_id,     format_session_id    (S#el_state.session_id)},
     {num_bytes_sent, S#el_state.num_bytes_sent},  % Excluding pings
     {num_items_sent, S#el_state.num_items_sent},  % Count of successful send/2 calls
     {num_pings_sent, S#el_state.num_pings_sent}   % Assume 1 byte per ping
    ].

format_listen_socket(Socket, Start, undefined,  undefined)   -> {Socket, {started_listening, calendar_time(Start)}};
format_listen_socket(Socket, Start, Accept_Pid, Accept_Mref) -> {Socket, {started_listening, calendar_time(Start)},
                                                                 {accepter, {Accept_Pid, Accept_Mref}}}.

format_stream_socket(undefined, _,     _) -> none;
format_stream_socket(Socket, Start, Stop) -> {Socket, calendar_time(Start), calendar_time(Stop)}.
    
format_session_id(undefined) -> none;
format_session_id(Sid)       -> uuid:uuid_to_string(Sid).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Save the socket, send the HTTP/1.1 stream headers, then return the new state.
start_stream(Socket, #el_state{} = State) ->
    Session_Id = uuid:get_v4(),
    New_State = State#el_state{stream_socket=Socket, start_stream=timestamp(), session_id=Session_Id},
    error_logger:info_msg("Socket connected to ~p as new client ~p~n", [self(), New_State]),
    case esse_user_agent:receive_request(Socket) of
        {error, _Reason} = Err ->
            error_logger:error_msg("Error receiving: ~p~n", [Err]),
            noreply(State);
        #{} = Headers ->
            error_logger:info_msg("Got headers:~n~p~n", [Headers]),
            ok = gen_tcp:send(Socket, esse_out:response_headers(ok)),
            ets:insert_new(esse_sessions, {Session_Id, self()}),
            send(New_State, esse_out:data_event(<<"new_session_id">>, [uuid:uuid_to_string(Session_Id)]))
    end.

%%% Send a ':' on the socket stream, but stop if there is no client receiving or an error.
keep_alive(#el_state{} = State) -> send(State, <<":">>).

%%% Send an SSE event on the open Socket.
send(#el_state{stream_socket=undefined} = State, _Sse_Data) -> noreply(State);
send(#el_state{stream_socket=Socket}    = State,  Sse_Data) ->
    case gen_tcp:send(Socket, Sse_Data) of
        {error, timeout} -> close(Socket, timeout, State);
        {error, Reason}  -> close(Socket, Reason,  State);
        ok               -> noreply(accum_send_stats(Sse_Data, State))
    end.

accum_send_stats(<<":">>,  #el_state{num_pings_sent=NP} = State) ->
    State#el_state{num_pings_sent=NP+1};
accum_send_stats(Sse_Data, #el_state{num_items_sent=NI, num_bytes_sent=NB} = State) ->
    State#el_state{num_items_sent=NI+1, num_bytes_sent=NB+iolist_size(Sse_Data)}.

close(Socket, Reason, #el_state{session_id=Session_Id} = State0) ->
    State = State0#el_state{stop_stream=timestamp()},
    ets:delete(esse_sessions, Session_Id),
    ok = gen_tcp:close(Socket),
    error_logger:info_report([{termination_reason, Reason} | format_state(State)]),
    {stop, normal, State#el_state{session_id=undefined, stream_socket=undefined}}.

accepter_down(MRef, MPid, #el_state{accepter_mref=MRef, accepter_pid=MPid} = State) ->
    %% error_logger:info_msg("Accepter down ~p ~p", [MRef, MPid]),
    State#el_state{accepter_mref=undefined, accepter_pid=undefined}.

reply   (Reply, New_State) -> {reply, Reply, New_State, keep_alive_time()}.
noreply (       New_State) -> {noreply,      New_State, keep_alive_time()}.


%%%===================================================================
%%% Date hack copied from OTP docs and error_logger.
%%%===================================================================

%%% From erts erlang:timestamp/0 documentation
calendar_time(undefined)   -> "unknown";
calendar_time(System_Time) ->
    MegaSecs  = System_Time div 1000000000000,
    Secs      = System_Time div 1000000 - MegaSecs*1000000,
    MicroSecs = System_Time rem 1000000,
    Cal_Time  = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    display_date(Cal_Time, MicroSecs).

%%% From error_logger, but modified to RFC8601 (without using io:format)
display_date({{Y,Mo,D},{H,Mi,S}}, Micros) ->
    integer_to_list(Y) ++ "-" ++
        two_digits(Mo) ++ "-" ++
        two_digits(D)  ++ "T" ++
        two_digits(H)  ++ ":" ++
        two_digits(Mi) ++ ":" ++
        two_digits(S)  ++ "." ++
        six_digits(Micros) ++ "Z".

two_digits(N) when 0 =< N, N =< 9 ->
    [$0, $0 + N];
two_digits(N) ->
    integer_to_list(N).

six_digits(N) when      0 =< N, N =<     9 -> [$0, $0, $0, $0, $0,         $0 + N ];
six_digits(N) when     10 =< N, N =<    99 -> [$0, $0, $0, $0 | integer_to_list(N)];
six_digits(N) when    100 =< N, N =<   999 -> [$0, $0, $0     | integer_to_list(N)];
six_digits(N) when   1000 =< N, N =<  9999 -> [$0, $0         | integer_to_list(N)];
six_digits(N) when  10000 =< N, N =< 99999 -> [$0             | integer_to_list(N)];
six_digits(N)                              -> integer_to_list(N).

