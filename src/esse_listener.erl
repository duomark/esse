%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Listener for SSE requests. Each gen_server instance is a single
%%%   accept socket waiting for a client connection. When the connection
%%%   ends, its supervisor will restart it as a new Listen Socket acceptor.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% Internally spawned functions
-export([accept/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(el_state, {
          listen_socket                :: gen_tcp:socket(),
          num_items_sent = 0           :: pos_integer(),
          accepter_pid                 :: pid()            | undefined,
          accepter_mref                :: reference()      | undefined,
          stream_socket  = undefined   :: gen_tcp:socket() | undefined,
          start_stream   = undefined   :: pos_integer()    | undefined,
          stop_stream    = undefined   :: pos_integer()    | undefined,
          start_listen   = timestamp() :: pos_integer()
         }).

-type el_state() :: #el_state{}.

%%% Timestamp for start_listen and start_stream
timestamp() ->
    erlang:system_time(milli_seconds).

%%% Period at which ":" is sent to keep SSE stream active,
%%% Perturbed randomly to avoid thundering herds of timeouts.
keep_alive_time() ->
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
            ok = report_error(Listen_Socket, Esse_Listener, Err),
            exit(Esse_Listener, {socket_error, Err})
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init        ({gen_tcp:socket()})          -> {ok, el_state()}.
-spec code_change (string(), el_state(), any()) -> {ok, el_state()}.
-spec terminate   (atom(),   el_state())        ->  ok.

init({Listen_Socket}) ->
    process_flag(trap_exit, true),
    {Pid, Ref} = spawn_monitor(?MODULE, accept, [Listen_Socket, self()]),
    {ok, #el_state{listen_socket=Listen_Socket, accepter_pid=Pid, accepter_mref=Ref}, keep_alive_time()}.

code_change (_OldVsn, State, _Extra) -> {ok, State}.

terminate   ({socket_error, _}, _State) -> ok;
terminate   (shutdown,          _State) -> ok.


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

%%% 'EXIT' message arrives when the Socket Accepter encounters and error waiting for an accept.
%%% Monitor 'DOWN' message arrives when the Socket Accepter terminates.
handle_info({'EXIT', From, {socket_error, _} = Err},#el_state{} = State) -> noreply(accepter_exit(From, Err,  State));
handle_info({'DOWN', MRef, process, MPid, normal},  #el_state{} = State) -> noreply(accepter_down(MRef, MPid, State));

%%% Ignore all other info requests else.
handle_info(Info, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected info ~p ignored~n", [Info]),
    noreply(State).


%%% When a client connects, the Socket is saved in the gen_server.
handle_cast ({new_client, Socket}, #el_state{stream_socket=undefined, start_stream=undefined} = State) ->
    noreply(start_stream(Socket, State));
handle_cast (Msg, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected cast ~p~n", [Msg]),
    noreply(State).


%%% All synchronous requests are ignored.
handle_call (Request, From, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected call ~p ignored from ~p~n", [Request, From]),
    reply({ignored, Request}, State).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Save the socket, send the HTTP/1.1 stream headers, then return the new state.
start_stream(Socket, #el_state{} = State) ->
    New_State = State#el_state{stream_socket=Socket, start_stream=timestamp()},
    error_logger:info_msg("Starting new client ~p~n", [New_State]),
    ok = gen_tcp:send(Socket, esse_out:response_headers(ok)),
    New_State.

%%% Send a ':' on the socket stream, but stop if there is no client receiving or an error.
keep_alive(#el_state{stream_socket=Socket} = State) ->
    case gen_tcp:send(Socket, <<":">>) of
        {error, timeout} -> close(Socket, timeout, State);
        {error, Reason}  -> close(Socket, Reason,  State);
        ok               -> noreply(State)
    end.

close(Socket, Reason, #el_state{num_items_sent=Num_Sent, start_stream=Start_Stream_Time} = State) ->
    ok = gen_tcp:close(Socket),
    Stop_Stream_Time = timestamp(),
    New_State = State#el_state{stream_socket=undefined, stop_stream=Stop_Stream_Time},
    Props = [{sse_socket,         Socket},
             {termination_reason, Reason},
             {start_time,         Start_Stream_Time},
             {stop_time,          Stop_Stream_Time},
             {num_items_sent,     Num_Sent}],
    {stop, Props, New_State}.
    
accepter_exit(MPid, {socket_error, _} = Err, #el_state{accepter_pid=MPid}) -> exit(Err).

accepter_down(MRef, MPid, #el_state{accepter_mref=MRef, accepter_pid=MPid} = State) ->
    State#el_state{accepter_mref=undefined, accepter_pid=undefined}.

reply   (Reply, New_State) -> {reply, Reply, New_State, keep_alive_time()}.
noreply (       New_State) -> {noreply,      New_State, keep_alive_time()}.
    
report_error(LS, Pid, {error, closed})       -> error_logger:error_msg("Listen socket ~p closed for ~p~n",       [LS, Pid]);
report_error(LS, Pid, {error, timeout})      -> error_logger:error_msg("Listen socket ~p timed out for ~p~n",    [LS, Pid]);
report_error(LS, Pid, {error, system_limit}) -> error_logger:error_msg("Listen socket ~p no ports for ~p~n",     [LS, Pid]);
report_error(LS, Pid, {error, Reason})       -> error_logger:error_msg("Listen socket ~p for ~p got error ~p~n", [LS, Pid, Reason]).
