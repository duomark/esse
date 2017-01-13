%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Listener for SSE client connect requests. Each gen_server instance
%%%   monitors a single accept socket waiting for a client connection.
%%%   When the connection ends, its supervisor will restart it as a new
%%%   Listen Socket accepter.
%%%
%%%   A new connection is spawned using cxy_ctl limits, as a new esse_session.
%%%   This ensures a static number of esse_listener instances determined at
%%%   initialization, but a variable number of esse_stream instances without
%%%   exceeding the maximum number of active sessions. If a connection is
%%%   requested when the server is already running the maximum number of
%%%   active connections, a 503 Service Unavailable response is sent.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%% API
-export([start_link/1, get_status/1]).


%% Internally spawned functions
-export([accept/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-record(el_state, {
          listen_socket  :: gen_tcp:socket(),
          accepter_pid   :: pid()             | undefined,
          accepter_mref  :: reference()       | undefined,
          start_listen   = esse_time:timestamp() :: pos_integer()
         }).

-type state() :: #el_state{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link (gen_tcp:socket())        -> {ok, pid()}.
-spec accept     (gen_tcp:socket(), pid()) ->  ok | no_return().

%%% Inherit shared Listen_Socket on each instance of listen worker.
start_link(Listen_Socket) ->
    gen_server:start_link(?MODULE, {Listen_Socket}, []).

%%% One accept per worker, spawn_monitor on accept to avoid blocking
%%% TODO: Change to trap_exit and link, so if Listener down, accepters die.
accept(Listen_Socket, Esse_Listener) ->
    error_logger:info_msg("Starting to listen on socket ~p in ~p~n",
                          [Listen_Socket, Esse_Listener]),
    case gen_tcp:accept(Listen_Socket) of
        {ok, Socket} ->
            ok = maybe_launch_stream(Socket, Esse_Listener);
        {error, _Any} = Err ->
            Msg = [{socket_error, {?MODULE, Esse_Listener}, Err}],
            error_logger:error_report(Msg),
            exit(Esse_Listener, normal)
    end.


%%% Report the internal status of the listener
-spec get_status(pid()) -> proplists:proplist().

get_status(Pid) ->
    gen_server:call(Pid, get_status).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init        ({gen_tcp:socket()})       -> {ok, state()}.
-spec code_change (string(), state(), any()) -> {ok, state()}.
-spec terminate   (atom(),   state())        ->  ok.

init({Listen_Socket}) ->
    {Pid, Ref} = spawn_monitor(?MODULE, accept, [Listen_Socket, self()]),
    {ok, #el_state{listen_socket=Listen_Socket, accepter_pid=Pid, accepter_mref=Ref}}.

code_change (_OldVsn, State, _Extra)  -> {ok, State}.
terminate   ({error, closed}, _State) -> ok;
terminate   (normal, _State)          -> ok.
              

%%% Handler functions
-type from()     :: {pid(),  reference()}.
-type down()     :: {'DOWN', reference(), process, pid(), normal | noconnection | noproc}.

-spec handle_info(down(),        state()) -> {noreply, state()}.
-spec handle_cast(any(),         state()) -> {noreply, state()}.
-spec handle_call(any(), from(), state())
                 -> {reply, proplists:proplist(), state()}
                  | {reply, {ignored, any()},     state()}.

%%% Monitor 'DOWN' message arrives when the Socket Accepter terminates.
handle_info({'DOWN', MRef, process, MPid, normal},  #el_state{} = State) ->
    {noreply, accepter_down(MRef, MPid, State)};

%%% Ignore all other info requests else.
handle_info(Info, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected info ~p ignored~n", [Info]),
    {noreply, State}.


%%% Cast requests are logged as unexpected.
handle_cast (Msg, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected cast ~p~n", [Msg]),
    {noreply, State}.


%%% Summary status can be obtained by session_id.
handle_call (get_status, _From, #el_state{} = State) ->
    {reply, {status, format_state(State)}, State};

%%% All other synchronous requests are ignored.
handle_call (Request, From, #el_state{} = State) ->
    error_logger:warning_msg("Unexpected call ~p ignored from ~p~n", [Request, From]),
    {reply, {ignored, Request}, State}.


%%%===================================================================
%%% Formatting el_state for display or logging
%%%===================================================================
-type run_state() :: normal | terminate.
-type pdict()     :: [{any(), any()}].   % Key/Value process dictionary as a list
-type status()    :: [{data, [{string(), proplists:proplist()}]}].
-spec format_status(run_state(), [pdict() | state()]) -> status().

format_status(_Run_State, [PDict0, #el_state{} = State0]) ->
    PDict = format_pdict(PDict0),
    State = format_state(State0),
    [{data, [{"State", State}] ++ PDict}].

format_pdict(PDict) ->
    case [Elem || Elem = {K,_V} <- PDict, K =/= '$initial_call', K =/= '$ancestors'] of
        []   -> [];
        Data -> [{"PDict", Data}]
    end.

format_state(#el_state{listen_socket=LS, start_listen=SLS,
                       accepter_pid=AP, accepter_mref=AM}) ->
    [{listen_socket, format_listen_socket (LS, SLS, AP, AM)}].

format_listen_socket(Socket, Start, undefined,  undefined)   ->
    {Socket, esse_time:calendar_time(Start)};
format_listen_socket(Socket, Start, Accept_Pid, Accept_Mref) ->
    {Socket, esse_time:calendar_time(Start), format_accepter(Accept_Pid, Accept_Mref)}.

format_accepter(Pid, Mref) -> {accepter, {Pid, Mref}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Attempt to spawn a new stream, others server is too busy.
maybe_launch_stream(Socket, Listener) ->
    case cxy_ctl:maybe_execute_pid_link(esse_session, esse_session_sup, start_child, []) of
        {max_pids, _Max}     -> unavailable(Socket, Listener);
        Pid when is_pid(Pid) -> true = unlink(Pid),
                                ok = gen_tcp:controlling_process(Socket, Pid),
                                esse_session:new_client(Pid, Socket)
    end.

unavailable(Socket, Listener) ->
    case gen_tcp:send(Socket, esse_out:response_headers(service_unavailable)) of
        {error, timeout} -> close(Socket, timeout, Listener);
        {error, Reason}  -> close(Socket, Reason,  Listener);
        ok               -> ok
    end.

close(Socket, _Reason, Listener) ->
    ok = gen_tcp:close(Socket),
    exit(Listener, normal).

accepter_down(MRef, MPid, #el_state{accepter_mref=MRef, accepter_pid=MPid} = State) ->
    %% error_logger:info_msg("Accepter down ~p ~p", [MRef, MPid]),
    State#el_state{accepter_mref=undefined, accepter_pid=undefined}.

