%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Accepter waiting for clients to connect to the Listen Socket.
%%%   An esse_listener is monitoring the accepter, and will die and
%%%   relaunch when the accepter goes down so that it can start a new
%%%   Listen Socket accepter.
%%% @since v0.2.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listen_accepter).
-author('Jay Nelson <jay@duomark.com>').

-export([accept/2]).


%%%===================================================================
%%% API
%%%===================================================================
-spec accept(gen_tcp:socket(), pid()) ->  ok | no_return().

%%% Accept is spawned to wait for client, so gen_server calls still handled.
accept(Listen_Socket, Esse_Listener) ->
    Info_Msg = "Starting to listen on socket ~p in ~p~n",
    error_logger:info_msg(Info_Msg, [Listen_Socket, Esse_Listener]),

    %% Block here waiting for a client to connect
    case gen_tcp:accept(Listen_Socket) of
        {error, _Any} = Err ->
            Msg = [{socket_error, {?MODULE, Esse_Listener}, Err}],
            error_logger:error_report(Msg);
        {ok, Socket} ->
            Stream_Result = maybe_launch_stream(Socket, Esse_Listener),
            monitor_stream(Esse_Listener, Stream_Result)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Wait for a Session pid to stop before deducting from active sessions.
monitor_stream(_Listener,  no_stream             ) -> no_stream;
monitor_stream( Listener, {ok, Session_Pid, Mref}) ->
    exit(Listener, done),
    error_logger:error_msg("Exited listener ~p, waiting for Session ~p to end~n",
                           [Listener, Session_Pid]),
    receive
        {'DOWN', Mref, process, Session_Pid, _Reason} ->
            ets:update_counter(esse_sessions, active_sessions, {3, -1})
    end.

%%% Spawn a new stream if server is not too busy.
maybe_launch_stream(Socket, Listener) ->
    %% ets not incremented if too many sessions running...
    %% otherwise, monitor to be able to deduct from active sessions.
    case esse_session_sup:start_child(self()) of
        {error, server_busy} -> unavailable (Socket, Listener);
        {error, Other}       -> problem     (Socket, Listener, Other);
        {ok, Session_Pid}    -> case transfer_socket_to_session_pid(Socket, Listener) of
                                    session_transfer_timeout ->
                                        no_stream;
                                    ok ->
                                        Mref = erlang:monitor(process, Session_Pid),
                                        {ok, Session_Pid, Mref}
                                end
    end.

%%% Get the Session_Pid in a message from the execute_task call.
transfer_socket_to_session_pid(Socket, Listener) ->
    receive
        {esse_session_sup, Session_Pid} ->
            ok = gen_tcp:controlling_process(Socket, Session_Pid),
            ok = esse_session:new_client(Session_Pid, Socket)
    after 3000 -> exit(Listener, session_transfer_timeout),
                  session_transfer_timeout
    end.

unavailable(Socket, Listener) ->
    Busy_Response = esse_out:response_headers(service_unavailable),
    case gen_tcp:send(Socket, Busy_Response) of
        {error, timeout} -> close(Socket, timeout,     Listener);
        {error, Reason}  -> close(Socket, Reason,      Listener);
        ok               -> close(Socket, server_busy, Listener)
    end.

problem(Socket, Listener, Other) ->
    Msg = [{socket_error, {?MODULE, Listener}, {socket, Socket}, {error, Other}}],
    error_logger:error_report(Msg),
    unavailable(Socket, Listener).

close(Socket, Reason, Listener) ->
    ok = gen_tcp:close(Socket),
    exit(Listener, Reason),
    no_stream.
