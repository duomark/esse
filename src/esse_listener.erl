%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Listener for SSE requests. A gen_server initializes with a single
%%%   listen socket, and spawns acceptors to service new connections.
%%%   The acceptors are limited to a maximum cap.
%%%
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          port                    :: non_neg_integer() | undefined,
          listen_socket           :: gen_tcp:socket()  | undefined,
          active_acceptors = 0    :: non_neg_integer(),
          max_acceptors    = 0    :: non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(non_neg_integer(), pos_integer()) -> {ok, pid()}.
start_link(Port, Max_Acceptors) ->
    Init_Args = {Port, Max_Acceptors},
    gen_server:start_link({local, ?SERVER}, ?MODULE, Init_Args, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

listen_options() ->
    [binary, {packet, raw}, {active, false}].

listen(Port) ->
    gen_tcp:listen(Port, listen_options()).

init({Port, Max_Acceptors}) ->
    LSock = listen(Port),
    {ok, #state{max_acceptors=Max_Acceptors, listen_socket=LSock, port=Port}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
