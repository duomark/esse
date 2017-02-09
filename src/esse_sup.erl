%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   The root supervisor controls the application and relies on rest_for_one
%%%   strategies to ensure that ets tables are built and exist before they
%%%   are used.
%%%
%%%   The session manager creates and owns the 'esse_sessions' ets table,
%%%   and initializes the epocxy cxy_ctl limits for number of active sessions.
%%%
%%%   The esse_listener_sup manages the listen socket and accepters waiting
%%%   for client connections. The accepters are simple_one_for_one workers
%%%   sharing a single Listen Socket. They each launch a process which blocks
%%%   until a client connects, creating a new active session if the limit on
%%%   number of active sessions has not been exceeded. If the limit has been
%%%   exceeded, it responds with Server Busy retry later.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_sup).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(supervisor).

%%% External API
-export([start_link/0]).

%%% Internal API
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> supervisor:startchild_ret().

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init({}) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({}) ->
    Sse_Session_Mgr  = worker_child     (esse_session_mgr,  start_link, []),
    Sse_Session_Sup  = supervisor_child (esse_session_sup,  start_link, []),
    Sse_Listener_Sup = supervisor_child (esse_listener_sup, start_link, sse_listener_args()),

    Children = [Sse_Session_Mgr, Sse_Session_Sup, Sse_Listener_Sup],
    {ok, {rest_for_one_sup_options(5,1), Children} }.

sse_listener_args () -> [esse_env:get_sse_port(), esse_env:get_max_accepters()].

rest_for_one_sup_options(Intensity, Period) ->
   #{
      strategy  => rest_for_one,
      intensity => Intensity,     % Num failures allowed,
      period    => Period         % Within this many seconds
    }.

supervisor_child(Mod, Fun, Args) ->
    #{
       id      =>  Mod,
       start   => {Mod, Fun, Args},
       type    =>  supervisor,
       modules => [Mod]
     }.

worker_child(Mod, Fun, Args) ->
    #{
       id      =>  Mod,
       start   => {Mod, Fun, Args},
       restart =>  permanent,
       type    =>  worker,
       modules => [Mod]
     }.
