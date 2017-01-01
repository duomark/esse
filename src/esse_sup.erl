%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Main supervisor controls the SSE process connection cxy_fount.
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
    Sse_Session_Mgr  = worker_child     ( esse_session_mgr,  start_link, []        ),
    Sse_Listener_Sup = supervisor_child ( esse_listener_sup, start_link, [9997, 3] ),
    Children         = [Sse_Session_Mgr, Sse_Listener_Sup],
    {ok, {rest_for_one_sup_options(5,1), Children} }.

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
