%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Main supervisor controls the SSE process connection cxy_fount.
%%%
%%% @since v0.0.1
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
%    FMFA = {cxy_fount_sup,     start_link, [Mod, []]},
%    LMFA = {esse_listener_sup, start_link, []},
%    Sse_Fount_Sup    = supervisor_child(sse_fount_sup,     FMFA),
%    Sse_Listener_Sup = supervisor_child(esse_listener_sup, LMFA),

    Children = [
%                Sse_Fount_Sup,
%                Sse_Listener_Sup
               ],
    {ok, { rest_for_one_sup_options(1,5), Children} }.

rest_for_one_sup_options(Intensity, Period) ->
   #{
      strategy  => rest_for_one,
      intensity => Intensity,     % Num failures allowed,
      period    => Period         % Within this many seconds
    }.

supervisor_child(Id, {_M, _F, _A} = Start) ->
    #{
       id    => Id,
       start => Start,
       type  => supervisor
     }.

%% worker_child(Id, {_M, _F, _A} = Start) ->
%%     #{
%%        id    => Id,
%%        start => Start,
%%        type  => worker
%%      }.
