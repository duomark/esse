%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Supervisor one gen_server to manage a listener socket for new
%%%   client SSE connections.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener_sup).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(supervisor).

%%% External API
-export([start_link/2]).

%%% Internal API
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link(non_neg_integer(), pos_integer()) -> supervisor:startchild_ret().

start_link(Port, Max_Acceptors) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {Port, Max_Acceptors}).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec init({port(), pos_integer()})
          -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init({Port, Max_Acceptors}) ->
    LMF_Args = {esse_listener, start_link, [Port, Max_Acceptors]},
    Listener = supervisor_child(esse_listener, LMF_Args),
    {ok, { one_for_one_sup_options(1,5), [Listener]} }.

one_for_one_sup_options(Intensity, Period) ->
   #{
      strategy  => one_for_one,
      intensity => Intensity,     % Num failures allowed,
      period    => Period         % Within this many seconds
    }.

supervisor_child(Id, {_M, _F, _A} = Start) ->
    #{
       id    => Id,
       start => Start,
       type  => supervisor
     }.
