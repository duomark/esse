%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Simple_one_for_one supervisor which launches as many transient
%%%   child client SSE acceptor workers as is specified by the Max
%%%   Acceptors. Any worker that does not end with 'normal' or
%%%   'shutdown' will be relaunched in the initial state accepting
%%%   connections.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_listener_sup).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(supervisor).

%%% External API
-export([start_link/2, start_child/1, terminate_child/1]).

%%% Internal API
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link  (non_neg_integer(), pos_integer()) -> supervisor:startchild_ret().
-spec terminate_child (pid())                        -> ok.

start_link(Port, Max_Acceptors) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, {Port}),
    _ = [start_child(Pid) || _ <- lists:seq(1, Max_Acceptors)],
    {ok, Pid}.
    
start_child(Pid) ->
    supervisor:start_child(Pid, []).

terminate_child(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%% Send timeout is < 5 seconds to avoid gen_server crashing.
listen_options() ->
    [binary, {packet, raw}, {active, false},
     {reuseaddr, true},     {send_timeout, timer:seconds(3)}].

listen(Port) ->
    {ok, Listen_Socket} = gen_tcp:listen(Port, listen_options()),
    Listen_Socket.

-spec init({port(), pos_integer()})
          -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({Port}) ->
    Listen_Socket = listen(Port),
    LMF_Args = {esse_listener, start_link, [Listen_Socket]},
    Listener = worker_child(esse_listener, LMF_Args),
    {ok, {simple_one_for_one_sup_options(5,1), [Listener]} }.

simple_one_for_one_sup_options(Intensity, Period) ->
   #{
      strategy  => simple_one_for_one,
      intensity => Intensity,     % Num failures allowed,
      period    => Period         % Within this many seconds
    }.

worker_child(Id, {_M, _F, _A} = Start) ->
    #{
       id      =>  Id,
       start   =>  Start,
       restart =>  transient,
       type    =>  worker,
       modules => [_M]
     }.
