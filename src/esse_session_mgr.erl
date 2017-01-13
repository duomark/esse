%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Owner of sse_sessions ets table, and initializer of epocxy
%%%   cxy_ctl limits for accepters and listener sessions.
%%%
%%% @since v0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_session_mgr).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(esm_state, {}).
-type state() :: #esm_state{}.


%%%===================================================================
%%% API
%%%===================================================================

start_link(Max_Sessions) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Max_Sessions}, []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init        ({non_neg_integer()})      -> {ok, state()}.
-spec code_change (string(), state(), any()) -> {ok, state()}.
-spec terminate   (atom(),   state())        ->  ok.

%%% Publicly named table containing record instances.
ets_options() ->
    [public, set, named_table, {keypos, 2}, {read_concurrency, true}].

%%% Cap on number of active sessions sending SSE events.
cxy_ctl_options(Max_Sessions) ->
    Session_Startup_Stat_Count = 100,
    Slow_Factor_Percentage     = 1000,
    [
     {esse_session, Max_Sessions, Session_Startup_Stat_Count, Slow_Factor_Percentage}
    ].

init({Max_Sessions}) ->
    true = cxy_ctl:init(cxy_ctl_options(Max_Sessions)),
    esse_sessions = ets:new(esse_sessions, ets_options()),
    {ok, #esm_state{}}.

code_change (_OldVsn,  State, _Extra) -> {ok, State}.
terminate   (_Reason, _State)         ->  ok.


-spec handle_info(any(),                       state()) -> {noreply,        state()}.
-spec handle_cast(any(),                       state()) -> {noreply,        state()}.
-spec handle_call(any(), {pid(), reference()}, state()) -> {reply,   any(), state()}.

handle_info(_Info,           #esm_state{} = State) -> {noreply,     State}.
handle_cast(_Msg,            #esm_state{} = State) -> {noreply,     State}.
handle_call(_Request, _From, #esm_state{} = State) -> {reply,   ok, State}.
