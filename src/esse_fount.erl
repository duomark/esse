%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Fount of SSE connections.
%%%
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_fount).

-behaviour(cxy_fount).

%%% External API
-export([start_connection/1]).

%%% Behaviour API
-export([init/1, start_pid/2, send_msg/2]).

%%% Exported for spawn access
-export([connect/2]).

-spec start_connection(pid()) -> [pid()] | {error, any()}.

start_connection(Fount) ->
    cxy_fount:task_pid(Fount, done).

%% -spec init({}) -> {}.
%% -spec start_pid(cxy_fount:fount_ref(), {}) -> pid().
%% -spec send_msg(Worker, hexdump_cmd()) -> Worker when Worker :: worker().

init({}) -> {}.

start_pid(Fount, State) ->
    cxy_fount:spawn_worker(Fount, ?MODULE, connect, [Fount, State]).

send_msg(Worker, Msg) ->
    cxy_fount:send_msg(Worker, Msg).

connect(_Fount, {}) ->
    receive
        done -> ok
    end.
             
