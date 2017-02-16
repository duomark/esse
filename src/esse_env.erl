%%%------------------------------------------------------------------------------
%%% @copyright (c) 2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Environment configuration variables, plus access functions with
%%%   default values supplied. The file rel/sys.config contains the
%%%   actual runtime configured values for production.
%%%
%%% @since v0.2.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_env).
-author('Jay Nelson <jay@duomark.com>').

%% External API
-export([get_max_acceptors/0,
         get_max_sessions/0,
         get_retry_frequency/0,
         get_sse_port/0,
         get_sse_send_timeout/0]).


%%%===================================================================
%%% API functions
%%%===================================================================
-spec get_max_acceptors    () -> pos_integer().
-spec get_max_sessions     () -> pos_integer().
-spec get_retry_frequency  () -> sse_out:millis().
-spec get_sse_port         () -> pos_integer().
-spec get_sse_send_timeout () -> pos_integer().

get_max_acceptors    () -> get_app_env(max_acceptors,        2).
get_max_sessions     () -> get_app_env(max_sessions,         2).
get_retry_frequency  () -> get_app_env(retry_frequency,  30000).
get_sse_port         () -> get_app_env(sse_port,          9997).
get_sse_send_timeout () -> get_app_env(sse_send_timeout,  3000).


%%%===================================================================
%%% Support functions
%%%===================================================================
-spec get_app_env(atom(), any()) -> any().

get_app_env(Param, Default) ->
    case application:get_env(esse, Param) of
        {ok, Val} -> Val;
        undefined ->
            case init:get_argument(Param) of
                {ok, [[FirstVal | _OtherVals] | _MoreVals]} -> FirstVal;
                error -> Default
            end
    end.
