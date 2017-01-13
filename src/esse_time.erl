%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Utility functions to support timing and timestamps.
%%% @since v0.2.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_time).
-author('Jay Nelson <jay@duomark.com>').

-export([timestamp/0, calendar_time/1]).

-spec timestamp() -> integer().
-spec calendar_time(undefined | integer()) -> nonempty_string().

%%% Timestamp for start_listen and start_stream
timestamp() ->
    erlang:system_time(microsecond).

%%%===================================================================
%%% Date hack copied from OTP docs and error_logger.
%%%===================================================================

%%% From erts erlang:timestamp/0 documentation
calendar_time(undefined)   -> "unknown";
calendar_time(System_Time) ->
    MegaSecs  = System_Time div 1000000000000,
    Secs      = System_Time div 1000000 - MegaSecs*1000000,
    MicroSecs = System_Time rem 1000000,
    Cal_Time  = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    display_date(Cal_Time, MicroSecs).

%%% From error_logger, but modified to RFC8601 (without using io:format)
display_date({{Y,Mo,D},{H,Mi,S}}, Micros) ->
    integer_to_list(Y) ++ "-" ++
        two_digits(Mo) ++ "-" ++
        two_digits(D)  ++ "T" ++
        two_digits(H)  ++ ":" ++
        two_digits(Mi) ++ ":" ++
        two_digits(S)  ++ "." ++
        six_digits(Micros) ++ "Z".

two_digits(N) when 0 =< N, N =< 9 ->
    [$0, $0 + N];
two_digits(N) ->
    integer_to_list(N).

six_digits(N) when      0 =< N, N =<     9 -> [$0, $0, $0, $0, $0,         $0 + N ];
six_digits(N) when     10 =< N, N =<    99 -> [$0, $0, $0, $0 | integer_to_list(N)];
six_digits(N) when    100 =< N, N =<   999 -> [$0, $0, $0     | integer_to_list(N)];
six_digits(N) when   1000 =< N, N =<  9999 -> [$0, $0         | integer_to_list(N)];
six_digits(N) when  10000 =< N, N =< 99999 -> [$0             | integer_to_list(N)];
six_digits(N)                              -> integer_to_list(N).
