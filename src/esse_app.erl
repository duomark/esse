%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   ESSE is the Essential Erlang Server for Sent Events.
%%%
%%%   It was consciously written to be used only VMs running
%%%   Erlang 19.0 or later, so it uses the new map interface
%%%   for supervisor specification, and can take advantage of
%%%   any of the new optimizations.
%%%
%%% @since v0.0.1
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_app).
-author('Jay Nelson <jay@duomark.com>').

-behaviour(application).

-export([start/0, start/2]).
-export([stop/1]).

-type restart_type() :: 'permanent' | 'transient' | 'temporary'.

-spec start() -> ok | {error, any()}.
start() ->
    application:start(esse).

-spec start(application:start_type(), restart_type()) -> {ok, pid()}.
start(_Type, _Args) ->
    esse_sup:start_link().

-spec stop([]) -> ok.
stop([]) ->
    ok.
