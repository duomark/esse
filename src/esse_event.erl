%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Library functions for creating events to send.
%%%
%%% @since v0.1.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_event).
-author('Jay Nelson <jay@duomark.com>').

%%% External API
-export([data_only/1, data_event/2, object_event/3]).

-define(NL, 10).


%%%===================================================================
%%% API functions
%%%===================================================================

-type id      () ::  binary().
-type event   () ::  binary().
-type data    () :: [string()] | [binary()].
-type sse_out () ::  iolist()  |  binary().

-spec data_only    (               [data()]) -> sse_out().
-spec data_event   (      event(), [data()]) -> sse_out().
-spec object_event (id(), event(), [data()]) -> sse_out().

data_only([])           -> <<":", ?NL>>;
data_only([Data])       ->  make_data_only(Data);
data_only([_|_] = Data) -> [make_data_only(Line) || Line <- Data].

make_data_only(Data) when is_binary (Data) ->  <<"data: ",   Data/binary, ?NL>>;
make_data_only(Data) when is_list   (Data) -> [<<"data: ">>, Data,        ?NL].


data_event(Event, [])           ->  <<"event: ", Event/binary, ?NL,     "data: ", ?NL>>;
data_event(Event, [Data])       -> [<<"event: ", Event/binary, ?NL>>,   make_data_only(Data)];
data_event(Event, [_|_] = Data) -> [<<"event: ", Event/binary, ?NL>> | [make_data_only(Line) || Line <- Data]].


object_event(Id, Event, [])           ->  <<"id: ", Id/binary, ?NL,    (data_event(Event, []))/binary>>;
object_event(Id, Event, [Data])       -> [<<"id: ", Id/binary, ?NL>> |  data_event(Event, [Data])];
object_event(Id, Event, [_|_] = Data) -> [<<"id: ", Id/binary, ?NL>> |  data_event(Event,  Data )].
