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
-export([response_headers/1]).

-define(CR, 13).
-define(LF, 10).


%%%===================================================================
%%% Response headers
%%%===================================================================

-type response_type () ::  ok
                         | no_content
                         | {temporary, binary()}
                         | {permanent, binary()}.

-spec response_headers(response_type()) -> sse_out().

%%% Response headers are only modern HTTP/1.1 format.
response_headers(ok)                                   -> make_headers(<< "HTTP/1.1 200 OK">>);
response_headers(no_content)                           -> make_headers(<< "HTTP/1.1 204 No Content">>);
response_headers({temporary, URL}) when is_binary(URL) -> make_headers(<< "HTTP/1.1 307 Temporary Redirect">>, <<"Location: ", URL/binary>>);
response_headers({permanent, URL}) when is_binary(URL) -> make_headers(<< "HTTP/1.1 308 Permanent Redirect">>, <<"Location: ", URL/binary>>).

make_headers(Status_Code) ->
    << Status_Code        /binary, ?CR, ?LF,
       (common_headers()) /binary, ?CR, ?LF >>.

make_headers(Status_Code, Extra) ->
    << Status_Code        /binary, ?CR, ?LF,
       Extra              /binary, ?CR, ?LF,
       (common_headers()) /binary, ?CR, ?LF >>.

common_headers() ->
    Date   = make_timestamp(),
    Server = << "esse/", (get_version())/binary >>,
    << "Date: ",          Date   /binary,      ?CR, ?LF,
       "Server: ",        Server /binary,      ?CR, ?LF,
       "Content-Type: ",  "text/event-stream", ?CR, ?LF,
       "Cache-Control: ", "no-store",          ?CR, ?LF >>.  % no-cache?

get_version() ->
    list_to_binary(application:get_env(esse, version, "Dev")).

make_timestamp() ->
    <<"Thu, 29 Dec 2016 21:45:30 GMT">>.


%%%===================================================================
%%% Streamed data functions
%%%===================================================================

-type id            () ::  binary().
-type event         () ::  binary().
-type data          () :: [string()] | [binary()].
-type sse_out       () ::  iolist()  |  binary().

-spec data_only    (               [data()]) -> sse_out().
-spec data_event   (      event(), [data()]) -> sse_out().
-spec object_event (id(), event(), [data()]) -> sse_out().

%%% Default event type 'message'
data_only([])           -> <<":", ?LF>>;
data_only([Data])       ->  make_data_only(Data);
data_only([_|_] = Data) -> [make_data_only(Line) || Line <- Data].

make_data_only(Data) when is_binary (Data) ->  <<"data: ",   Data/binary, ?LF>>;
make_data_only(Data) when is_list   (Data) -> [<<"data: ">>, Data,        ?LF].

%%% Explicit event type
data_event(Event, [])           ->  <<"event: ", Event/binary, ?LF,     "data: ", ?LF>>;
data_event(Event, [Data])       -> [<<"event: ", Event/binary, ?LF>>,   make_data_only(Data)];
data_event(Event, [_|_] = Data) -> [<<"event: ", Event/binary, ?LF>> | [make_data_only(Line) || Line <- Data]].

%%% Sequence id used for resuming event stream
object_event(Id, Event, [])           ->  <<"id: ", Id/binary, ?LF,    (data_event(Event, []))/binary>>;
object_event(Id, Event, [Data])       -> [<<"id: ", Id/binary, ?LF>> |  data_event(Event, [Data])];
object_event(Id, Event, [_|_] = Data) -> [<<"id: ", Id/binary, ?LF>> |  data_event(Event,  Data )].
