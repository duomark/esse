%%%------------------------------------------------------------------------------
%%% @copyright (c) 2016-2017, DuoMark International, Inc.
%%% @author Jay Nelson <jay@duomark.com>
%%% @reference The license is based on the template for Modified BSD from
%%%   <a href="http://opensource.org/licenses/BSD-3-Clause">OSI</a>
%%% @doc
%%%   Library of functions for receiving and parsing user_agent requests,
%%%   plus acting as a user_agent.
%%%
%%% @since v0.1.1
%%% @end
%%%------------------------------------------------------------------------------
-module(esse_user_agent).
-author('Jay Nelson <jay@duomark.com>').

%%% External API
-export([receive_request/1]).

%%% For testing
-export([parse_request/1, example/1]).


%%%===================================================================
%%% External API
%%%===================================================================

-type client_request() :: any().
-spec receive_request(gen_tcp:socket()) -> client_request().

-define(MAX_HDR_SIZE, 2000).
recv_timeout() -> timer:seconds(2).
     
receive_request(Socket) ->
    case gen_tcp:recv(Socket, 0, recv_timeout()) of
        {error, _Reason} = Err ->
            Err;
        {ok, Bin_Request} when byte_size(Bin_Request) >= ?MAX_HDR_SIZE ->
            {error, client_sent_too_much_data};
        {ok, Bin_Request} ->
            parse_request(Bin_Request)
    end.


%%%===================================================================
%%% Internal support functions
%%%===================================================================

crlf           () -> binary:compile_pattern(<<"\r\n">>).
header_elems   () -> binary:compile_pattern(<<": ">>).
accept_clauses () -> binary:compile_pattern(<<";">>).
accept_elems   () -> binary:compile_pattern(<<",">>).
request_elems  () -> binary:compile_pattern(<<" ">>).
    

parse_request(Bin_Request) ->
    [Req, Headers] = binary:split(Bin_Request, crlf()),
    Req_Elems = binary:split(Req, request_elems(), [global]),
    parse_request(Req_Elems, Headers).

parse_request([<<"GET">>, Path, Http_Version], Headers) ->
    Header_Lines = binary:split(Headers, crlf(), [global]),
    validate_http_and_headers(Path, Http_Version, lists:reverse(Header_Lines));
parse_request([Bad_Verb | _Request], _Headers) ->
    {error, {not_a_get_request, Bad_Verb}}.

validate_http_and_headers( Path, <<"HTTP/1.1">>, [<<>>, <<>> | Headers]) ->
    validate_headers(Headers, length(Headers) + 2, #{path => Path});
validate_http_and_headers(_Path, <<"HTTP/1.1">>, _Headers) ->
    {error, http_header_must_end_with_2_crlfs};
validate_http_and_headers(_Path, Http_Version, [<<>>, <<>> | _Headers]) ->
    {error, {'not_http_1.1', Http_Version}};
validate_http_and_headers(_Path, Http_Version, _Headers) ->
    {error, [{'not_http_1.1', Http_Version}, http_header_must_end_with_2_crlfs]}.

%%% All should be 'Key: Value', report header line number on first error.
validate_headers(          [],    2, #{} = Header_Map) -> Header_Map;
validate_headers([Hdr | Hdrs], Line, #{} = Header_Map) ->
    case Hdr of

        %% The 'Accept: ' must allow 'Content-Type: text/event-source' reply
        <<"Accept: ", Value/binary>> ->
            case valid_accept(Value) of
                {error, Err} -> {error, {{header_line, Line-2}, Err}};
                valid        -> validate_headers(Hdrs, Line-1, Header_Map)
            end;

        %% No other headers are validated on explicit data values.
        Hdr ->
            case binary:split(Hdr, header_elems()) of
                [_Key]        -> {error, {invalid_header, Hdr}};
                [ Key, Value] -> validate_headers(Hdrs, Line-1, Header_Map#{Key => Value})
            end
    end.

valid_accept(Accept) ->
    case accepts_text_event_stream(Accept) of
        false -> {error, must_accept_text_event_stream};
        true  -> valid
    end.

accepts_text_event_stream(<<"*/*">>)       -> true;
accepts_text_event_stream(<<"text/html">>) -> false;
accepts_text_event_stream(Maybe_Multiple)  ->
    Accept_Formulae = binary:split(Maybe_Multiple, accept_clauses(), [global]),
    All_Accepts = lists:flatten([[Patt || Patt <- binary:split(Formula, accept_elems(), [global])]
                                 || Formula <- Accept_Formulae]),
    error_logger:info_msg("Accepts => ~p~n", [All_Accepts]),
    lists:member(<<"*/*">>, All_Accepts)
        orelse lists:member(<<"text/event-stream">>, All_Accepts).

%%% Example 2 versions of Safari 'http://localhost:9997/abc' client browser request:
example(1) ->
    <<"GET /abc HTTP/1.1\r\n",
      "Host: localhost:9997\r\n",
      "Connection: keep-alive\r\n",
      "Upgrade-Insecure-Requests: 1\r\n",
      "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n",
      "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/602.3.12 (KHTML, like Gecko) Version/10.0.2 Safari/602.3.12\r\n",
      "Accept-Language: en-us\r\n",
      "DNT: 1\r\n",
      "Accept-Encoding: gzip, deflate\r\n",
      "\r\n">>;
example(2) ->
    <<"POST /abc HTTP/1.1\r\n",
      "Host: localhost:9997\r\n",
      "Accept-Encoding: gzip, deflate\r\n",
      "Connection: keep-alive\r\n",
      "Upgrade-Insecure-Requests: 1\r\n",
      "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n",
      "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/602.3.12 (KHTML, like Gecko) Version/10.0.2 Safari/602.3.12\r\n",
      "Accept-Language: en-us\r\n",
      "DNT: 1\r\n",
      "Cache-Control: max-age=0\r\n",
      "\r\n">>.
