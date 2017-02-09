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
-export([
         receive_packets/1,   % erlang:decode_packet/3
         receive_request/1    % recv + functional parse
        ]).

%%% For testing functional parse
-export([parse_request/1, example/1]).


%%%===================================================================
%%% External API and support for erlang:decode_packet/3
%%%===================================================================

%%% Here are example returns from curl and Safari, respectively:

%%% #{path => <<"/abc">>,
%%%   <<"Host">> => <<"127.0.0.1:9997">>,
%%%   <<"User-Agent">> => <<"curl/7.51.0">>}

%%% #{path => <<"/">>,
%%%   <<"Accept-Encoding">> => <<"gzip, deflate">>,
%%%   <<"Accept-Language">> => <<"en-us">>,
%%%   <<"Connection">> => <<"keep-alive">>,
%%%   <<"DNT">> => <<"1">>,
%%%   <<"Host">> => <<"localhost:9997">>,
%%%   <<"Upgrade-Insecure-Requests">> => <<"1">>,
%%%   <<"User-Agent">> => <<"Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/602.4.8 (KHTML, like Gecko) Version/10.0.3 Safari/602.4.8">>}

-define(MAX_HDR_SIZE, 2000).
recv_timeout() -> timer:seconds(2).

%%% Receives as much as it can from socket.
receive_packets(Socket) ->
    case get_socket_data(Socket, 0, 0) of
        {error, _Reason} = Err1 -> Err1;
        Bin_Request -> 
            case parse_packets(Bin_Request, Socket) of
                {error, _} = Err2 -> Err2;
                Headers -> make_header_map(Headers)
            end
    end.

make_header_map([{http_request, 'GET',    {_Type, Path_Str},  {1,1}}    | Headers]) ->
    Hdr_Fields = [KV_Pair || {http_header, _Num, Hdr, _Extra, Val_Str} <- Headers,
                             (KV_Pair = validate_header(Hdr, Val_Str)) =/= valid_accept],
    case maps:from_list(Hdr_Fields) of
        #{error := Value} -> {error, Value};
        Map = #{}         -> Map#{path => list_to_binary(Path_Str)}
    end;
make_header_map([{http_request, 'GET',    {_Type, _Path_Str},  Version} | _Headers]) ->
    {error, {'not_http_1.1', Version}};
make_header_map([{http_request, Bad_Verb, {_Type, _Path_Str}, _Version} | _Headers]) ->
    {error, {not_a_get_request, Bad_Verb}}.

validate_header(Hdr, Val_Str)
  when is_list(Hdr) ->
    {list_to_binary(Hdr), list_to_binary(Val_Str)};
validate_header('Accept', Val_Str) ->
    valid_accept(list_to_binary(Val_Str));
validate_header(Hdr, Val_Str) ->
    {atom_to_binary(Hdr, utf8), list_to_binary(Val_Str)}.

%%% In place recursion because of upper bound on size.
parse_packets(Bin_Request, Socket) ->
    case erlang:decode_packet(http, Bin_Request, []) of
        {error,   _} = Err1  -> Err1;
        {ok, http_eoh, <<>>} -> {error, no_request};
        {more,       Length} -> 
            case get_more(Socket, Bin_Request, Length) of
                {error, _} = Err2     -> Err2;
                << More_Bin/binary >> -> parse_packets(More_Bin, Socket)
            end;
        {ok, {http_request, _Method, _Path, _Version} = Req, Rest} ->
            [Req | parse_headers(Rest, Socket)]
    end.

parse_headers({error, _} = Err, _Socket) -> [Err];
parse_headers(Bin_Request, Socket) ->
    case erlang:decode_packet(httph, Bin_Request, []) of
        {error, _} = Err1    -> [Err1];
        {ok, http_eoh, <<>>} -> [];
        {more,       Length} ->
            case get_more(Socket, Bin_Request, Length) of
                {error, _} = Err2     -> [Err2];
                << More_Bin/binary >> -> parse_headers(More_Bin, Socket)
            end;
        {ok, Packet, Rest} ->
            [Packet | parse_headers(Rest, Socket)]
    end.

get_more(Socket, Bin_Request, More_Bytes) ->
    case get_socket_data(Socket, byte_size(Bin_Request), More_Bytes) of
        {error, _} = Err2     -> Err2;
        << More_Bin/binary >> -> << Bin_Request/binary, More_Bin/binary >>
    end.
    
get_socket_data(_Socket, Num_Bytes_Gotten, Num_Bytes_To_Get)
  when Num_Bytes_Gotten + Num_Bytes_To_Get > ?MAX_HDR_SIZE ->
    {error, client_sent_too_much_data};
get_socket_data( Socket, Num_Bytes_Gotten, Num_Bytes_To_Get) ->
    case gen_tcp:recv(Socket, Num_Bytes_To_Get, recv_timeout()) of
        {error, _Reason} = Err ->
            Err;
        {ok, Bin_Request}
          when Num_Bytes_Gotten =:= 0, byte_size(Bin_Request) >= ?MAX_HDR_SIZE ->
            {error, client_sent_too_much_data};
        {ok, Bin_Request} ->
            Bin_Request
    end.


%%%===================================================================
%%% Internal support functions
%%%===================================================================

-type client_request() :: any().
-spec receive_request(gen_tcp:socket()) -> client_request().

receive_request(Socket) ->
    case gen_tcp:recv(Socket, 0, recv_timeout()) of
        {error, _Reason} = Err ->
            Err;
        {ok, Bin_Request} when byte_size(Bin_Request) >= ?MAX_HDR_SIZE ->
            {error, client_sent_too_much_data};
        {ok, Bin_Request} ->
            parse_request(Bin_Request)
    end.

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
                valid_accept -> validate_headers(Hdrs, Line-1, Header_Map)
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
        true  -> valid_accept
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
