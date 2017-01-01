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

-export([receive_request/1]).


%%%===================================================================
%%% External API
%%%===================================================================

-type client_request() :: any().
-spec receive_request(gen_tcp:socket()) -> client_request().

receive_request(Socket) ->
    case gen_tcp:recv(Socket, 0, timer:seconds(1)) of
        {error, _Reason} = Err -> Err;
        {ok, Bin_Request}      -> parse_request(Bin_Request)
    end,
    ok.


%%%===================================================================
%%% Internal support functions
%%%===================================================================

%%% Example Safari 'http://localhost:9997/abcadoi-2355' client browser request:
%%%
%%% GET /abcadoi-2355 HTTP/1.1\r\n
%%% Host: localhost:9997\r\n
%%% Connection: keep-alive\r\n
%%% Upgrade-Insecure-Requests: 1\r\n
%%% Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n
%%% User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/602.3.12 (KHTML, like Gecko) Version/10.0.2 Safari/602.3.12\r\n
%%% Accept-Language: en-us\r\n
%%% DNT: 1\r\n
%%% Accept-Encoding: gzip, deflate\r\n
%%% \r\n

%%% GET /abc HTTP/1.1\r\n
%%% Host: localhost:9997\r\n
%%% Accept-Encoding: gzip, deflate\r\n
%%% Connection: keep-alive\r\n
%%% Upgrade-Insecure-Requests: 1\r\n
%%% Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n
%%% User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_2) AppleWebKit/602.3.12 (KHTML, like Gecko) Version/10.0.2 Safari/602.3.12\r\n
%%% Accept-Language: en-us\r\n
%%% DNT: 1\r\n
%%% Cache-Control: max-age=0\r\n
%%% \r\n
crlf() -> binary:compile_pattern(<<"\r\n\r\n">>).

parse_request(Request)
  when is_binary(Request) ->
    Lines = binary:split(Request, crlf(), [global]),
    report_lines(Lines),
    ok.

report_lines([<<"">>, <<"">>]) -> done;
report_lines([Line   | Lines]) -> error_logger:info_msg("~s~n",[Line]).

    
