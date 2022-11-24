%%%-------------------------------------------------------------------
%% @doc laika example public API
%% @end
%%%-------------------------------------------------------------------

-module(laika_example).

-behaviour(application).

-export([start/2, stop/1, dummyHandler/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/laika/priv/keys/server.crt",
    KeyFile  = "/laika/priv/keys/server.key",
    _PID = laika:start(Port, CertFile, KeyFile, {laika_example, dummyHandler}),
    laika_example_sup:start_link().

stop(_State) ->
    ok.

dummyHandler(#{path := ["test", "input"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"10 What's your name\r\n">>
    ];
dummyHandler(#{path := ["test", "password"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"11 password plz\r\n">>
    ];
dummyHandler(#{path := []} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"20 text/gemini\r\n">>,
        <<"=> /test/input test input (status 10)\r\n">>,
        <<"=> /test/password test password input (status 11)\r\n">>,
        <<"=> /test/redirect/temporary test temporary redirects (status 30)\r\n">>,
        <<"=> /test/redirect/permanent test permanent redirects (status 31)\r\n">>,
        <<"=> /test/failure/temporary test temporary failure (status 40)\r\n">>,
        <<"=> /test/failure/permanent test permanent failure (status 50)\r\n">>,
        <<"=> /test/certificate test mandatory certificates (status 60)\r\n">>,
        <<"# header 1\r\n">>,
        <<"## header 2\r\n">>,
        <<"### header 3\r\n">>,
        <<"* bingo\r\n">>,
        <<"* bongo\r\n">>,
        <<"# Chess\r\n">>,
        <<"``` alt text\r\n">>,
        <<"   1  2  3  4  5  6  7  8 \r\n">>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"a ░♜░ ♞ ░♝░ ♚ ░♛░ ♝ ░♞░  ♜ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"      ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"b  ♟︎  ░♟︎░ ♟︎ ░♟︎░ ♟︎ ░♟︎░ ♟︎ ░♟︎░ \r\n"/utf8>>,
        <<"      ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"c ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"d    ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"e ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"f    ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"g ░♙░ ♙ ░♙░ ♙ ░♙░ ♙ ░♙░ ♙  \r\n"/utf8>>,
        <<"  ░░░   ░░░   ░░░   ░░░    \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"h  ♖ ░♘░ ♗ ░♕░ ♔ ░♗░ ♘ ░♖░ \r\n"/utf8>>,
        <<"     ░░░   ░░░   ░░░   ░░░ \r\n"/utf8>>,
        <<"```\r\n">>
    ];
dummyHandler(#{path := ["test", "redirect", "temporary"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"30 /test/redirect/success\r\n">>
    ];
dummyHandler(#{path := ["test", "redirect", "permanent"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"31 /test/redirect/success\r\n">>
    ];
dummyHandler(#{path := ["test", "redirect", "success"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"20 text/gemini\r\n">>,
        <<"successfully redirected\r\n">>
    ];
dummyHandler(#{path := ["test", "failure", "temporary"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"40 temporary failure\r\n">>
    ];
dummyHandler(#{path := ["test", "failure", "permanent"]} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"50 permanent failure\r\n">>
    ];
dummyHandler(#{path := ["test", "certificate"], id := no_identity} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
        <<"60 need certificate\r\n">>
    ];
dummyHandler(#{path := ["test", "certificate"], id := #{key := K, name := N}} = Route) ->
    io:format("handler got route ~p~n", [Route]),
    io:format("name is ~p~nkey is ~p~n", [N, K]),
    [
        <<"20 text/gemini\r\n">>,
        <<"you can see it because you are logged in\r\n">>
    ];
dummyHandler(Route) ->
    io:format("handler got route ~p~n", [Route]),
    [
    <<"20 text/gemini\r\n">>,
    <<"404\r\n">>
    ].
