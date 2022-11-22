%%%-------------------------------------------------------------------
%% @doc valentina public API
%% @end
%%%-------------------------------------------------------------------

-module(valentina_app).

-behaviour(application).

-export([start/2, stop/1, handle_response/1]).

start(_StartType, _StartArgs) ->
    io:format("in process ~p~n", [self()]),
    ok = ssl:start(),
    F = fun(_, {bad_cert, selfsigned_peer}, UserState) ->
                   io:format("in self signed~n"),
                   {valid, UserState}; %% Allow self-signed certificates
                 (_,{bad_cert, _} = Reason, _) ->
                   io:format("in bad cert~n"),
                   {fail, Reason};
                 (_,{extension, _}, UserState) ->
                   io:format("in extension~n"),
                   {unknown, UserState};
                 (_, valid, UserState) ->
                   io:format("in valid~n"),
                   {valid, UserState};
                 (_, valid_peer, UserState) ->
                   io:format("in valid_peer~n"),
                   {valid, UserState}
               end,
    Certs = [{certs_keys, [#{certfile => "/valentina/priv/keys/server.crt",
                             keyfile => "/valentina/priv/keys/server.key"}]}],
    A = [{active, true}],
    L = [{log_level, info}],
    V = [{verify, verify_peer}, {fail_if_no_peer_cert, false}, {verify_fun, F}],
    {ok, ListenSSLSocket} = ssl:listen(1965, Certs ++ A ++ L ++ V),
    loop(ListenSSLSocket),
    valentina_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
loop(ListenSSLSocket) ->
    io:format("in loop~n"),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    io:format("in loop with socket ~p~n", [TLSTransportSocket]),
    spawn(valentina_app, handle_response, [TLSTransportSocket]),
    loop(ListenSSLSocket).

handle_response(TLSTransportSocket) ->
    io:format("spawned to ~p~n", [self()]),
    ok = ssl:controlling_process(TLSTransportSocket, self()),
    Resp = ssl:handshake(TLSTransportSocket, 5000),
    io:format("handshake response is ~p~n", [Resp]),
    {ok, Socket} = Resp,
    io:format("in handle_response with socket ~p~n", [Socket]),
    io:format("handshake complete~n"),
    ConnInf = ssl:connection_information(Socket),
    io:format("conn inf is ~p~n", [ConnInf]),
    case ssl:peercert(Socket) of
        {error, no_peercert} ->
            io:format("no peer certificate~n");
        {ok, Cert} ->
            io:format("peer cert is ~p~n", [Cert])
        end,
    io:format("~p~n", [Socket]),
    receive
        Msg ->
            io:format("got msg ~p~n", [Msg]),
            ok = ssl:send(Socket, <<"20 text/gemini\r\n">>),
            ok = ssl:send(Socket, <<"gingo bongo\r\n">>),
            ok = ssl:close(Socket)
        end,
    ok.
