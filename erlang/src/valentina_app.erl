%%%-------------------------------------------------------------------
%% @doc valentina public API
%% @end
%%%-------------------------------------------------------------------

-module(valentina_app).

-behaviour(application).

-export([start/2, stop/1, handle_response/1]).

start(_StartType, _StartArgs) ->
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
    V = [{verify, verify_peer}, {fail_if_no_peer_cert, false}, {verify_fun, {F, []}}],
    {ok, ListenSSLSocket} = ssl:listen(1965, Certs ++ A ++ L ++ V),
    loop(ListenSSLSocket),
    valentina_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
loop(ListenSSLSocket) ->
    io:format("in loop~n"),
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    spawn(valentina_app, handle_response, [TLSTransportSocket]),
    loop(ListenSSLSocket).

handle_response(TLSTransportSocket) ->
    io:format("spawned to ~p~n", [self()]),
    ok = ssl:controlling_process(TLSTransportSocket, self()),
    Resp = ssl:handshake(TLSTransportSocket, 5000),
    {ok, Socket} = Resp,
    ConnInf = ssl:connection_information(Socket),
    case ssl:peercert(Socket) of
        {error, no_peercert} ->
            io:format("no peer certificate~n");
        {ok, Cert} ->
            C = extract_details(Cert),
            io:format("extracted cert is ~p~n", [C])
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

extract_details(Cert) ->
    {_, Data, _, _} = public_key:pkix_decode_cert(Cert, otp),
    {_, _, _, _, _, _, Subject, PubKey, _, _, _} = Data,
    {_, _, {'RSAPublicKey', Key, _}} = PubKey,
    {dump_rdn(Subject), Key}.


dump_rdn({rdnSequence, Data}) ->
    Details = [ {oid_alias(Oid), munge_utf8(Value) } ||
        [{'AttributeTypeAndValue', Oid, Value}] <- Data ],
    proplists:get_value(common_name, Details);
dump_rdn(_X) ->
    throw(rdn_parse_failure).

munge_utf8(S) when is_list(S)                 -> list_to_binary(S);
munge_utf8({utf8String, B}) when is_binary(B) -> B.

oid_alias({2,5,4,3}) -> common_name;
oid_alias({2,5,4,6}) -> country;
oid_alias({2,5,4,8}) -> location;
oid_alias({2,5,4,10}) -> organisation;
oid_alias(_) -> unknown.
