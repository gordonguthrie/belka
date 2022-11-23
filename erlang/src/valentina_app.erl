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
                   {valid, UserState}; %% Allow self-signed certificates
                 (_,{bad_cert, _} = Reason, _) ->
                   {fail, Reason};
                 (_,{extension, _}, UserState) ->
                   {unknown, UserState};
                 (_, valid, UserState) ->
                   {valid, UserState};
                 (_, valid_peer, UserState) ->
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
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    spawn(valentina_app, handle_response, [TLSTransportSocket]),
    loop(ListenSSLSocket).

handle_response(TLSTransportSocket) ->
    ok = ssl:controlling_process(TLSTransportSocket, self()),
    Resp = ssl:handshake(TLSTransportSocket, 5000),
    {ok, Socket} = Resp,
    Id = case ssl:peercert(Socket) of
        {error, no_peercert} ->
            no_identity;
        {ok, Cert} ->
            C = extract_details(Cert),
            {identity, C}
        end,
    io:format("~p~n", [Socket]),
    receive
        Msg ->
            {ssl, _, Gemini} = Msg,
            URI = parse_gemini(Gemini),
            io:format("URI is ~p~n", [URI]),
            #{scheme := Scheme} = URI,
            case Scheme of
                "gemini" ->
                    Path = get_path(URI),
                    QueryKVs = get_query_KVs(URI),
                    Frags = get_frag(URI),
                    Route = #{id       => Id,
                              path     => Path,
                              querykvs => QueryKVs,
                              frags    => Frags},
                    io:format("Route is ~p~n", [Route]),
                    ok = ssl:send(Socket, <<"20 text/gemini\r\n">>),
                    ok = ssl:send(Socket, <<"gingo bongo\r\n">>),
                    ok = ssl:close(Socket);
                Other ->
                    io:format("in invalid scheme"),
                    exit({invalid_scheme, Other})
            end
        end,
    ok.

get_path(#{path := P}) -> string:tokens(P, "/").

get_query_KVs(#{'query' := Q}) -> uri_string:dissect_query(Q);
get_query_KVs(_)            -> [].

get_frag(#{fragment := F}) -> F;
get_frag(_)                -> "".

parse_gemini(Path) ->
    _URI = uri_string:percent_decode(uri_string:parse(string:trim(Path))).

extract_details(Cert) ->
    {_, Data, _, _} = public_key:pkix_decode_cert(Cert, otp),
    {_, _, _, _, _, _, Subject, PubKey, _, _, _} = Data,
    {_, _, {'RSAPublicKey', Key, _}} = PubKey,
    #{name => get_common_name(Subject), key => Key}.

get_common_name({rdnSequence, Data}) ->
    Details = [ {convert_key(Oid), ensure_binary(Value) } ||
        [{'AttributeTypeAndValue', Oid, Value}] <- Data ],
    proplists:get_value(common_name, Details);
get_common_name(_X) ->
    throw(common_name_parse_failure).

ensure_binary(S) when is_list(S)                 -> list_to_binary(S);
ensure_binary({utf8String, B}) when is_binary(B) -> B.

convert_key({2,5,4,3})  -> common_name;
convert_key({2,5,4,6})  -> country;
convert_key({2,5,4,8})  -> location;
convert_key({2,5,4,10}) -> organisation;
convert_key(_)          -> unknown.
