# Belka Gemini Server

## Belka is a Gemini Server - https://gemini.circumlunar.space/

Named after Belka - the second dog in space

This is the sequence diagram for the server

     +---------+                      +-------------+          +---------------+                 +-----------------+                    +---------------+
     | YourApp |                      | Belkaserver |          | ListeningLoop |                 | HandleIncoming  |                    | GeminiClient  |
     +---------+                      +-------------+          +---------------+                 +-----------------+                    +---------------+
          |                                  |                         |                                  |                                     |
          | define handler fn                |                         |                                  |                                     |
          |------------------                |                         |                                  |                                     |
          |                 |                |                         |                                  |                                     |
          |<-----------------                |                         |                                  |                                     |
          |                                  |                         |                                  |                                     |
          | start Belka w/ handler fn        |                         |                                  |                                     |
          |--------------------------------->|                         |                                  |                                     |
          |                                  |                         |                                  |                                     |
          |                                  | spawn process           |                                  |                                     |
          |                                  |------------------------>|                                  |                                     |
          |                                  |                         |                                  |                                     |
          |                                  |                         | bind to TLS Socket               |                                     |
          |                                  |                         |-------------------               |                                     |
          |                                  |                         |                  |               |                                     |
          |                                  |                         |<------------------               |                                     |
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  |                          start conn |
          |                                  |                         |<-----------------------------------------------------------------------|
          |                                  |                         |                                  |                                     |
          |                                  |                         | spawn process with conn          |                                     |
          |                                  |                         |--------------------------------->|                                     |
          |                                  |                         |                                  |                                     |
          |                                  |                         | go back to listening             |                                     |
          |                                  |                         |---------------------             |                                     |
          |                                  |                         |                    |             |                                     |
          |                                  |                         |<--------------------             |                                     |
          |                                  |-----------------------\ |                                  |                                     |
          |                                  || (ready for new conn) |-|                                  |                                     |
          |                                  ||----------------------| |                                  |                                     |
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  | take control of conn                |
          |                                  |                         |                                  |---------------------                |
          |                                  |                         |                                  |                    |                |
          |                                  |                         |                                  |<--------------------                |
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  | signal ready                        |
          |                                  |                         |                                  |------------------------------------>|
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  |                         get command |
          |                                  |                         |                                  |<------------------------------------|
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  | call handler fn                     |
          |                                  |                         |                                  |----------------                     |
          |                                  |                         |                                  |               |                     |
          |                                  |                         |                                  |<---------------                     |
          |                                  |                         |                                  |                                     |
          |                                  |                         |                                  | return output of handler fn         |
          |                                  |                         |                                  |------------------------------------>|
          |                                  |                         |                                  |                                     |


```erlang

-module(belka).

```

## Public API

This is the API used to start the Gemini Server

```erlang
-export([start/4]).

```

These exports are reserved for use inside the Gemini Server

```erlang
-export([
    listening_loop/2,
    handle_incoming/2
    ]).

start(Port, CertFile, KeyFile, HandlerFn) ->

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

    Certs = [{certs_keys, [#{certfile => CertFile,
                             keyfile  => KeyFile}]}],
    A = [{active, true}],
    L = [{log_level, info}],
    V = [{verify, verify_peer}, {fail_if_no_peer_cert, false}, {verify_fun, {F, []}}],
    {ok, ListenSSLSocket} = ssl:listen(Port, Certs ++ A ++ L ++ V),
    _Pid = spawn_link(belka, listening_loop, [ListenSSLSocket, HandlerFn]).

```

## Private Functions

The listening loop

```erlang
listening_loop(ListenSSLSocket, HandlerFn) ->
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    _PID = spawn(belka, handle_incoming, [TLSTransportSocket, HandlerFn]),
    listening_loop(ListenSSLSocket, HandlerFn).

```

The functions that handles incoming connections

```erlang
handle_incoming(TLSTransportSocket, {M, F}) ->
    ok = ssl:controlling_process(TLSTransportSocket, self()),
    {ok, Socket} = ssl:handshake(TLSTransportSocket, 5000),
    Id = case ssl:peercert(Socket) of
        {error, no_peercert} ->
            no_identity;
        {ok, Cert} ->
            extract_details(Cert)
        end,
    receive
        Msg ->
            {ssl, _, Gemini} = Msg,
            URI = parse_gemini(Gemini),
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
                    Responses = M:F(Route),
				    [ok = ssl:send(Socket, X) || X <- Responses],
				    ok = ssl:close(Socket);
                Other ->
                    exit({invalid_scheme, Other})
            end
        end,
    ok.

get_path(#{path := P}) -> string:tokens(P, "/").

get_query_KVs(#{'query' := Q}) -> uri_string:dissect_query(Q);
get_query_KVs(_)               -> [].

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


```
