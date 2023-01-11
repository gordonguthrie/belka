## Belka Gemini Server

Belka is a [Gemini Server](https://gemini.circumlunar.space/)

Named after Belka - the second dog in space

This is the sequence diagram for the server

```
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

```

```erlang

-module(belka).

```

This is the API used to start the Gemini Server

```erlang
-export([start/3]).

```

These exports are reserved for use inside the Gemini Server

```erlang
-export([
    listening_loop/2,
    handle_incoming/2
    ]).


start(Port, Certs, HandlerFn) ->

```

This function verifies the client certificate provide
We override the default behavior as Gemini welcomes self-signed
certificates

```erlang
    Fun1 = fun(_, {bad_cert, selfsigned_peer}, UserState) ->
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

```

This function is to support multiple domains bound to the same IP
for SSL multi-hosting
We looking up the website the client is requesting and return their certificates

```erlang
    Fun2 = fun(Site) ->
            {Site, Cs} = lists:keyfind(Site, 1, Certs),
            [{certs_keys, [Cs]}]
        end,

```

these are all the options we start the ssl socket with

```erlang

    A = [{active,               true}],
    L = [{log_level,            info}],
    S = [{sni_fun,              Fun2}],
    V = [{verify,               verify_peer},
         {fail_if_no_peer_cert, false},
         {verify_fun,           {Fun1, []}}],

```

we start a socket listening for a connection on the gemini:// port 1965

* if the client passes in a client certificate we will check it with our callback function
* when the client tells us what URL they are accessing we will look up the certificate for that domain to establish a TLS connection for them
^

```erlang
    {ok, ListenSSLSocket} = ssl:listen(Port, A ++ L ++ S ++V),

```

now we pass off the listening socket to its own process

```erlang
    _Pid = spawn_link(belka, listening_loop, [ListenSSLSocket, HandlerFn]).

```

internal functions

## The listening loop
This is a very lightweight loop - when someone external contacts it is sets up a TLS connection and then passes it off immediately to another process to handle. Each connection gets its own handling process, and spawning a process is cheap so this is safe up to 100,000s of connections

```erlang
listening_loop(ListenSSLSocket, HandlerFn) ->
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    _PID = spawn(belka, handle_incoming, [TLSTransportSocket, HandlerFn]),
    listening_loop(ListenSSLSocket, HandlerFn).

```

# The handling process
This function handles incoming connections, each in their own process

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
```

we are running the socket in passive mode so we wait in receive for the socket to send up a message

```erlang
    receive
        Msg ->
            {ssl, _, Gemini} = Msg,
            URI = parse_gemini(Gemini),
            #{scheme := Scheme} = URI,
            case Scheme of
                "gemini" ->
                    Path = get_path(URI),
                    #{host := Host} = URI,
                    QueryKVs = get_query_KVs(URI),
                    Frags = get_frag(URI),
                    Route = #{id       => Id,
                              host     => Host,
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

```

## Internal functions

These internal functions are all called by the handling function to process the URLs, extract keys and stuff

```erlang

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
