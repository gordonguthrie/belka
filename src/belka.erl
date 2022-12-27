%% # Belka Gemini Server 2️⃣🐕🚀
%%
%% ## Belka is a Gemini Server
%%
%% [Read more about Gemini](https://gemini.circumlunar.space/)
%%
%% This server is named after Belka - the second dog in space
%%
%% This is the sequence diagram for the server
%%
%%      +---------+                      +-------------+          +---------------+                 +-----------------+                    +---------------+
%%      | YourApp |                      | Belkaserver |          | ListeningLoop |                 | HandleIncoming  |                    | GeminiClient  |
%%      +---------+                      +-------------+          +---------------+                 +-----------------+                    +---------------+
%%           |                                  |                         |                                  |                                     |
%%           | define handler fn                |                         |                                  |                                     |
%%           |------------------                |                         |                                  |                                     |
%%           |                 |                |                         |                                  |                                     |
%%           |<-----------------                |                         |                                  |                                     |
%%           |                                  |                         |                                  |                                     |
%%           | start Belka w/ handler fn        |                         |                                  |                                     |
%%           |--------------------------------->|                         |                                  |                                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  | spawn process           |                                  |                                     |
%%           |                                  |------------------------>|                                  |                                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         | bind to TLS Socket               |                                     |
%%           |                                  |                         |-------------------               |                                     |
%%           |                                  |                         |                  |               |                                     |
%%           |                                  |                         |<------------------               |                                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  |                          start conn |
%%           |                                  |                         |<-----------------------------------------------------------------------|
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         | spawn process with conn          |                                     |
%%           |                                  |                         |--------------------------------->|                                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         | go back to listening             |                                     |
%%           |                                  |                         |---------------------             |                                     |
%%           |                                  |                         |                    |             |                                     |
%%           |                                  |                         |<--------------------             |                                     |
%%           |                                  |-----------------------\ |                                  |                                     |
%%           |                                  || (ready for new conn) |-|                                  |                                     |
%%           |                                  ||----------------------| |                                  |                                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  | take control of conn                |
%%           |                                  |                         |                                  |---------------------                |
%%           |                                  |                         |                                  |                    |                |
%%           |                                  |                         |                                  |<--------------------                |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  | signal ready                        |
%%           |                                  |                         |                                  |------------------------------------>|
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  |                         get command |
%%           |                                  |                         |                                  |<------------------------------------|
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  | call handler fn                     |
%%           |                                  |                         |                                  |----------------                     |
%%           |                                  |                         |                                  |               |                     |
%%           |                                  |                         |                                  |<---------------                     |
%%           |                                  |                         |                                  |                                     |
%%           |                                  |                         |                                  | return output of handler fn         |
%%           |                                  |                         |                                  |------------------------------------>|
%%           |                                  |                         |                                  |                                     |
%%

-module(belka).

%% ## Public API

%% This is the API used to start the Gemini Server
-export([start/4]).

%% These exports are reserved for use inside the Gemini Server
-export([
    listening_loop/2,
    handle_incoming/2
    ]).

%% This is the function that your application will actually call.
%% There are 4 arguments:

%% * what port to start listening on (normally 1965 and some Gemini clients will fail if you try to use another one)
%% * a certificate file
%% * a keyfile
%% * a handler function
%% ^

%% Every computer on the internet has its own IP (Internet Protocol Address) - which looks like 123.456.789.012
%% and on that IP address it can listen on 65536 (2 to the power 16) ports:

%% * the web is normally 80 (unsecured `http://`) and 43 (secured 'https://')
%% * email is on port 25 (`smtp://`)
%% * ssh is on port 22
%% ^

%% `gemini://` is 1965

%% The two cryptographic parameters identify your server and service
%% please see [belka-example](https://github.com/gordonguthrie/belka-example)
%% for a deeper explanaction

%% The last parameter is a handler function. This server is dumb.
%% It listens for someone trying to speak `gemini://` to it.
%% Then the servrer says hello and does a crypto graphic handshake and makes sure
%% the machine-to-machine comms are set up safely
%% and then it says "I'm done" and calls the handler function

%% The handler function's job is to do stuff - provide the service
%% be the 'thing' that you are writing.

%% The start function prepares a socket to listen on the port:

%% * it creates a function to verify the Secure Socket Layer connection
%% * it then starts listening on the port
%% * and finally it spawns another process and passes the listener it has just created (and the handler function) over
%% ^

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

%% ## Private Functions

%% ## The listening loop

%% The listening loop is a very simple function

%% * it listens for somebody out there on the internet trying to connect
%% * when they do it executes a `transport accept` which means it is ready to talk
%% * then instead of actually talking it spawns another process and gives the connection to it
%% ^

%% It is a receptionist function, it answers the phone, puts you through and goes
%% back to waiting for the phone to ring

listening_loop(ListenSSLSocket, HandlerFn) ->
    {ok, TLSTransportSocket} = ssl:transport_accept(ListenSSLSocket),
    _PID = spawn(belka, handle_incoming, [TLSTransportSocket, HandlerFn]),
    listening_loop(ListenSSLSocket, HandlerFn).

%% ## Handling Incoming requrests

% this is the meat of the `gemini://` server

% It takes the transport socket accept and first extracts the ID
% of the client - a key and a name

% Then it checks if the system talking to it wants to talk `gemini://`

% If the client wants to talk `gemini://` this function breaks up the request
% tidy's it up and then

% * calls the handler function
% * waits to get a response back
% * sends that response back to the client
% * closes the socket and dies
% ^

% If the remote server doesn't want to talk `gemini://`
% (say the remote user has pointed their web browser to port 1965
% and is looking for something that speaks `http://`) this process just kills itself

handle_incoming(TLSTransportSocket, {Module, Function}) ->
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
                    Frag = get_frag(URI),
                    Route = #{id       => Id,
                              path     => Path,
                              querykvs => QueryKVs,
                              frag     => Frag},
                    Responses = Module:Function(Route),
				    [ok = ssl:send(Socket, X) || X <- Responses],
				    ok = ssl:close(Socket);
                Other ->
                    exit({invalid_scheme, Other})
            end
        end,
    ok.

%% ## Bits and pieces

% ### Tidying up the request

% These functions are all called on the request that the user has sent it.
% They break up that request and make it easy to process

get_path(#{path := P}) -> string:tokens(P, "/").

get_query_KVs(#{'query' := Q}) -> uri_string:dissect_query(Q);
get_query_KVs(_)               -> [].

get_frag(#{fragment := F}) -> F;
get_frag(_)                -> "".

parse_gemini(Path) ->
    _URI = uri_string:percent_decode(uri_string:parse(string:trim(Path))).

% ### Getting bits out of the cryptographic certificates

% The application server is only really interested in two things:

% * what the person has called themselves (their name)
% * what public key are they identifiable by
% ^

% these functions just rummage around in the data that is sent setting up a secure
% communication and extract those bits.

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

