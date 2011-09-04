%% 
%% This is a wrapper around the mochiweb_socket_server.  It's based
%% on 'mochiweb_http' but handles the WebSocket protocol.
%% In this implementation there's no timeout set on reading from the socket so the
%% client connection does not timeout.
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(mochiweb_websocket).

-export([start/3, stop/1]).
-export([loop/2, default_hello/1]).

-export([generate_websocket_accept/1]).

-include("common.hrl").

-define(DEFAULTS, [{name, ?MODULE},
    {port, 8002}]).

-define(WEBSOCKET_PREFIX,"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n").

set_default({Prop, Value}, PropList) ->
  case proplists:is_defined(Prop, PropList) of
    true ->
      PropList;
    false ->
      [{Prop, Value} | PropList]
  end.


set_defaults(Defaults, PropList) ->
  lists:foldl(fun set_default/2, PropList, Defaults).

parse_options(Options) ->
  {loop, MyLoop} = proplists:lookup(loop, Options),
  Loop = fun (S) ->
      ?MODULE:loop(S, MyLoop)
  end,
  Options1 = [{loop, Loop} | proplists:delete(loop, Options)],
  set_defaults(?DEFAULTS, Options1).

start(Host, Port, Loop) ->
  start([{ip, Host}, {loop, Loop}, {port, Port}, {name, port_name(Port)}]).

stop(Port) ->
  Name = port_name(Port),
  io:format("mochiweb_websocket stop ~p~n", [Name]),
  mochiweb_socket_server:stop(Name).

start(Options) ->
  mochiweb_socket_server:start(parse_options(Options)).

port_name(Port) when is_integer(Port) ->
    list_to_atom("portServer" ++ integer_to_list(Port)).

%% Default loop if you start the server with 'start()'
default_hello(WebSocket) ->
  Data = WebSocket:get_data(),
  error_logger:info_msg("Rec from the client: ~p~n",[Data]),
  WebSocket:send("hello from the new WebSocket api").

loop(Socket, MyLoop) ->
  %% Set to http packet here to do handshake
  inet:setopts(Socket, [{packet, http}]),
  handshake(Socket,MyLoop).

handshake(Socket,MyLoop) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, {http_request, _Method, Path, _Version}} ->
      {abs_path,PathA} = Path,
      check_header(Socket,PathA,[],MyLoop);
    {error, {http_error, "\r\n"}} ->
      handshake(Socket, MyLoop);
    {error, {http_error, "\n"}} ->
      handshake(Socket, MyLoop);
    Other ->
      io:format("Got: ~p~n",[Other]),
      gen_tcp:close(Socket),
      exit(normal)
  end.

check_header(Socket,Path,Headers,MyLoop) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, http_eoh} ->
      verify_handshake(Socket,Path,Headers),
      %% Set packet back to raw for the rest of the connection
      inet:setopts(Socket, [{packet, raw}, {active, true}]),
      request(Socket, MyLoop, none);
    {ok, {http_header, _, Name, _, Value}} ->
      check_header(Socket, Path, [{Name, Value} | Headers],MyLoop);
    _Other ->
      gen_tcp:close(Socket),
      exit(normal)
  end.

verify_handshake(Socket,Path,Headers) ->
  ?LOG([{websocket_handshake, {headers, Headers}}]),
  case string:to_lower(proplists:get_value('Upgrade',Headers)) of
    "websocket" ->
      send_handshake(Socket,Path,Headers);
    _Other ->
      error_logger:error_msg("Incorrect WebSocket headers. Closing the connection~n"),
      gen_tcp:close(Socket),
      exit(normal)
  end.

send_handshake(Socket, _Path,Headers) ->
  Key = get_header(Headers, "Sec-Websocket-Key"),
  Protocol = get_header(Headers, "Sec-Websocket-Protocol"),
  %Origin = get_header(Headers, "Sec-Websocket-Origin"),
  %Location = proplists:get_value('Host',Headers),
  Accept = generate_websocket_accept(Key),
  Resp = ?WEBSOCKET_PREFIX ++
  "Sec-WebSocket-Accept: " ++ Accept ++ "\r\n" ++
  "Sec-WebSocket-Protocol: " ++ Protocol ++ "\r\n\r\n" ,
  gen_tcp:send(Socket, Resp).

generate_websocket_accept(Key) ->
  base64:encode_to_string(crypto:sha(
    Key ++ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")).

get_header([{Key, Val}|T], FindKey) when is_list(Key) ->
  case string:to_lower(Key) == string:to_lower(FindKey) of
    true -> Val;
    _Other -> get_header(T, FindKey)
  end;

get_header([_H|T], FindKey) ->
  get_header(T, FindKey);

get_header([], _FindKey) ->
  ok.

request(Socket, MyLoop, LoopData) ->
  R = receive
    {tcp, Socket, Bin} ->
      Bin1 = websocket:decoding(Bin),
      {socket, Bin1};
    {tcp_closed, Socket} ->
      tcp_closed;
    {packet, Packet} ->
      {packet, Packet}
  end,

  LoopData1 = MyLoop(Socket, R, LoopData),
  request(Socket, MyLoop, LoopData1).
  %WebSocketRequest = websocket_request:new(Socket),
  %MyLoop(WebSocketRequest).
