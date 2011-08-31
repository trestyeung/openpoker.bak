%%
%% This is a wrapper for the Socket connection
%% @author Dave Bryson [http://weblog.miceda.org]
%%
-module(websocket_request,[Socket]).
-export([get/1, get_data/0, send/1]).
-define(LOG(L), error_logger:info_report([{line, ?LINE},L])).

%% Get the Socket if you need it
get(socket) ->
    Socket.

%% Return the data from the Socket. Parse it from the WebSocket format
get_data() ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      case parse_data(Data) of
        {tcp_closed} ->
          ?LOG([{close_handshake}]),
          {tcp_closed};
        NewData ->
          {tcp, Socket, base64:decode(NewData)}
      end;
    {error, closed} ->
      {tcp_closed};
    {error, Reason} ->
      ?LOG([{error, Reason}]),
      {error, Reason}
    end.


send(Data) ->
  Data1 = base64:encode(Data),
  send(Data1, size(Data1)),
  ok.

%% Inner Function 
parse_data(<<_Fin:1, _Rsv:3, 8:4, _/binary>>) ->
  {tcp_closed};

parse_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, _Size:16, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []);

parse_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, _Size:7, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []).

unmask_data([], _MaskKey, _Index, Result) ->
  lists:reverse(Result);

unmask_data([H|T], MaskKey, Index, Result) ->
  Unmask = H bxor binary:at(MaskKey, Index rem 4),
  unmask_data(T, MaskKey, Index + 1, [Unmask|Result]).

send(Msg, Size) when Size =< 125 ->
  ?LOG([{msg_size, Size}, {msg, Msg}]),
  %%       FIN  RSV  OPCODE  MASK  SIZE    DATA
  Data = <<1:1, 0:3, 1:4,    0:1,  Size:7, Msg/binary>>,
  gen_tcp:send(Socket, Data);

send(Msg, Size) ->
  ?LOG([{msg_size, Size}, {msg, Msg}]),
  %%       FIN  RSV  OPCODE  MASK  SIZE            DATA
  Data = <<1:1, 0:3, 1:4,    0:1,  126:7, Size:16, Msg/binary>>,
  gen_tcp:send(Socket, Data).
