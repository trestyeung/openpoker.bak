-module(websocket).
-export([encoding/1, decoding/1]).

-include("common.hrl").

encoding(Bin) ->
  Bin1 = base64:encode(Bin),
  coding_data(Bin1, size(Bin1)).

decoding(Bin) ->
  case Data = decoding_data(Bin) of
    {tcp_closed} ->
      {tcp_closed};
    _ ->
      base64:decode(Data)
  end.

%% Inner Function 
decoding_data(<<_Fin:1, _Rsv:3, 8:4, _/binary>>) ->
  {tcp_closed};

decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, 126:7, _Size:16, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []);

decoding_data(<<_Fin:1, _Rsv:3, _Opcode:4, _Mask:1, _Size:7, MaskKey:32, Msg/binary>>) ->
  unmask_data(binary_to_list(Msg), <<MaskKey:32>>, 4, []).

coding_data(Bin, Size) when Size =< 125 ->
  %%       FIN  RSV  OPCODE  MASK  SIZE    DATA
  <<1:1, 0:3, 1:4,    0:1,  Size:7, Bin/binary>>;

coding_data(Bin, Size) ->
  %%       FIN  RSV  OPCODE  MASK  SIZE            DATA
  <<1:1, 0:3, 1:4,    0:1,  126:7, Size:16, Bin/binary>>.

unmask_data([], _MaskKey, _Index, Result) ->
  lists:reverse(Result);

unmask_data([H|T], MaskKey, Index, Result) ->
  Unmask = H bxor binary:at(MaskKey, Index rem 4),
  unmask_data(T, MaskKey, Index + 1, [Unmask|Result]).
