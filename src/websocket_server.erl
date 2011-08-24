-module(websocket_server).
-export([start/0, stop/0, loop/2]).
-define(LOG(L), error_logger:info_report([{line, ?LINE},L])).

start() ->
  State = 1,
  Loop = fun(WebSocket) ->
      ?MODULE:loop(WebSocket, State)
  end,
  mochiweb_websocket:start("127.0.0.1", 8002, Loop).

stop() ->
  mochiweb_websocket:stop(8002).

loop(WebSocket, State) ->
  Data = WebSocket:get_data(),
  State1 = State + 1,
  ?LOG([{recv_data, Data}, {state, State}]),
  NewData = <<State1:8>>,
  WebSocket:send(NewData),
  loop(WebSocket, State1).
