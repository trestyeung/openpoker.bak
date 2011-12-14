-module(sim).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("common.hrl").
-include("game.hrl").
-include("pp.hrl").
-include("schema.hrl").
-include("texas/ctx.hrl").

init() ->
  %Host = "192.168.1.102",
  Host = "127.0.0.1",
  Port = 8002,

  schema:install(),

  player:create("1010", "pass", "5b635bee55S3", "5Lit5Zu9", 1000), %% Jack
  player:create("1020", "pass", "6buRSkFDSw==", "6aaZ5riv", 1000), %% Sam 
  player:create("1030", "pass", "6buRSkFDSw==", "6aaZ5riv", 1000), %% Sam 
  player:update_photo(1, <<"def_face_1">>),
  player:update_photo(2, <<"def_face_2">>),
  player:update_photo(3, <<"def_face_3">>),

  server:start(Host, Port),
  timer:sleep(2000),
  login(),

  ok.

login() ->
  login:login(<<"1010">>, <<"pass">>, spawn(?MODULE, player_loop, [1])),
  login:login(<<"1020">>, <<"pass">>, spawn(?MODULE, player_loop, [2])),
  login:login(<<"1030">>, <<"pass">>, spawn(?MODULE, player_loop, [3])),

  gen_server:cast(p(1), #player_query{player=p(1)}),
  gen_server:cast(p(2), #player_query{player=p(2)}),
  gen_server:cast(p(3), #player_query{player=p(3)}),

  ok.

join(PID) ->
  gen_server:cast(p(PID), #join{game=g(1), seat=0, amount=700.0}).

call(PID) ->
  gen_server:cast(p(PID), #raise{game=g(1), raise=0.0}).

player_loop(PID) ->
  receive
    {packet, {notify_cancel_game, _}} ->
      player_loop(PID);
    {packet,{bet_req, _GID, _Call, _Min, _Max}} ->
      call(PID),
      player_loop(PID);
    Msg ->
      io:format("Flush [~p]: ~p~n", [PID, Msg]),
      player_loop(PID)
  end.

p(PID) ->
  [P] = db:read(tab_player, PID),
  P#tab_player.process.

g(GID) ->
  [G] = db:read(tab_game_xref, GID),
  G#tab_game_xref.process.

debug(GID) when is_integer(GID) ->
  debug(g(GID));

debug(GID) when is_pid(GID) ->
  gen_server:call(GID, 'DEBUG').

