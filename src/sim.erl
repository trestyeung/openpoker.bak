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

  player:create("1010", "pass", "5b635bee55S3", "5Lit5Zu9", 100000), %% Jack
  player:create("1020", "pass", "6ICB6Yyi", "6aaZ5riv", 100000), %% Sam 
  player:create("1030", "pass", "6buRSkFDSw==", "6aaZ5riv", 100000), %% Sam 
  player:update_photo(1, <<"def_face_1">>),
  player:update_photo(2, <<"def_face_2">>),
  player:update_photo(3, <<"def_face_3">>),

  server:start(Host, Port),
  timer:sleep(2000),
  login(),
  timer:sleep(1000),
  join(2, 500),
  ok.

login() ->
  register(p1, spawn(?MODULE, player_loop, [1])),
  register(p2, spawn(?MODULE, player_loop, [2])),
  register(p3, spawn(?MODULE, player_loop, [3])),

  login:login(<<"1010">>, <<"pass">>, whereis(p1)),
  login:login(<<"1020">>, <<"pass">>, whereis(p2)),
  login:login(<<"1030">>, <<"pass">>, whereis(p3)),

  gen_server:cast(p(1), #player_query{player=p(1)}),
  gen_server:cast(p(2), #player_query{player=p(2)}),
  gen_server:cast(p(3), #player_query{player=p(3)}),

  ok.

join(PID) ->
  gen_server:cast(p(PID), #join{game=g(1), seat=0, amount=1000.0}).

join(PID, Amount) ->
  gen_server:cast(p(PID), #join{game=g(1), seat=0, amount=Amount}).

call(PID, Raise) ->
  gen_server:cast(p(PID), #raise{game=g(1), raise=Raise}).

fold(PID) ->
  gen_server:cast(p(PID), #fold{game=g(1)}).

leave(PID) ->
  gen_server:cast(p(PID), #leave{game=g(1)}).

player_loop(PID) when PID == 2 ->
  receive
    {packet, {bet_req, _GID, Call, Min, Max}} ->
      io:format("BET_REQ ~p [Raise: ~p - ~p Call: ~p] ~n", [PID, Min, Max, Call]);
    {call, Amount} ->
      call(PID, Amount);
    {fold} ->
      fold(PID);
    {leave} ->
      leave(PID);
    _Msg ->
      ok
  end,
  player_loop(PID);
  
player_loop(PID) ->
  receive
    {packet, {notify_raise, _GID, Player, Raise, Call}} ->
      io:format("NOTIFY_RAISE ~p [Raise: ~p Call ~p] ~n", [Player, Raise, Call]);
    {packet, {bet_req, _GID, Call, Min, Max}} ->
      io:format("BET_REQ ~p [Raise: ~p - ~p Call: ~p] ~n", [PID, Min, Max, Call]);
    {packet, {game_stage, _GID, Stage}} ->
      io:format("NEW_STAGE [~p] ~n", [Stage]);
    {packet, {seat_state, _GID, _SN, _State, undefined, _Inplay, _Nick}} ->
      ok;
    {packet, {seat_state, _GID, _SN, State, Player, Inplay, _Nick}} ->
      io:format("SEAT_STATE ~p [State: ~p Inplay: ~p] ~n", [Player, State, Inplay]);
    {call, Amount} ->
      call(PID, Amount);
    {fold} ->
      fold(PID);
    {leave} ->
      leave(PID);
    _Msg ->
      %io:format("Flush [~p]: ~p~n", [PID, Msg]),
      ok
  end,
  player_loop(PID).

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

