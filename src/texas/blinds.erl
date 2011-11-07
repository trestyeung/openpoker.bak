-module(blinds).
-export([start/3, small_blind/3, big_blind/3]).

-include("texas.hrl").

start(Game, Ctx, []) ->
  start(Game, Ctx, [normal]);

start(Game, Ctx, [Type]) ->
  {Small, Big} = (Game#game.limit):blinds(Game#game.low, Game#game.high),
  ?LOG([{small, Small, big, Big, type, Type}]),

  Ctx1 = Ctx#texas{
    sb_amt = Small, bb_amt = Big, sb_bet = 0.0,
    no_sb = false, sb_all_in = false, blind_type = Type 
  },

  %% advance button and broadcast position
  Button = advance_button(Game, Ctx1),
  Game1 = g:broadcast(Game, 
    #notify_button{ game = Game#game.gid, button = Button }
  ),

  %% 确定大小盲并下盲注
  AllPlayers = g:get_seats(Game1, Button, ?PS_ACTIVE),
  L = length(AllPlayers),
  HeadsUp = (L == 2), %% 除庄家外只有一个玩家

  ?LOG([{button, Button, all_players, L, AllPlayers}]),

  if
    L < 2 ->
      {goto, top, Game1, Ctx1};
    HeadsUp ->
      %% 一对一时特殊规则生效
      %% 庄家下小盲注，对家下大盲注
      %% 首次行动由庄家先叫，之后每次都为对家先叫
      ?LOG([{heads_up, {button, Button}, {sb, Button}}]),

      Ctx2 = Ctx1#texas{ b = Button, headsup = true },
      ask_for_blind(Game1, Ctx2, Button, Ctx2#texas.sb_amt, small_blind);
    true ->
      Ctx2 = Ctx1#texas{ b = Button, sb = hd(AllPlayers) },
      ask_for_blind(Game1, Ctx2, Ctx2#texas.sb, Ctx2#texas.sb_amt, small_blind)
  end.

%% small blind state
small_blind(Game, Ctx, #raise{ player = Player }) 
  when Ctx#texas.exp_player /= Player ->
    ?LOG([{small_blind, {not_our_player}}]),
    {continue, Game, Ctx};

small_blind(Game, Ctx, R = #raise{ raise = Raise }) when Raise == 0.0 ->
  post_sb(Game, Ctx, R);

small_blind(Game, Ctx, R = #join{}) ->
  join(Game, Ctx, R);

small_blind(Game, Ctx, R = #leave{}) ->
  leave(Game, Ctx, R, small_blind);

small_blind(Game, Ctx, R = #watch{}) ->
  watch(Game, Ctx, R);

small_blind(Game, Ctx, R) when 
  is_record(R, sit_out); 
  is_record(R, come_back) ->
    {skip, Game, Ctx};

small_blind(Game, Ctx, Event) ->
  ?LOG([{small_blind, unknown_event, Event}]),
  {continue, Game, Ctx}.

%%% big blind
big_blind(Game, Ctx, #raise{ player = Player }) when 
  Ctx#texas.exp_player /= Player ->
    {continue, Game, Ctx};

big_blind(Game, Ctx, R = #raise{ raise = Raise }) when Raise == 0.0 ->
  post_bb(Game, Ctx, R);

big_blind(Game, Ctx, R) when
  is_record(R, sit_out); 
  is_record(R, come_back) ->
    {skip, Game, Ctx};

big_blind(Game, Ctx, R = #join{}) ->
  join(Game, Ctx, R);

big_blind(Game, Ctx, R = #leave{}) ->
  leave(Game, Ctx, R, big_blind);

big_blind(Game, Ctx, R = #watch{}) ->
  watch(Game, Ctx, R);

big_blind(Game, Ctx, Event) ->
  ?LOG([{big_blind, unknown_event, Event}]),
  {continue, Game, Ctx}.

%%
%% Utility
%%
advance_button(Game, Ctx) ->
  case Ctx#texas.b of
    none ->
      %% 新的牌局开始时庄家自动选择
      ?LOG([{advance_button, new}]),
      AllPlayers = g:get_seats(Game, ?PS_PLAY),
      lists:last(AllPlayers);
    _ ->
      ?LOG([{advance_button, next}]),
      Players = g:get_seats(Game, Ctx#texas.b, ?PS_PLAY),
      hd(Players)
  end.

ask_for_blind(Game, Ctx, N, Amount, State) ->
  Seat = g:get_seat(Game, N),
  Player = Seat#seat.player,
  Ctx1 = Ctx#texas{ exp_player = Player, exp_seat = N, exp_amt = Amount },

  %% 每局自动下盲注
  R = #raise{ player = Player, raise = 0.0 },
  Game1 = g:cancel_timer(Game),

  case State of
    small_blind -> 
      g:broadcast(Game1, _ = #notify_sb{sb = N, game = Game1#game.gid}),
      post_sb(Game1, Ctx1, R);
    big_blind ->
      g:broadcast(Game1, _ = #notify_bb{bb = N, game = Game1#game.gid}),
      post_bb(Game1, Ctx1, R)
  end.

post_sb(Game, Ctx, #raise{ player = Player, raise = 0.0 }) ->
  N = Ctx#texas.exp_seat,
  Amt = Ctx#texas.exp_amt,
  Seat = g:get_seat(Game, N),

  %% 为玩家下小盲注
  Ctx1 = Ctx#texas{ sb = N, sb_bet = Amt },
  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_raise { 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt,
      raise = 0.0
    }),

  Game3 = g:notify_state(Game2, N),

  %% 问玩家下大盲注
  BBPlayers = g:get_seats(Game3, N, ?PS_ACTIVE),
  ask_for_blind(Game3, Ctx1, hd(BBPlayers), Ctx1#texas.bb_amt, big_blind).

post_bb(Game, Ctx, #raise{ player = Player, raise = 0.0 }) ->
  N = Ctx#texas.exp_seat,
  Amt = Ctx#texas.exp_amt,
  Seat = g:get_seat(Game, N),

  Ctx1 = Ctx#texas{ bb = N, call = Amt,
    %% 由其他模块决定谁应该行动
    exp_seat = none,
    exp_player = none,
    exp_amt = 0.0
  },

  %% 为玩家下小盲注
  Game1 = g:add_bet(Game, Player, Amt),
  Game2 = g:broadcast(Game1, #notify_raise{ 
      game = Game1#game.gid, 
      player = Seat#seat.pid,
      call = Amt,
      raise = 0.0
    }
  ),
  Game3 = g:notify_state(Game2, N),

  ?LOG([{stop_blind, {ctx, Ctx1}}]),

  %% 结束盲注
  {stop, Game3, Ctx1}.

watch(Game, Ctx, R) -> 
  Game1 = g:watch(Game, Ctx, R),
  {continue, Game1, Ctx}.

join(Game, Ctx, R) ->
  join(Game, Ctx, R, ?PS_MAKEUP_BB).

join(Game, Ctx, R, State) ->
  Game1 = g:join(Game, R#join{ state = State }),
  {continue, Game1, Ctx}.

leave(Game, Ctx, R, State) ->
  Player = R#leave.player,
  {Seat, _} = g:get_seat(Game, Player),
  PS = if
    (State == big_blind) and (Seat == Ctx#texas.sb) ->
      %% fold and leave next time 
      %% a bet is requested from us
      ?PS_CAN_LEAVE;
    true ->
      %% leave now
      ?PS_ANY
  end,
  Game1 = g:leave(Game, R#leave{ state = PS }),
  {continue, Game1, Ctx}.
