-module(rank).
-export([start/3]).

-include("texas.hrl").

start(Game, Ctx, _) ->
  Ranks = g:rank_hands(Game),
  notify_hands(Game, Ranks),
  {stop, Game, Ctx}.

notify_hands(_, []) ->
  ok;

notify_hands(Game, [H|T]) ->
  Hand = hand:player_hand(H),
  Event = #notify_hand{
    player = H#hand.pid,
    game = Game#game.gid,
    hand = Hand
  },
  gen_server:cast(pp:id_to_player(H#hand.pid), Event),
  notify_hands(Game, T).
