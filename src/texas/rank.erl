-module(rank).

-export([start/3]).

-include("texas.hrl").

start(Game, Ctx, _) ->
  ?LOG([{rank_module, log}]),
  {stop, Game, Ctx}.

