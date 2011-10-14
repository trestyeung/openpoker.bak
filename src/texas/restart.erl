-module(restart).

-export([start/3]).

start(Game, Ctx, []) ->
    {goto, top, Game, Ctx}.


