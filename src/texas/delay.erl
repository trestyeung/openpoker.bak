-module(delay).

%%% 
%%% Pre-game delay
%%%

-export([start/3, delay/3]).

-include_lib("eunit/include/eunit.hrl").

-include("texas.hrl").

start(Game, Ctx, [Delay]) ->
    Game1 = g:restart_timer(Game, Delay),
    {next, delay, Game1, Ctx}.

delay(Game, Ctx, {timeout, _, _}) ->
    {stop, Game, Ctx};

delay(Game, Ctx, _) ->
    {skip, Game, Ctx}.

