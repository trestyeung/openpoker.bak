-module(limit).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{raise, 5}, {blinds, 2}].

