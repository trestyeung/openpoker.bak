-module(names).
-export([get_table_name/1]).

get_table_name(Id) when Id =< 0 ->
  <<"5paw6JGh5Lqs5aib5qiC5aC0">>;

get_table_name(Id) ->
  Def = <<"5paw6JGh5Lqs5aib5qiC5aC0">>,
  Mode = {<<"576F5rWu5a6u5aib5qiC5aC0">>},

  case Id =< tuple_size(Mode) of
    true -> element(Id, Mode);
    false -> Def
  end.
  
  

