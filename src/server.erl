    R = #pong{} ->
      process_pong(Client, Socket, R);
    R = #start_game{ rigged_deck = [_|_] } ->
      process_test_start_game(Client, Socket, R);
    R when is_record(R, game_query) ->
      process_game_query(Client, Socket, R);
    Event ->
      process_event(Client, Socket, Event)
  end,
  {loop_data, Client1};

parse_packet(_Socket, Event, Client) ->
  ?LOG([{parse_packet, {event, Event}}]),
  {loop_data, Client}.


%%%
%%% Handlers
%%%

%%%
%%% Utility
%%% {{{ 

send_games(_, [], _) ->
    ok;

send_games(Socket, [H|T], C) ->
    N = H#game_info{game_count = C},
    ?tcpsend(Socket, N),
    send_games(Socket, T, C).

find_games(Socket, 
           GameType, LimitType,
           #query_op{ op = ExpOp, val = Expected }, 
           #query_op{ op = JoinOp, val = Joined },
           #query_op{ op = WaitOp, val = Waiting }) ->
    {atomic, L} = g:find(GameType, LimitType,
                         ExpOp, Expected, 
                         JoinOp, Joined,
                         WaitOp, Waiting),
    
    send_games(Socket, L, lists:flatlength(L)).

start_games() ->
    {atomic, Games} = db:find(tab_game_config),
    start_games(Games).

start_games([]) ->
    ok;

start_games([Game|Rest]) ->
    start_games(Game, Game#tab_game_config.max),
    start_games(Rest).

start_games(_Game, 0) ->
    ok;

start_games(Game, N) ->
    g:make(_ = #start_game{ 
             type = Game#tab_game_config.type, 
             limit = Game#tab_game_config.limit, 
             start_delay = Game#tab_game_config.start_delay,
             player_timeout = Game#tab_game_config.player_timeout,
             seat_count = Game#tab_game_config.seat_count
            }),
    start_games(Game, N - 1).

kill_games() ->
    {atomic, Games} = db:find(tab_game_xref),
    kill_games(Games).

kill_games([]) ->
    ok;

kill_games([H|T]) ->
    gen_server:cast(H#tab_game_xref.process, stop),
    kill_games(T).

start_test_game(R) ->
    {ok, Game} = game:start(R),
    GID = game:call(Game, 'ID'),
    #your_game{ game = GID }.

%% }}}

%% 
%% Test suite
%%

test() ->
    ok.

%% vim: fdm=marker
