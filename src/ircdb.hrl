-record(irc_player, {
					usr, 
					%% [action1, action2, ...]
					actions,
					cards,
					total_action,
					balance,
					win
				 }).

-record(irc_game, {
					id,
					player_count,
					%% [{#players, pot$}, ...]
					stages, 
					board,
					players
				 }).

