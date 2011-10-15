%%% Texas Hold'em game context 

-record(texas, {
					b = none,                       %% button
					sb = none,                      %% small blind
					bb = none,                      %% big blind
					no_sb = false,                            
					sb_all_in = false,
					sb_amt = 0,
					bb_amt = 0,
					sb_bet = 0,
					blind_type = normal,
					exp_player = none,              %% expecting player
					exp_seat = none,                %% expecting seat
					exp_amt = 0,                    %% expecting amount
					exp_min = 0,                    %% expecting min amount
					exp_max = 0,                    %% expecting max amount
					call = 0,
					have_blinds, 
					max_raises,
					stage,                          %% current stage
					deal_type,                      %% current private or share
					deal_count,                      
					winners = none                  %% last winners
				 }).

