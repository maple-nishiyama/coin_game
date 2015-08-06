-record(user, {
	  uuid,
	  name = "ななし",
	  coin = 0,
	  combo = 0,
	  max_coin = 0,
	  max_combo = 0,
	  last_game = undefined,
	  total_game = 0,
	  history = maps:new()
	 }).
