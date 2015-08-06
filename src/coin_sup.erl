-module(coin_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		 {backend_serv,
		  {coin_backend_serv, start_link, []},
		  permanent,
		  5000,
		  worker,
		  [coin_backend_serv]
		 }
		],
	{ok, {{one_for_one, 1, 5}, Procs}}.

