-module(coin_backend_serv).

-export([start_link/0, init/1]).

-include("coin_records.hrl").

start_link() ->
    Pid = spawn_link(coin_backend_serv, init, [self()]),
    {ok, Pid}.


init(_From) ->
    {ok, _} = dets:open_file(coin, [{type, set}, {file, coin_dets}, {keypos, 2}, {access, read_write}]),
    Status = [],
    loop(Status).

loop(_Status) ->
    Sum = sum(),
    gproc_ps:publish(l, broadcast_sum, Sum),
    timer:sleep(1000),
    loop(_Status).

sum() ->
    dets:foldl(fun(User, Sum) -> Sum + User#user.coin end, 0, coin).
