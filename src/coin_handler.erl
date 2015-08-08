-module(coin_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("coin_records.hrl").

-define(record_to_list(Record),
    fun(Val) ->
        Fields = record_info(fields, Record),
        [_Tag| Values] = tuple_to_list(Val),
        lists:zip(Fields, Values)
    end
).

-record(state, {user}).

% 参加費
-define(FEE, 5). 

init(_, _, _) ->
    % 乱数初期化
    random:seed(os:timestamp()),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_, Req, _Opts) ->
    % プロセスをgproc pubsubに登録する
    gproc_ps:subscribe(l, broadcast_sum),
    % stateを設定する
    State = #state{user=not_logged_in},
    % WebSocketリクエストは長くなる可能性があるため
    % 不要なデータをReqから削除
    Req2 = cowboy_req:compact(Req),
    % 自動切断を10分に設定する（60万ミリ秒）
    {ok, Req2, State, 600000, hibernate}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% クライアントからのリクエスト
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
websocket_handle({text, Cmd}, Req, #state{user = User} = State) ->
    % io:format("Command = ~s~n", [Cmd]),
    User1 = case Cmd of
		<<"post_bet">> -> cmd_bet(User);
		<<"login:", Buuid:36/binary>> -> cmd_user_login(User, Buuid);
		_ -> User
	    end,
    save(User1),
    State1 = State#state{user = User1},
    Response = make_response(User1, <<"update_status">>),
    {reply, {text, Response}, Req, State1};		    

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% システム側からの呼び出し
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% コインの集計情報
websocket_info({gproc_ps_event, broadcast_sum, Sum}, Req, State) ->
    Response = jiffy:encode(#{
			       <<"type">> => <<"update_sum">>,
			       <<"data">> => Sum
			     }),
    {reply, {text, Response}, Req, State};

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ログイン
cmd_user_login(_User, Buuid) ->
    Uuid = binary_to_list(Buuid),
    io:format("login: uuid = ~s~n", [Uuid]),
    User1 = case find_user(Uuid) of
		{error, Reason} ->
		    io:format("ユーザー検索でエラー：~p~n", [Reason]),
		    not_logged_in;
		[] ->
		    io:format("user ~s not found. create a new user.~n", [Uuid]),
		    create_user(Uuid);
		[Found] ->
		    io:format("user ~s found.~n", [Uuid]),
		    io:format("user status: ~p~n", [Found]),
		    Found
	    end,
    User1.

%  賭ける
cmd_bet(User) ->
    Coin = User#user.coin,
    Combo = User#user.combo,
    MaxCoin = User#user.max_coin,
    MaxCombo = User#user.max_combo,

    % 一回目に投げるときに参加費を払う
    Fee = case Combo of
	      0 -> ?FEE;
	      _ -> 0
	  end,

    case win_or_lose() of
	win ->
	    User#user{
	      coin = Coin - Fee,
	      combo = Combo + 1,
	      max_coin = MaxCoin,
	      max_combo = lists:max([MaxCombo, Combo + 1]),
	      last_game = win
	     };
	lose ->
	    Coin1 = Coin + prize(Combo) - Fee,
	    History = User#user.history,
	    Key = list_to_binary(integer_to_list(Combo)),
	    User#user{
	      coin = Coin1,
	      combo = 0,
	      max_coin = lists:max([MaxCoin, Coin1]),
	      max_combo = MaxCombo,
	      last_game = lose,
	      total_game = User#user.total_game + 1,
	      history = maps:put(Key, maps:get(Key, History, 0) + 1, History)
	     }
    end.

% N回連続表を出した時の賞金
prize(0) -> 0;
prize(N) -> pow(2, N - 1).

% べき乗
pow(_, 0) -> 1;
pow(B, P) -> B * pow(B, P - 1).
     
     
% 抽選
win_or_lose() ->
    Rnd = random:uniform(1000000),
    case Rnd rem 2 =:= 0 of
	true -> win;   % おもて
	false -> lose  % うら
    end.
    

save(User) ->
        dets:insert(coin, User).

find_user(Uuid) ->
    dets:lookup(coin, Uuid).

% ユーザー作成
create_user(Uuid) ->
    User = #user{uuid = Uuid},
    case dets:insert_new(coin, User) of
	true -> User;
	false -> false;
	{error, Reason} ->
	    io:format("ユーザー作成失敗~p~n", [Reason])
    end.

make_response(User, Type) ->
    jiffy:encode(#{<<"type">> => Type,
		   <<"data">> => record_to_map(User)}).

record_to_map(Record) ->
    PropList = (?record_to_list(user))(Record),
        maps:from_list(PropList).
