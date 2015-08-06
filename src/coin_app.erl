-module(coin_app).
-behaviour(application).

-export([start/2, start_all/0]).
-export([stop/1]).

start_all() ->
    application:ensure_all_started(coin).


start(_Type, _Args) ->

    % ETSテーブルを初期化
    {ok, _} = dets:open_file(coin, [{type, set}, {file, coin_dets}, {keypos, 2}]),

    % ルート宣言
    Dispatch = cowboy_router:compile([
    {'_', [
           % cowboy_staticはパスマッチに対して、静的ファイルを読み込む
           % index.htmlを読み込む
	   {"/", cowboy_static, {priv_file, coin, "index.html"}},
           % /websocketのリクエストをcoin_handlerに渡す
	   {"/websocket", coin_handler, []}
	  ]}
	]),
    % Cowboyを起動
    {ok, _} = cowboy:start_http(http, 100, 
        [{port, 8001}],
        [
          {env, [{dispatch, Dispatch}]}
        ]),

    coin_sup:start_link().

stop(_State) ->
	ok.
