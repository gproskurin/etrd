-module(etrd_arbitrage).

-export([run/0]).

%%%

get_api_base_url() ->
	{ok, Url} = application:get_env(binance_base_url),
	Url.


geturl_ping() ->
	Url0 = <<(get_api_base_url())/binary, "/ping">>,
	binary_to_list(Url0).


geturl_exchange_info() ->
	Url0 = <<(get_api_base_url())/binary, "/exchangeInfo">>,
	binary_to_list(Url0).


req_ping() ->
	Url = geturl_ping(),
	Resp = httpc:request(Url),
	{ok, {{_HttpVer, 200, _}, _Headers, _Body}} = Resp,
	ok.


req_exchange_info() ->
	Url = geturl_exchange_info(),
	Resp = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
	{ok, {{_HttpVer, 200, _}, _Headers, Body}} = Resp,
	BodyMap = jsx:decode(Body, [return_maps]),
	{ok, AllowedAssets} = application:get_env(binance_allowed_assets),
	Symbols = maps:get(<<"symbols">>, BodyMap),
	SymFilterFun = fun(SymbolMap) ->
		BaseAsset = maps:get(<<"baseAsset">>, SymbolMap, undefined),
		QuoteAsset = maps:get(<<"quoteAsset">>, SymbolMap, undefined),
		IsAllowed = lists:member(BaseAsset, AllowedAssets) andalso lists:member(QuoteAsset, AllowedAssets),
		case IsAllowed of
			true ->
				Symbol = maps:get(<<"symbol">>, SymbolMap),
				NewItem = {Symbol, binary_to_atom(BaseAsset, utf8), binary_to_atom(QuoteAsset, utf8)},
				{true, NewItem};
			false ->
				false
		end
	end,
	NewSymbols = lists:filtermap(SymFilterFun, Symbols),

	{ok, NewSymbols}.


	%
	%G = digraph:new([cyclic, private]),
	%GraphAddVerticesFun = fun(Symbol) ->
	%	B = binary_to_atom(maps:get(<<"baseAsset">>, Symbol), utf8),
	%	Q = binary_to_atom(maps:get(<<"quoteAsset">>, Symbol), utf8),
	%	GetOrAddVertexFun = fun(NewVertex) ->
	%		case digraph:vertex(G, NewVertex) of
	%			{V, _Label} ->
	%				V;
	%			false ->
	%				digraph:add_vertex(G, NewVertex)
	%		end
	%	end,
	%	Vb = GetOrAddVertexFun(B),
	%	Vq = GetOrAddVertexFun(Q),
	%	_Ebq = digraph:add_edge(G, Vb, Vq),
	%	_Eqb = digraph:add_edge(G, Vq, Vb)
	%end,
%-spec req_rate(atom(), atom(), binary()) -> float().
%req_rate(Base, Quote, ExchName) ->


%%%

run() ->
	ok = req_ping(),
	io:format("PING ok~n", []),

	%{ok, Info} = req_exchange_info(),
	%io:format("EXCHANGE_INFO ok:~p~n", [Info]),

	Loops =
	[
		[
			{'USDT', 'BTC', <<"BTCUSDT">>, reverse},
			{'BTC', 'ETH', <<"ETHBTC">>, reverse},
			{'ETH', 'USDT', <<"ETHUSDT">>, direct}
		]
	],

	ok.

%%%
