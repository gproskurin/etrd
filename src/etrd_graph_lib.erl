-module(etrd_graph_lib).

-export([new_graph/0, new_edge/3, add_edge/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%

-type vertex() :: atom().
-type edge_weight() :: integer().
-type edge() :: {vertex(), vertex(), edge_weight()}.
-type graph() :: [edge()]. % support only connected graphs, no vertices list is needed
-type path() :: [vertex()].

%%%

%%
-spec new_graph() -> graph().
new_graph() ->
	[].

%%
-spec new_edge(vertex(), vertex(), edge_weight()) -> edge().
new_edge(V1, V2, W) ->
	{V1, V2, W}.

%%
-spec add_edge(graph(), edge()) -> graph().
add_edge(G, E) ->
	[E | G].

-spec add_edges(graph(), [edge()]) -> graph().
add_edges(G, Edges) ->
	NewG = lists:foldl(
		fun(E, AccG) -> add_edge(AccG, E) end,
		Edges,
		G
	),
	NewG.

%%
-spec find_cycles(graph(), vertex(), integer()) -> [path()].
find_cycles(G, Vfirst, MaxEdges) when MaxEdges > 0 ->
	find_cycles_impl(G, Vfirst, [Vfirst], MaxEdges).


%%

-spec find_cycles_impl(graph(), vertex(), path(), integer()) -> [path()].

find_cycles_impl(G, Vfirst, [Vfirst | _] = CurPath, RestEdges) ->
	[CurPath];

find_cycles_impl(G, Vfirst, _CurPath, 0 = _RestEdges) ->
	[];

find_cycles_impl(G, Vfirst, CurPath, RestEdges) ->
	[Vlast | _] = CurPath,
	Vnext0 = vertices_from(G, Vlast),
	Vnext = Vnext0 -- (CurPath -- [Vfirst]), % XXX optimize
	Paths = lists:map(
		fun(Vn) ->
			find_cycles_impl(G, Vfirst, [Vn | CurPath], RestEdges-1)
		end,
		Vnext
	),
	[P || P <- Paths, P =/= []].

%%%
% Internal
%%%

%%
-spec vertices_from(graph(), vertex()) -> [vertex()].
vertices_from(G, V) ->
	lists:filtermap(
		fun({Vsrc, Vdst, _W}) ->
			case Vsrc of
				V -> {true, Vdst};
				_ -> false
			end
		end,
		G
	).

%%%
% Tests
%%%

-ifdef(TEST).

add_edges_test() ->
	G = add_edges(new_graph(), [
		new_edge(v1, v2, 1),
		new_edge(v1, v3, 1),
		new_edge(v1, v4, 1),
		new_edge(v2, v3, 1)
	]),
	?assertEqual(
		G,
		[
			{v1, v2, 1},
			{v1, v3, 1},
			{v1, v4, 1},
			{v2, v3, 1}
		]
	),
	ok.

vertices_from_test() ->
	G = add_edges(new_graph(), [
		new_edge(v1, v2, 1),
		new_edge(v1, v3, 1),
		new_edge(v1, v4, 1),
		new_edge(v2, v3, 1)
	]),
	?assertEqual(
		lists:sort([v2, v3, v4]),
		lists:sort(vertices_from(G, v1))
	),
	?assertEqual(
		[v3],
		vertices_from(G, v2)
	),
	ok.

find_cycles_1_test() ->
	?debugFmt("TEST1~n", []),
	G = add_edges(new_graph(), [
		new_edge(v, v1, 1),
		new_edge(v1, v2, 1),
		new_edge(v2, v3, 1)
	]),
	?assertEqual(
		[],
		find_cycles(G, v, 2)
	),
	ok.

%find_cycles_2_test() ->
%	?debugFmt("TEST2~n", []),
%	G = add_edges(new_graph(), [
%		new_edge(v1, v2, 1),
%		new_edge(v2, v1, 1)
%	]),
%	?assertEqual(
%		[[v1, v2, v1]],
%		find_cycles(G, v1, 2)
%	),
%	ok.

%find_cycles_test() ->
%	G = add_edges(new_graph(), [
%		new_edge(v1, v2, 1),
%		new_edge(v2, v3, 1),
%		new_edge(v1, v4, 1),
%		new_edge(v3, v4, 1),
%		new_edge(v4, v1, 1)
%	]),
%	Paths = find_cycles(G, v1, 10),
%	?debugFmt("G:~p~nP:~p~n", [G, Paths]),
%	ok.

-endif.

%%%
