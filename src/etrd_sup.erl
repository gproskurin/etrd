%%%-------------------------------------------------------------------
%% @doc etrd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(etrd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    SrvSpec = #{
	id => test,
	start => {etrd_arbitrage, run, []}
    },
    {ok, {SupFlags, [SrvSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
