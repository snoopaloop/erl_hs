-module(hs_pool_sup).
-behaviour(supervisor).

-export([start_link/0, start_connection/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
	
start_connection() ->
	supervisor:start_child(?MODULE, []).
	
init(_Args) ->
	%% get host port and opts here
	{ok, Host} = application:get_env(hs_app, host),
	{ok, Port} = application:get_env(hs_app, read_port),
	{ok, MaxConns} = application:get_env(hs_app, max_conns),
	Opts = [],
%%	error_logger:info_msg("Starting hs_conn_manager\n"),
	{ ok, { {one_for_one, 10, 10},
		[ 
		  { hs_conn_sup,
		    { hs_conn_sup, start_link, [ {Host, Port, Opts} ] }, 
			permanent, 2000, supervisor, [ hs_conn_sup ] 
		  },
		  { hs_pool_manager, 
			{ hs_pool_manager, start_link, [MaxConns] },
			permanent, 2000, worker, [ hs_pool_manager ]
		  }
		]
	} }.