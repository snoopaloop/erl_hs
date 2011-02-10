-module(hs_pool_manager).
-behaviour(gen_server).

-export([start_link/1, start_connection/3, get_connection/0]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-include("hs.inc.hrl").

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).
	
start_connection(Host, Port, _Opts) ->
%%	error_logger:info_msg("hs_pool_manager:start_connection Host ~p Port ~p\n", [Host, Port]),
	case hs:start_link(Host, Port) of
		{ok, Pid} ->
%%			error_logger:info_msg("conn succeeds, process pid ~p\n", [Pid]),
			case pg2:join(read_conn, Pid) of
				ok ->
%%					error_logger:info_msg("successfully joined pg group\n"),
					{ok, Pid};
				{error, Reason} ->
%%					error_logger:info_msg("pg2 failure ~p\n", [Reason]),
					hs:close(Pid),
					{error, Reason}
			end;
		{error, Conn} ->
			{error, Conn}
	end.

get_connection() ->
	pg2:get_closest_pid(read_conn).
	
init([Maxconns]) ->
%%	error_logger:error_msg("starting init\n"),
	pg2:create(read_conn),
	[ {ok, _} = hs_conn_sup:start_connection() || _ <- lists:seq(1, Maxconns) ],
	{ok, #state {max_r_conns = Maxconns}}.
	
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Request, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
