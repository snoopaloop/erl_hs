-module(hs).

-behaviour(gen_server).
-author("Joseph Lambert <joseph@thenetcircle.com>").

-export([start_link/2, start_link/3, init/1, terminate/2,
		handle_call/3, handle_cast/2, handle_info/2, code_change/3, 
		open_index/6, close/1,
		select/4, select/5, select/6,
		delete/4, delete/5, delete/6,
		update/7, insert/3]).
		
-include("hs.inc.hrl").

%%--------------------------
%% public API
%%--------------------------

start_link(Host, Port) ->
	start_link(Host, Port, []).
	
start_link(Host, Port, Opts) ->
	gen_server:start_link(?MODULE, [Host, Port, Opts], []).

close(Pid) ->
    gen_server:call(Pid, stop).

open_index(Pid, Idxid, Dbname, Tblname, Idxname, Columns) ->
	Str = [?O_IDX, ?HT, Idxid, ?HT, Dbname, ?HT, Tblname, ?HT, Idxname, ?HT, Columns, ?LF],
	gen_server:call(Pid, {open_index, Str}, infinity).
	
select(Pid, Idxid, Op, Params) ->
	select(Pid, Idxid, Op, Params, "1", "0").
select(Pid, Idxid, Op, Params, Limit) ->
	select(Pid, Idxid, Op, Params, Limit, "0").
select(Pid, Idxid, Op, Params, Limit, Offset) ->
	Str = [Idxid, ?HT, Op, ?HT, "1", ?HT, Params, ?HT, Limit, ?HT, Offset, ?LF],
	gen_server:call(Pid, {select, Str}, infinity). %% should be a setting

%getting error code 2: stmtnum
insert(State, Idxid, Params) ->
	Str = [Idxid, ?HT, ?I_OP, ?HT, length(Params), ?HT, arr_to_string(Params, [], ?HT), ?LF],
%%	error_logger:error_msg("insert string ~p\n", [Str]),
	case gen_tcp:send(State#sock_state.sock, Str) of
		ok ->
			{ok, Bin} = recv_bin(State#sock_state.sock, []),
			{ok, Bin};
		{error, Reason} ->
			{error, Reason}
	end.	

%getting error code 2: stmtnum - remove space between fields and commas
delete(State, Idxid, Op, Params) ->
	delete(State, Idxid, Op, Params, "1", "0").
delete(State, Idxid, Op, Params, Limit) ->
	delete(State, Idxid, Op, Params, Limit, "0").
delete(State, Idxid, Op, Params, Limit, Offset) ->
	Str = list_to_binary([Idxid, ?HT, Op, ?HT, "1", ?HT, Params, ?HT, Limit, ?HT, Offset, ?HT, ?D_OP, ?LF]),
	case gen_tcp:send(State#sock_state.sock, Str) of
		ok ->
			{ok, Bin} = recv_bin(State#sock_state.sock, []),
			{ok, Bin};
		{error, Reason} ->
			{error, Reason}
	end.

update(State, Idxid, Op, Params, Limit, Offset, Mparams) ->
	Str = list_to_binary([Idxid, ?HT, Op, ?HT, "1", ?HT, Params, ?HT, Limit, ?HT, Offset, ?HT, ?U_OP, ?HT, arr_to_string(Mparams, [], ?HT), ?LF]),
	case gen_tcp:send(State#sock_state.sock, Str) of
		ok ->
			{ok, Bin} = recv_bin(State#sock_state.sock, []),
			{ok, Bin};
		{error, Reason} ->
			{error, Reason}
	end.

%%--------------------------
%% get_server callbacks
%%--------------------------

init([Host, Port, _Opts]) ->
	case connect(Host, Port) of
		{error, State} ->
			{stop, State#sock_state.err};
		Ok ->
			Ok
	end.

%% we need to handle statistics and checking if the socket is busy, etc.
handle_call({select, Req}, _From, State) ->
	case gen_tcp:send(State#sock_state.sock, Req) of
		ok ->
			case recv_bin(State#sock_state.sock, []) of
				{ok, Bin} ->
					{ reply, { ok, parse_resp(Bin) }, State };
				{error, Reason} ->
					{ reply, { error, Reason}, State }
			end;
		{error, Reason} ->
			{ reply, {error, Reason}, State }
	end;

%% we need to record which index is open in this socket. 
handle_call({open_index, Req}, _From, State) ->
%%	error_logger:info_msg("openindex string ~p\n", [Str]),
	case gen_tcp:send(State#sock_state.sock, Req) of
		ok ->
			case recv_bin(State#sock_state.sock, []) of
				{ok, Bin} ->
					case check_for_error(Bin) of
						ok ->
							{ reply, ok, State};
						{error, Reason} ->
							{ reply, {error, Reason}, State }
					end;
				{error, Reason} ->
					{ reply, {error, Reason}, State}
			end;
		{error, Reason} ->
			{reply, {error, Reason}, State }
	end;
handle_call(stop, _From, State) ->
    disconnect(State),
    {stop, normal, ok, State}.
	
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Request, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------
%% private functions
%%--------------------------
parse_resp(Str) when is_list(Str) ->
	parse_resp(list_to_binary(Str));
parse_resp(Str) when is_binary(Str) ->
	case check_for_error(Str) of
		ok ->
			parse_response(Str);
		{error, Reason} ->
			{error, Reason}
	end.

check_for_error(Str) ->
%%	error_logger:info_msg("checkerrorfirst ~p\n", [binary:part(Str, 0, 1)]),
	case binary:part(Str, 0, 1) of
		<<"0">> ->
			ok;
		_ ->
			%[Code, ?HT, Len, ?HT, Msg, ?LF] = Str,
			[Code, _, Msg] = binary:split(Str, ?HT, [global]),
			{error, {Code, binary:part(Msg, 0, size(Msg)-1)}}
	end.

%% if we got here, it's a good value, so we need to remove the 0\t"numcol"\t
%% we should keep the numcol
parse_response(Str) ->
	<<_:2/binary, Cols:8/integer, _:1/binary, Rest/binary>> = Str,
%%	error_logger:error_msg("some bin shit ~p ~p\n", [<<Cols>>, Rest]),
	Lists = binary:split(binary:part(Rest, 0, size(Rest)-1), ?HT, [global]),
	chunk_response_list(list_to_integer([Cols]), Lists, []).

%% make lists of tuples
chunk_response_list(_, [], Acc) -> Acc;
chunk_response_list(Cols, List, Acc) ->
	{Chunk, Rest} = lists:split(Cols, List),
	chunk_response_list(Cols, Rest, Acc++[list_to_tuple(Chunk)]).
	
connect(Server, Port) ->
	State = #sock_state{host=Server, port=Port},
	%% make the socket stuff optional
	case gen_tcp:connect(Server, Port, [binary, {packet, 0}, {active, false}]) of
		{ok, Socket} ->
			{ok, State#sock_state{sock=Socket}};
		{error, Reason} ->
			{error, State#sock_state{err=Reason, ecount=1}}
	end.
	
disconnect(State) ->
	case gen_tcp:close(State#sock_state.sock) of
		ok ->
			{ok, State};
		{error, Reason} ->
			{error, Reason}
	end.

recv_bin(Sock, _) ->
	case gen_tcp:recv(Sock, 0, 5000) of
		{ok, B} ->
%%			error_logger:info_msg("string ~p\n", [B]),
			{ok, B};
%			recv_bin(Sock, [Bs, B]);
		{error, Closed} ->
			{error, Closed}
	end.

arr_to_string([H|T], [], Sep) ->
	arr_to_string(T, lists:append(H, Sep), Sep);
arr_to_string([H|T], Acc, Sep) ->
	arr_to_string(T, lists:append(Acc, lists:append(H, Sep)), Sep);
arr_to_string([], Acc, _Sep) ->
	Acc.