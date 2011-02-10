%% socket state.
-record(sock_state, {
	sock, 
	err, 
	ecount, 
	idx,
	host,
	port}).
	
	%% gen_server state. Needs index of sock_state (Round-robin, thing)
-record(state, {
	sock_r, 
	sock_w, 
	init_params, %% dict or proplist
	max_r_conns,
	e_tot, 
	e_last_c, 
	e_last_m,
	idx_sock %% mapping of idx to socket pid
	}).
	
%default params
-define(R_MIN_CONN, 1).
-define(R_MAX_CONN, 10).
-define(W_MIN_CONN, 1).
-define(W_MAX_CONN, 1).

%errors
% "fld" - Unknown field
% "op" - Bad Op

%command params
-define(EQUAL,	 <<"=">>).
-define(GTHAN, 	 <<">">>).
-define(GTEQUAL, <<">=">>).
-define(LTHAN, 	 <<"<">>).
-define(LTEQUAL, <<"<=">>).

-define(HT, 	 <<"\t">>).
-define(LF, 	 <<"\n">>).
-define(O_IDX, 	 <<"P">>).
-define(D_OP, 	 <<"D">>).
-define(U_OP, 	 <<"U">>).
-define(I_OP, 	 <<"+">>).
-define(NUL, 	 <<"0">>).