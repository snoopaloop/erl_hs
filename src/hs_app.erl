-module(hs_app).

-behaviour(application).

%%Callbacks
-export([start/2, stop/1]).

%%Application callbacks
start(_StartType, _StartArgs) ->
	%%error_logger:info_msg("start callback starttype: ~p - startargs: ~p\n", [_StartType, _StartArgs]),
    hs_pool_sup:start_link().

	
stop(_State) ->
	ok.