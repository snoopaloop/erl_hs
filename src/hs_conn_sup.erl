-module (hs_conn_sup).
-behaviour (supervisor).
-export ([start_link/1, start_connection/0, init/1]).

start_link({Ip, Port, _Opts}) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Ip, Port, _Opts]).

start_connection () ->
  supervisor:start_child (?MODULE, []).

init([Ip, Port, _Opts]) ->
  { ok, { { simple_one_for_one, 10, 10 },
          [ { conns,
              { hs_pool_manager, start_connection, [Ip, Port, _Opts] },
              transient, 2000, worker, dynamic
            }
          ]
    } }.