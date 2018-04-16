-module(gerl_gateway_app).
-include ("gateway.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok             = application:start(ranch),
    NumOfAcceptors = gerl_setting:get(gateway_num_of_acceptor),
    Opts           = get_socket_opts(),
    {ok, _}        = ranch:start_listener(?MODULE, NumOfAcceptors, ranch_tcp, Opts, client_socket, []),
    {ok, SupPid}   = gerl_gateway_sup:start_link(),

    start_gateway_dispatch(),
    % start_map_dispatch(),

    {ok, SupPid}.

stop(_State) -> ok.

start_gateway_dispatch() ->
    role_socket:init_table(),
    StartFun   = {gateway_dispatch, start_link, []},
    Spec       = {gateway_dispatch, StartFun, permanent, 10000, worker, [gateway_dispatch]},
    {ok, _Pid} = gerl_gateway_sup:start_child(Spec).

% start_map_dispatch() ->
%     map_dispatch:init_table(),
%     StartFun   = {map_dispatch, start_link, []},
%     Spec       = {map_dispatch, StartFun, permanent, 10000, worker, [map_dispatch]},
%     {ok, _Pid} = gerl_gateway_sup:start_child(Spec).

get_socket_opts() ->
	[{port, gerl_setting:get(gateway_port)}, 
	 {max_connections, infinity}, 
     {reuseaddr, true}, 
     {nodelay, true}, 
     {delay_send, true},
     {packet, 4}, 
     {active, false}
    ].
