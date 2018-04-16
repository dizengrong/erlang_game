-module(gerl_map_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = gerl_map_srv_sup:start_link(),
    map_manager:pre_create_maps(),
    {ok, SupPid}.

stop(_State) ->
    ok.
