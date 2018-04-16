-module(gerl_chat_sup).
-include ("chat.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Mod, Args), {Id, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, 
    		[
    			?CHILD(?CHANNEL_TEAM, chat_channel, [?CHANNEL_TEAM]),
    			?CHILD(?CHANNEL_WORLD, chat_channel, [?CHANNEL_WORLD]),
    			?CHILD(?CHANNEL_PRIVATE, chat_channel, [?CHANNEL_PRIVATE])
    		]
    	 } 
    }.

