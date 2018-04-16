-module(gerl_cache_app).
-include("db.hrl").
-include("log.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, SupPid} = gerl_cache_sup:start_link(),
    start_mysql(),
    Ret = db:init_amnesia_database(),
    ?INFO("~p", [Ret]),
    start_gen_cache(),
    {ok, SupPid}.

stop(_State) ->
    ok.


start_mysql() ->
	LogFunc = fun(Level, Format, Arguments) ->
		case Level of
			debug  -> ?DEBUG(Format, Arguments);
			normal -> ?INFO(Format, Arguments);
			error  -> ?ERROR(Format, Arguments)
		end
	end,
	Args = [?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, LogFunc],
	Spec = {mysql, {mysql, start_link, Args}, permanent, 10000, worker, [mysql]},
	{ok, _MysqlPid} = gerl_cache_sup:start_child(Spec),	
	mysql:connect(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, true),
	ok.

start_gen_cache() ->
	[begin StartFunc = {gen_cache, start_link, [cache_config:cache_name(DBMod), DBMod:tables()]},
		   Spec = {"gen_cache" ++ atom_to_list(DBMod), StartFunc, permanent, 10000, worker, [gen_cache]},
		   gerl_cache_sup:start_child(Spec)
	 end || DBMod <- cache_config:table_modules()].
