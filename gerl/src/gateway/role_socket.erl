%% @author dzR <dizengrong@gmail.com>
%% @doc 管理所有玩家的#role_socket{}记录数据
%% 这里只是对ets表做了下封装，目的是避免暴露数据细节

-module (role_socket).
-include ("gateway.hrl").

-export([init_table/0, insert/4, lookup/1, delete/1]).

init_table() ->
    ets:new(ets_role_socket, [public,named_table,set,{keypos, #role_socket.role_id}]).

insert(RoleId, Socket, Transport, Pid) ->
	Rec = #role_socket{
		role_id   = RoleId,
		socket    = Socket,
		transport = Transport,
		pid       = Pid
	},
	ets:insert(ets_role_socket, Rec).

lookup(RoleId) ->
	ets:lookup(ets_role_socket, RoleId).

delete(RoleId) ->
	ets:delete(ets_role_socket, RoleId).
