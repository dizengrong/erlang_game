%% @doc 有关网关的一些定义

-record(role_socket, {
	role_id = 0,
	socket,
	transport,		%% the ranch socket transport
	pid				%% 管理socket连接的进程
}).
