%%
%% mysql_drv.erl
%%
%% ---------------------------------------------------------------------------
%%
%% AMNESIA Copyright (C) 2008-09 Corrado Santoro, Vincenzo Nicosia
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   - Redistributions of source code must retain the above copyright notice,
%%     this list of conditions and the following disclaimer.
%%
%%   - Redistributions in binary form must reproduce the above copyright
%%     notice, this list of conditions and the following disclaimer in the
%%     documentation and/or other materials provided with the distribution.
%%
%% The name of the author may not be used to endorse or promote products
%% derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
%% NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%% PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%% LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% ---------------------------------------------------------------------------
%%
-module (mysql_drv).

-include("mysql.hrl").

% -behaviour (amnesia_driver).

-export ([open/2,
          open/3,
          execute/2,
          close/1,
          drop_database/2,
          create_database/2,
          create_user/2,
          drop_user/2,
          sequence_field/0,
          table_create_options/0,
          field_option/2,
          field_type/1,
          insert/5,
          update/6,
          delete/3,
          has_join/2,
          begin_transaction/1,
          commit/1,
          rollback/1]).

-define(PORT, 3306).

%% ===================================================================
%%  Function: open/2
%%   Returns: {ok, Pid} |{error, Reason}
%% ===================================================================
open (DBName, DBMSOptions) ->
    crypto:start(),
    case proplists:get_value (logging, DBMSOptions) of
        true ->
            mysql_conn:start_link (proplists:get_value (host, DBMSOptions),
                                   ?PORT,
                                   proplists:get_value (user, DBMSOptions),
                                   proplists:get_value (password, DBMSOptions),
                                   atom_to_list (DBName),
                                   undefined);
        _ ->
            mysql_conn:start_link (proplists:get_value (host, DBMSOptions),
                                   ?PORT,
                                   proplists:get_value (user, DBMSOptions),
                                   proplists:get_value (password, DBMSOptions),
                                   atom_to_list (DBName),
                                   fun (_,_,_) -> ok end)
    end.


%% ===================================================================
%%  Function: open/3
%%   Returns: {ok, Pid} |{error, Reason}
%% ===================================================================
open (DBName, DBMSOptions, use_dba) ->
    crypto:start(),
    case proplists:get_value (logging, DBMSOptions) of
        true ->
            mysql_conn:start_link (proplists:get_value (host, DBMSOptions),
                                   ?PORT,
                                   proplists:get_value (dba_user, DBMSOptions),
                                   proplists:get_value (dba_password,
                                                        DBMSOptions),
                                   atom_to_list (DBName),
                                   undefined);
        _ ->
            mysql_conn:start_link (proplists:get_value (host, DBMSOptions),
                                   ?PORT,
                                   proplists:get_value (dba_user, DBMSOptions),
                                   proplists:get_value (dba_password,
                                                        DBMSOptions),
                                   atom_to_list (DBName),
                                   fun (_,_,_) -> ok end)
    end.


%% ===================================================================
%%  Function: execute/2
%% ===================================================================
execute (Pid, Statement) ->
    %% io:format ("[~p] Execute: ~p~n", [?MODULE, Statement]),
    Reply =
        case mysql_conn:fetch (Pid, Statement, self()) of
            {data, MysqlRes} ->
                {ok, MysqlRes#mysql_result.rows};
            {updated, MysqlRes} ->
                {updated, MysqlRes};
            {error, {_, _, _, _, Msg}} ->
                {error, Msg};
            _Other ->
                io:format ("~p~n", [_Other]),
                {error, generic}
        end,
    Reply.


%% ===================================================================
%%  Function: close/1
%% ===================================================================
close (Pid) ->
  Pid ! {stop}.


%% ===================================================================
%%  Function: create_database/2
%% ===================================================================
create_database (DBName, DBMSOptions) ->
  Host = proplists:get_value (host, DBMSOptions),
  User = proplists:get_value (dba_user, DBMSOptions),
  Password = proplists:get_value (dba_password, DBMSOptions),
  Database = atom_to_list (DBName),
  Command = lists:flatten (["mysqladmin",
                            " --host=", Host,
                            " --password=", Password,
                            " --user=", User,
                            " create ", Database]),
  %% io:format ("~p~n", [Command]),
  os:cmd (Command),
  ok.


%% ===================================================================
%%  Function: drop_database/2
%% ===================================================================
drop_database (DBName, DBMSOptions) ->
  Host = proplists:get_value (host, DBMSOptions),
  User = proplists:get_value (dba_user, DBMSOptions),
  Password = proplists:get_value (dba_password, DBMSOptions),
  Database = atom_to_list (DBName),
  Command = lists:flatten (["mysqladmin",
                            " --host=", Host,
                            " --password=", Password,
                            " --user=", User,
                            " --force",
                            " drop ", Database]),
  %% io:format ("~p~n", [Command]),
  os:cmd (Command),
  ok.


%% ===================================================================
%%  Function: create_user/2
%% ===================================================================
create_user (DBName, DBMSOptions) ->
  Host = proplists:get_value (host, DBMSOptions),
  Username = proplists:get_value (user, DBMSOptions),
  Password = proplists:get_value (password, DBMSOptions),
  DBA_User = proplists:get_value (dba_user, DBMSOptions),
  DBA_Password = proplists:get_value (dba_password, DBMSOptions),
  Database = atom_to_list (DBName),

  %% create user
  Command1 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\" create user ", Username, "\""]),
  %% io:format ("~p~n", [Command1]),
  Res1 = os:cmd (Command1),
  io:format ("create user: ~p~n", [Res1]),

  %% set password
  Command2 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\"set password for '",
                             Username, "'@'%' = ",
                             " password('", Password, "')\"" ]),
  %% io:format ("~p~n", [Command2]),
  Res2 = os:cmd (Command2),
  io:format ("set password: ~p~n", [Res2]),

  %% grant privileges
  Command3 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\"grant all privileges on ",
                             Database, ".* "
                             " to '", Username, "'@'%' \""]),
  %% io:format ("~p~n", [Command2]),
  Res3 = os:cmd (Command3),
  io:format ("grant privileges: ~p~n", [Res3]),

  %% ---------------------------------------------

  %% create user@localhost
  Command4 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\" create user ", Username, "@localhost\""]),
  %% io:format ("~p~n", [Command1]),
  Res4 = os:cmd (Command4),
  io:format ("create user@localhost: ~p~n", [Res4]),

  %% set password
  Command5 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\"set password for '",
                             Username, "'@localhost = ",
                             " password('", Password, "')\"" ]),
  %% io:format ("~p~n", [Command2]),
  Res5 = os:cmd (Command5),
  io:format ("set password@localhost: ~p~n", [Res5]),

  %% grant privileges
  Command6 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\"grant all privileges on ",
                             Database, ".* "
                             " to '", Username, "'@localhost \""]),
  %% io:format ("~p~n", [Command2]),
  Res6 = os:cmd (Command6),
  io:format ("grant privileges@localhost: ~p~n", [Res6]),

  ok.


%% ===================================================================
%%  Function: drop_user/2
%% ===================================================================
drop_user (_DBName, DBMSOptions) ->
  Host = proplists:get_value (host, DBMSOptions),
  Username = proplists:get_value (user, DBMSOptions),
  DBA_User = proplists:get_value (dba_user, DBMSOptions),
  DBA_Password = proplists:get_value (dba_password, DBMSOptions),

  %% drop user
  Command1 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\" drop user ", Username, "\""]),
  %% io:format ("~p~n", [Command1]),
  Res1 = os:cmd (Command1),
  io:format ("drop user: ~p~n", [Res1]),

  %% drop user
  Command2 = lists:flatten (["mysql",
                             " --host=", Host,
                             " --password=", DBA_Password,
                             " --user=", DBA_User,
                             " --execute=\" drop user ", Username, "@localhost\""]),
  %% io:format ("~p~n", [Command2]),
  Res2 = os:cmd (Command2),
  io:format ("drop user@localhost: ~p~n", [Res2]),

  ok.


%% ===================================================================
%%  Function: sequence_field/0
%% ===================================================================
sequence_field () -> "id integer auto_increment primary key not null".


%% ===================================================================
%%  Function: table_create_options/0
%% ===================================================================
table_create_options () -> " engine=innodb CHARSET=utf8".


%% ===================================================================
%%  Function: field_option/2
%% ===================================================================
field_option (unique, _) -> "unique";
field_option (auto_increment, _) -> "auto_increment";
field_option (not_null, _) -> "not null";
field_option (null, _) -> "null";
field_option ({default, Value}, Type = term) ->
  "default " ++ amnesia:cast_to_sql (Value, Type);
field_option ({default, Value}, Type = {term, _}) ->
  "default " ++ amnesia:cast_to_sql (Value, Type);
field_option ({default, Value}, Type) when is_list (Value) ->
    if
        Type =/= varchar,
        Type =/= char,
        Type =/= text ->
            "default " ++ Value;
        true ->
            if
                is_tuple (Type) ->
                    if element (1, Type) =/= varchar ->
                            "default " ++ Value;
                       true ->
                            "default " ++ amnesia:cast_to_sql (Value, Type)
                       end;
                true ->
                    "default " ++ amnesia:cast_to_sql (Value, Type)
            end
    end;
field_option ({default, Value}, Type) ->
    "default " ++ amnesia:cast_to_sql (Value, Type);
field_option (_, _) -> "".


%% ===================================================================
%%  Function: field_type/1
%% ===================================================================
field_type (char) -> "varchar(1)";
field_type (varchar) -> "varchar(255)";
field_type ({varchar, N}) -> lists:flatten (["varchar(",
                                             integer_to_list (N),
                                             ")"]);
field_type (text) -> "text";
field_type (integer) -> "integer";
field_type (int) -> "integer";
field_type (date) -> "date";
field_type (datetime) -> "datetime";
field_type (decimal) -> "decimal";
field_type ({decimal, A, B}) -> lists:flatten (["decimal(",
                                                integer_to_list (A),
                                                ",",
                                                integer_to_list (B),
                                                ")"]);
field_type (boolean) -> "boolean";
field_type (bool) -> "boolean";
field_type (term) -> field_type (varchar);
field_type ({term, N}) -> field_type ({varchar, N});
field_type (_) -> "".


%% ===================================================================
%%  Function: has_join/2
%% ===================================================================
has_join (inner, on) -> ok;
has_join (inner, using) -> ok;
has_join (left, on) -> ok;
has_join (left, using) -> ok;
has_join (right, on) -> ok;
has_join (right, using) -> ok;
has_join (full, _) -> throw ({unsupported_join, full}).


%% ===================================================================
%%  Function: insert/5
%% ===================================================================
insert (Pid, Table, Names, Values, Types) ->
  [_ | FieldList] = lists:flatten (
                     lists:foldl (fun (X, Acc) ->
                                      [Acc, ",", X]
                                  end, "", Names)),
  %% io:format ("~p~n", [FieldList]),
  V = amnesia:cast_to_sql_fields (Values, Types),
  [_ | VV] = lists:flatten (
              lists:foldl (fun (X, Acc) ->
                               [Acc, ",", X]
                           end, "", V)),
  InsertStatement =
    lists:flatten (["insert into ", Table, "(", FieldList, ") values (",
                    VV, ")"]),

  %% io:format ("~p~n", [InsertStatement]),

  case execute (Pid, InsertStatement) of
    {updated, _} ->
      case execute (Pid, "select last_insert_id()") of
        {ok, [ [ID] ]}  ->
          %% io:format ("~p~n", [X]),
          {ok, list_to_integer (ID)};
        {error, _} = Error ->
          Error;
        _ ->
          {error, generic}
      end;
    {error, _} = Error ->
      Error;
    _ ->
      {error, generic}
  end.


%% ===================================================================
%%  Function: update/6
%% ===================================================================
update (Pid, Table, ID, Names, Values, Types) ->
  VV = amnesia:cast_to_sql_fields (Values, Types),
  NameAndValues = lists:zip (Names, VV),
  [_ | FieldList] = lists:flatten (
                     lists:foldl (fun ({N, V}, Acc) ->
                                      [Acc, ",", N , "=", V]
                                  end, "", NameAndValues)),
  UpdateStatement = lists:flatten (["update ", Table,
                                    " set ", FieldList, " where id = ",
                                    integer_to_list (ID)]),

  case execute (Pid, UpdateStatement) of
    {updated, _} ->
      ok;
    {error, _} = Error ->
      Error;
    _ ->
      {error, generic}
  end.


%% ===================================================================
%%  Function: delete/3
%% ===================================================================
delete (Pid, Table, ID) ->
  DeleteStatement = lists:flatten (["delete from ", Table,
                                    " where id = ",
                                    integer_to_list (ID)]),

  case execute (Pid, DeleteStatement) of
    {updated, _} ->
      ok;
    {error, _} = Error ->
      Error;
    _ ->
      {error, generic}
  end.


%% ===================================================================
%%  Function: begin_transaction/1
%% ===================================================================
begin_transaction (Pid) -> execute (Pid, "begin").


%% ===================================================================
%%  Function: commit/1
%% ===================================================================
commit (Pid) -> execute (Pid, "commit").


%% ===================================================================
%%  Function: rollback/1
%% ===================================================================
rollback (Pid) -> execute (Pid, "rollback").


