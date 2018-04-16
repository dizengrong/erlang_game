%%
%% amnesia.erl
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
%% @doc AMNESIA main module.
%%
%% <p>This module provides all AMNESIA functions, allowing insertion,
%% update, deletion and query of DB data.</p>
%%
%%

-module (amnesia).

-export ([open/1,
          open/2,
          close/1,
          start_link/1,
          start_link/2,
          stop/1,
          generate/2,
          add_new/2,
          update/2,
          delete/2,
          fetch/2,
          fetch/3,
          fetch/4,
          load_referenced/2,
          load_referenced/3,
          create_cursor/2,
          delete_cursor/2,
          next/2,
          prev/2,
          nth/3,
          first/2,
          last/2,
          iterate/3,
          index/2,
          sql_quote/1,
          cast_to_sql_fields/2,
          cast_to_sql/2,
          db_tool/2,
          db_tool_a/1]).

%%-export ([make_where_predicate/1]).

-export ([init/1,
          handle_call/3,
          handle_info/2,
          terminate/2]).

-include ("amnesia.hrl").


-define (GC_TIMEOUT, 30000).

%%
%% This record is the status of an amnesia gen_server representing a DB
%%
-record (amnesia_status, { module,
                           dbname,
                           tables,
                           tid,
                           driver,
                           driver_pid,
                           options,
                           cursors,
                           last_cursor_id}).


%%
%% This record represents the structure of a table
%%
-record (table_structure, { name,
                            data,
                            field_names,
                            field_types,
                            field_relations } ).


-record (cursor, { id,
                   dataset,
                   index,
                   last_index,
                   last_usage_time } ).


%%===================================================================
%%  Function: open/1
%%-------------------------------------------------------------------
%% @spec open (Module :: atom()) -> {ok, Pid :: pid()}
%%
%% @doc Opens the connection with the databased specified in the erlang
%% module <pre>Module</pre>. Returns the pid of the server process to be
%% used for interaction with the database.
%%
%%===================================================================
open (Module) ->
  F = driver_info,
  Options = Module:F (),
  gen_server:start_link (?MODULE, {Module, Options}, []).


%% ===================================================================
%%  Function: open/2
%% -------------------------------------------------------------------
%% @spec open (Name, Module :: atom()) -> {ok, Pid :: pid()}
%%   Name = {local, RegisteredName} | {global, RegisteredName}
%%
%% @doc Opens the connection with the databased specified in the erlang
%% module <pre>Module</pre>, starting the server with the given
%% (global or local) name.
%%
%% ===================================================================
open (Name, Module) ->
  F = driver_info,
  Options = Module:F (),
  gen_server:start_link (Name, ?MODULE, {Module, Options}, []).


%% ===================================================================
%%  Function: close/1
%% -------------------------------------------------------------------
%% @spec close (Pid::pid()) -> ok
%%
%% @doc Closes the connection with a database, stopping the server.
%% ===================================================================
close (Pid) ->
  gen_server:call (Pid, {close}).


%% ===================================================================
%%  Function: start_link/1
%% -------------------------------------------------------------------
%% @spec start_link (Module :: atom()) -> {ok, Pid :: pid()}
%%
%% @doc Equivalent to <tt>open/1</tt>.
%%
%% ===================================================================
start_link (Module) ->
  open (Module).


%% ===================================================================
%%  Function: start_link/2
%% -------------------------------------------------------------------
%% @spec start_link (Name, Module :: atom()) -> {ok, Pid :: pid()}
%%
%% @doc Equivalent to <tt>open/2</tt>.
%%
%% ===================================================================
start_link (Name, Module) ->
  open (Name, Module).


%% ===================================================================
%%  Function: stop/1
%% -------------------------------------------------------------------
%% @spec stop (Pid :: pid()) -> ok
%%
%% @doc Stops the server handling a database, also closing the connection
%% with the DBMS
%%
%% ===================================================================
stop (Module) ->
  close (Module).


%% ===================================================================
%%  Function: generate/2
%% -------------------------------------------------------------------
%% generate (Module, Options)
%%   Module :: atom -> the module defining the database
%%   Options :: list_of_options
%%      {driver, DBMSDriver}          -> specifies the driver name
%%      {make_db} || make_db          -> creates the database
%%      {make_header} || make_header ||
%%      {make_headers} || make_headers ||
%%      {make_hdr, TargetDir}         -> creates the .hrl file
%%      {host, Host}
%%      {user, User}
%%      {password, Password}
%%      {dba_user, DBAUser}
%%      {dba_password, DBAPassword}
%% ===================================================================
generate (Module, Opts) ->
    Options = Opts ++ Module:driver_info (),
    Tables = Module:tables (),
    TableDefs =
        lists:map (
          fun (TName) -> TD = Module:table (TName),
                         {TName, TD}
          end,
          Tables),
    Driver = proplists:get_value (driver, Options),
    generate (Module, Driver, Options, TableDefs, Options).


%% ===================================================================
%%  Function: add_new/2
%% ===================================================================
add_new (Pid, Records) when is_list (Records) ->
  recursive_add_record (Pid, Records, []);
add_new (Pid, Record) ->
  gen_server:call (Pid, {add_new, Record}).



recursive_add_record (_, [], TempResults) ->
  {ok, TempResults};
recursive_add_record (Pid, [Record | OtherRecords], TempResults) ->
  [RecordName | RecordData] = tuple_to_list (Record),
  NewRecordData = replace_markers (RecordData, TempResults),
  NewRecord = list_to_tuple ([RecordName | NewRecordData]),
  case add_new (Pid, NewRecord) of
    {ok, Data} ->
      recursive_add_record (Pid, OtherRecords, TempResults ++ [Data]);
    {error, Reason} ->
      {error, Reason, TempResults}
  end.



replace_markers ([], _) -> [];
replace_markers ([H | T], TempResults) when is_atom (H) ->
  Marker = atom_to_list (H),
  case Marker of
    [ $$, AIndex ] when (AIndex >= $1) and (AIndex =< $9) ->
      Index = AIndex - $0,
      L = length (TempResults),
      if
        Index > L ->
          [H | replace_markers (T, TempResults)];
        true ->
          [lists:nth (Index, TempResults) | replace_markers (T, TempResults)]
      end;
    _Other ->
      [H | replace_markers (T, TempResults)]
  end;
replace_markers ([H | T], TempResults) ->
  [H | replace_markers (T, TempResults)].



%% ===================================================================
%%  Function: update/2
%% ===================================================================
update (Pid, Record) ->
  gen_server:call (Pid, {update, Record}).


%% ===================================================================
%%  Function: delete/2
%% ===================================================================
delete (Pid, Record) ->
  gen_server:call (Pid, {delete, Record}).


%% ===================================================================
%%  Function: fetch/2
%% ===================================================================
fetch (Pid, RecordName) ->
  gen_server:call (Pid, {fetch, RecordName, [], []}).


%% ===================================================================
%%  Function: fetch/3
%% ===================================================================
fetch (Pid, RecordName, Predicate) ->
  gen_server:call (Pid, {fetch, RecordName, Predicate, []}).


%% ===================================================================
%%  Function: fetch/4
%% ===================================================================
fetch (Pid, RecordName, Predicate, Options) ->
  gen_server:call (Pid,
                   {fetch, RecordName, Predicate, Options}).


%% ===================================================================
%%  Function: load_referenced/2
%% ===================================================================
load_referenced (Pid, Record) ->
  gen_server:call (Pid, {load_referenced, Record}).


%% ===================================================================
%%  Function: load_referenced/3
%% ===================================================================
load_referenced (Pid, Record, ConnectedRecord) ->
  gen_server:call (Pid,
                   {load_referenced, Record, ConnectedRecord}).



%% ===================================================================
%%
%%          F U N C T I O N S    F O R    C U R S O R S
%%
%% ===================================================================
%% ===================================================================
%%  Function: create_cursor/2
%%   Returns: {ok, CursorID}
%% ===================================================================
create_cursor (_Pid, {error, _} = Error) -> Error;
create_cursor (Pid, FetchResult) when is_list (FetchResult) ->
  create_cursor (Pid, {ok, FetchResult});
create_cursor (Pid, {ok, FetchResult}) ->
  gen_server:call (Pid, {create_cursor, FetchResult}).


%% ===================================================================
%%  Function: delete_cursor/2
%% ===================================================================
delete_cursor (Pid, CursorID) ->
  gen_server:call (Pid, {delete_cursor, CursorID}).


%% ===================================================================
%%  Function: next/2
%% ===================================================================
next (Pid, CursorID) ->
  gen_server:call (Pid, {next_cursor, CursorID}).


%% ===================================================================
%%  Function: prev/2
%% ===================================================================
prev (Pid, CursorID) ->
  gen_server:call (Pid, {prev_cursor, CursorID}).


%% ===================================================================
%%  Function: nth/3
%% ===================================================================
nth (Pid, CursorID, Position) ->
  gen_server:call (Pid, {nth_cursor, CursorID, Position}).


%% ===================================================================
%%  Function: first/2
%% ===================================================================
first (Pid, CursorID) ->
  nth (Pid, CursorID, 1).


%% ===================================================================
%%  Function: last/2
%% ===================================================================
last (Pid, CursorID) ->
  gen_server:call (Pid, {last_cursor, CursorID}).


%% ===================================================================
%%  Function: index/2
%% ===================================================================
index (Pid, CursorID) ->
  gen_server:call (Pid, {index, CursorID}).


%% ===================================================================
%%  Function: iterate/3
%% ===================================================================
iterate (Pid, CursorID, Fun) ->
  iterate (Pid, CursorID, Fun, first (Pid, CursorID)).

iterate (Pid, CursorID, Fun, {ok, Data}) ->
  Fun (Data),
  iterate (Pid, CursorID, Fun, next (Pid, CursorID));
iterate (_, _, _, {end_of_data}) ->
  ok;
iterate (_, _, _, {error, _} = Error) ->
  Error.



%% ===================================================================
%%
%%    FUNCTIONS FOR GENERATING THE SQL-DDL TO CREATE THE TABLES
%%
%% ===================================================================
make_create_statements(Driver, TableDefs) ->
  {ok, Fd} = file:open("create_db.sql", write),
    CreateStatements =
        lists:map (
          fun ({TName, TD}) ->
                  Constr = lists:filter (fun ("") -> false;
                                             (_) -> true
                                         end, generate_constraints (Driver, TD)),
                  FL = [ Driver:sequence_field () | generate_fields (Driver, TD) ] ++ Constr,
                  % FL = generate_fields (Driver, TD) ++ Constr,
                  [_, _ | CommaFL] =
                      lists:foldl (fun (X, Acc) ->
                                           lists:flatten ([Acc, ", ", X])
                                   end, [], FL),
                  S = lists:flatten (["create table ",
                                  atom_to_list (TName),
                                  "(", CommaFL, ")",
                                  Driver:table_create_options ()]),
                  io:format(Fd, "DROP TABLE IF EXISTS ~p;\n", [TName]),
                  io:format(Fd, "~s;\n", [S]),
                  S
          end,
          TableDefs),
    % io:format ("~s~n", [CreateStatements]),
    file:close(Fd),
    CreateStatements.
%% -------------------------------------------------------------------
%%  Function: generate/5
%% -------------------------------------------------------------------
generate (_, _, _, _, [])  -> ok;
generate (Module, Driver, DBMSOptions, TableDefs, [H | T])
  when H == make_sql ->
    make_create_statements(Driver, TableDefs),
    generate (Module, Driver, DBMSOptions, TableDefs, T);
generate (Module, Driver, DBMSOptions, TableDefs, [H | T])
  when H == make_db;
       H == {make_db} ->
    %% Make create table statements
    CreateStatements = make_create_statements(Driver, TableDefs),

    %% Make create index statements
    Indexes =
        lists:map (
          fun ({TName, TD}) ->
                  IndexFields =
                      lists:filter (
                        fun ({_FieldName, _, Options}) when not (is_list(Options)) ->
                                lists:member (index, [Options]);
                            ({_FieldName, _, Options}) when is_list(Options) ->
                                lists:member (index, Options);
                            (_) -> false
                        end,
                        TD),
                  IndexFieldNames =
                      lists:foldl (
                        fun ({Name, _, _}, Acc) ->
                                [Name | Acc]
                        end, [], IndexFields),
                  {TName, IndexFieldNames}
          end, TableDefs),

    CreateIndexStatements =
        lists:map (
          fun ({_TName, []}) -> "";
              ({TName, IndexFieldNames}) ->
                  lists:foldl (
                    fun (X, _Acc) ->
                            lists:flatten (["create index ",
                                                   atom_to_list (X), "_index on ",
                                                   atom_to_list (TName),
                                                   "(", atom_to_list(X), ")"])
                    end, [], IndexFieldNames)
          end,
          Indexes),

    DBName = Module,
    Driver:drop_database (DBName, DBMSOptions),
    Driver:create_database (DBName, DBMSOptions),
    {ok, DriverPid} = Driver:open (DBName, DBMSOptions, use_dba),
    lists:foreach (fun (X) ->
                           {updated, _} = Driver:execute (DriverPid, X)
                   end,
                   CreateStatements),
    lists:foreach (fun ([]) -> ok;
                       (X) ->
                           {updated, _} = Driver:execute (DriverPid, X)
                   end,
                   CreateIndexStatements),

    Driver:drop_user (DBName, DBMSOptions),
    Driver:create_user (DBName, DBMSOptions),
    Driver:close (DriverPid),
    generate (Module, Driver, DBMSOptions, TableDefs, T);
%%
generate (Module, Driver, DBMSOptions, TableDefs, [H | T])
  when element (1, H) == make_header;
       element (1, H) == make_headers;
       element (1, H) == make_hdr
       ->
  {_, Dir} = H,
  RecordDefinitions =
    lists:map (
      fun ({TName, TD}) ->
          RecFields = generate_records (TD),
          [_, _ | CommaFL] =
            lists:foldl (fun (X, Acc) ->
                             lists:flatten ([Acc, ",\n\t", X])
                         end, [], ["id = 0" | RecFields]),
          lists:flatten (["-record (", atom_to_list (TName), ", {\n",
                          CommaFL,
                          "}).\n\n"])
      end,
      TableDefs),
  {ok, F} =
    file:open (lists:flatten ([Dir, "/", atom_to_list (Module), ".hrl"]),
               write),
  io:format (F, "~n~s", [lists:flatten (RecordDefinitions)]),
  file:close (F),
  generate (Module, Driver, DBMSOptions, TableDefs, T);
generate (Module, Driver, DBMSOptions, TableDefs, [_ | T])  ->
  generate (Module, Driver, DBMSOptions, TableDefs, T).



%% -------------------------------------------------------------------
%%  Function: generate_records/1
%% -------------------------------------------------------------------
generate_records ([]) -> [];
generate_records ([H | T]) ->
  [generate_record (H) | generate_records (T)].


%% -------------------------------------------------------------------
%%  Function: generate_record/1
%% -------------------------------------------------------------------
generate_record (R = #'$refers_to' {}) ->
    lists:flatten ([atom_to_list (R#'$refers_to'.to), " = null"]);
generate_record ({FieldName, _, FieldOptions}) ->
    DefaultValue = get_default_value (FieldOptions, null),
    lists:flatten ([atom_to_list (FieldName), " = ",
                    io_lib:format ("~p", [DefaultValue])]).


%% -------------------------------------------------------------------
%%  Function: get_default_value/2
%% -------------------------------------------------------------------
get_default_value ({default, Value}, _) -> Value;
get_default_value ([], Def) -> Def;
get_default_value ([{default, Value} | _], _Def) -> Value;
get_default_value ([ _H | T ], Def) -> get_default_value (T, Def);
get_default_value (X, Def) when not is_list (X) -> Def.


%% -------------------------------------------------------------------
%%  Function: generate_fields/2
%% -------------------------------------------------------------------
generate_fields (_, []) -> [];
generate_fields (Driver, [H | T]) ->
  [generate_field (Driver, H) | generate_fields (Driver, T)].


%% -------------------------------------------------------------------
%%  Function: generate_field/2
%% -------------------------------------------------------------------
generate_field (Driver, R = #'$refers_to' {  }) ->
    lists:flatten ([atom_to_list (R#'$refers_to'.to),
                    "_id ",
                    Driver:field_type (int)]);
generate_field (Driver, {FieldName, FieldType, Options}) ->
    SQLFieldType = Driver: field_type (FieldType),
    SQLOptions =
        lists:foldl (fun (X, Acc) ->
                             lists:flatten ([Acc, X, " "])
                     end, [],
                     field_options (Driver, Options, FieldType)),
    lists:flatten ([atom_to_list (FieldName), " ",
                    SQLFieldType, " ", SQLOptions]).

%% -------------------------------------------------------------------
%%  Function: generate_constraints/2
%% -------------------------------------------------------------------
generate_constraints (_, []) -> [];
generate_constraints (Driver, [H | T]) ->
    [generate_constraint (Driver, H) | generate_constraints (Driver, T)].


%% -------------------------------------------------------------------
%%  Function: generate_constraint/2
%% -------------------------------------------------------------------
generate_constraint (_Driver, R = #'$refers_to' { on_update = OnUpdate, on_delete = OnDelete }) ->
    lists:flatten (["index ",
                    atom_to_list (R#'$refers_to'.to),
                    "_index_id(", atom_to_list (R#'$refers_to'.to), "_id), ",
                    %%
                    "foreign key (", atom_to_list (R#'$refers_to'.to), "_id) references ",
                    atom_to_list (R#'$refers_to'.to),
                    "(id) ",
                    foreign_key_constraint ("on update", OnUpdate),
                    " ",
                    foreign_key_constraint ("on delete", OnDelete)
                   ]);
generate_constraint (_Driver, {FieldName, _FieldType, Options}) ->
    io:format("FieldName:~p, Options:~p\n", [FieldName, Options]),
    case Options == index orelse (is_list(Options) andalso lists:member(index, Options)) of
        true ->
            lists:flatten(["index (", atom_to_list(FieldName), ")"]);
        false ->
            []
    end.


%% -------------------------------------------------------------------
%%  Function: foreign_key_constraint/1
%% -------------------------------------------------------------------
foreign_key_constraint (Type, cascade) ->
    [Type, " cascade"];
foreign_key_constraint (Type, no_action) ->
    [Type, " no action"];
foreign_key_constraint (Type, set_null) ->
    [Type, " set null"];
foreign_key_constraint (_Type, null) ->
    [].

%% -------------------------------------------------------------------
%%  Function: field_options/2
%% -------------------------------------------------------------------
field_options (Driver, T, FieldType) when not is_list (T) ->
    [Driver:field_option (T, FieldType)];
field_options (_, [], _) -> [];
field_options (Driver, [H | T], FieldType) ->
  [Driver:field_option (H, FieldType) | field_options (Driver, T, FieldType)].



%% ===================================================================
%%
%%                        C A L L B A C K S
%%
%% ===================================================================
%% ===================================================================
%%  Function: init/1
%% ===================================================================
init ({Module, Options}) ->
  Tables = Module:tables (),
  TID = create_ets_structure (Module, Tables),
  CursorTID =
    ets:new (
      list_to_atom (
        lists:flatten (["$$edb_cursors_", atom_to_list (Module)])),
      [set, private]),
  Driver = proplists:get_value (driver, Options),
  {ok, DriverPID} = Driver:open (Module, Options),
  {ok, #amnesia_status { module = Module,
                         dbname = Module,
                         tables = Tables,
                         tid = TID,
                         driver = Driver,
                         driver_pid = DriverPID,
                         options = Options,
                         cursors = CursorTID,
                         last_cursor_id = 1}, ?GC_TIMEOUT}.


%% -------------------------------------------------------------------
%%  Function: create_ets_structure/2
%% -------------------------------------------------------------------
create_ets_structure (Module, Tables) ->
  TID = ets:new (
          list_to_atom (
            lists:flatten (["$$edb_", atom_to_list (Module)])),
          [set, private]),
  lists:foreach (
    fun (TName) ->
        TableData = Module:table (TName),
        Names = extract_names (TableData),
        Types = extract_types (TableData),
        Ref = extract_references (TableData),
        ets:insert (TID, {TName, #table_structure {name = TName,
                                                   data = TableData,
                                                   field_names = Names,
                                                   field_types = Types,
                                                   field_relations = Ref}}),
        ok
    end,
    Tables),
  TID.


%% ===================================================================
%%  Function: handle_call/3
%% ===================================================================
handle_call ({add_new, Record}, _From,
             Status = #amnesia_status { dbname = _DBName,
                                        tables = _Tables,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  [TableName | FieldData] = tuple_to_list (Record),
  ReturnValue =
    case ets:lookup (TID, TableName) of
      [] -> {error, {invalid_record, TableName}};
      [{TableName, #table_structure { field_names = Names,
                                      field_types = Types,
                                      field_relations = Ref }}] ->
        [_ | V] = FieldData, % skip "id"
        Values = resolve_relations (V, Ref),
        %% io:format ("~p,~p,~p,~p~n", [TableData, Names, Types, Values]),
        case Driver:insert (DriverPID, atom_to_list (TableName),
                            Names, Values, Types) of
          {error, _} = Error ->
            Error;
          {ok, ID} ->
            NewData = list_to_tuple ([TableName, ID | Values]),
            {ok, NewData}
        end
    end,
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({update, Record}, _From,
             Status = #amnesia_status { dbname = _DBName,
                                        tables = _Tables,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  [TableName | FieldData] = tuple_to_list (Record),
  ReturnValue =
    case ets:lookup (TID, TableName) of
      [] -> {error, {invalid_record, TableName}};
      [{TableName, #table_structure { field_names = Names,
                                      field_types = Types,
                                      field_relations = Ref }}] ->
        [ID | V] = FieldData, % skip "id"
        Values = resolve_relations (V, Ref),
        %% io:format ("~p,~p,~p,~p~n", [TableData, Names, Types, Values]),
        case Driver:update (DriverPID, atom_to_list (TableName),
                            ID, Names, Values, Types) of
          {error, _} = Error ->
            Error;
          ok ->
            {ok, Record}
        end
    end,
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({delete, Record}, _From,
             Status = #amnesia_status { dbname = _DBName,
                                        tables = _Tables,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  [TableName | FieldData] = tuple_to_list (Record),
  ReturnValue =
    case ets:lookup (TID, TableName) of
      [] -> {error, {invalid_record, TableName}};
      [{TableName, #table_structure {  }}] ->
        [ID | _] = FieldData, % get "id"
        case Driver:delete (DriverPID, atom_to_list (TableName), ID) of
          {error, _} = Error ->
            Error;
          ok ->
            ok
        end
    end,
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({fetch, TableName, Predicate, Options}, _From,
             Status = #amnesia_status { dbname = DBName,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  ReturnValue = fetch_all (Driver, DriverPID,
                           TID, DBName, TableName,
                           Predicate, Options),
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({load_referenced, Record}, _From,
             Status = #amnesia_status { dbname = DBName,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  [TableName, ID | FieldData] = tuple_to_list (Record),
  ReturnValue =
    case ets:lookup (TID, TableName) of
      [] -> {error, {invalid_record, TableName}};
      [{TableName, #table_structure { field_relations = Ref }}] ->
        NewFieldData = load_referenced (Driver, DriverPID,
                                        TID, DBName,
                                        TableName,
                                        FieldData, Ref),
        {ok, list_to_tuple ([TableName, ID | NewFieldData])}
    end,
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({load_referenced, Record, ConnectedTable}, _From,
             Status = #amnesia_status { dbname = DBName,
                                        driver = Driver,
                                        driver_pid = DriverPID,
                                        tid = TID }) ->
  [TableName, ID | _] = tuple_to_list (Record),
  ReturnValue =
    case ets:lookup (TID, ConnectedTable) of
      [] -> {error, {invalid_record, ConnectedTable}};
      [{ConnectedTable, #table_structure { field_relations = Ref }}] ->
        case lists:filter (
               fun ({relation, T}) -> T == TableName;
                   (_) -> false
               end, Ref) of
          [_] ->
            Predicate = lists:flatten ([atom_to_list (TableName), "_id = $1"]),
            FetchData = fetch_all (Driver, DriverPID,
                                   TID, DBName, ConnectedTable,
                                   {Predicate, [ID]}, []),
            FetchData;
          _ ->
            {error, no_reference}
        end
    end,
  {reply, ReturnValue, Status, ?GC_TIMEOUT};
%%
handle_call ({create_cursor, FetchResult}, _From,
             Status = #amnesia_status { cursors = CursorTID,
                                        last_cursor_id = CID }) ->
  Cursor = #cursor { id = CID,
                     dataset = FetchResult,
                     index = 0,
                     last_index = length (FetchResult),
                     last_usage_time = erlang:now () },
  ets:insert (CursorTID, {CID, Cursor}),
  cursors_gc (Status),
  {reply, {ok, CID},
   Status#amnesia_status { last_cursor_id = CID + 1},
  ?GC_TIMEOUT};
%%
handle_call ({delete_cursor, CID}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, _}] ->
      ets:delete (CursorTID, CID),
      RetVal = ok;
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({next_cursor, CID}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, #cursor {} = Cursor}] ->
      if
        Cursor#cursor.index >= Cursor#cursor.last_index ->
          Index = Cursor#cursor.last_index + 1,
          RetVal = {end_of_data};
        Cursor#cursor.index < 0 ->
          Index = Cursor#cursor.index,
          RetVal = {end_of_data};
        true ->
          Index = Cursor#cursor.index + 1,
          RetVal = {ok, lists:nth (Index, Cursor#cursor.dataset) }
      end,
      ets:insert (CursorTID,
                  {CID, Cursor#cursor { index = Index,
                                        last_usage_time = erlang:now () }});
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({prev_cursor, CID}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, #cursor {} = Cursor}] ->
      if
        Cursor#cursor.index > Cursor#cursor.last_index ->
          Index = Cursor#cursor.last_index,
          RetVal = {ok, lists:nth (Index, Cursor#cursor.dataset) };
        Cursor#cursor.index =< 1 ->
          Index = 0,
          RetVal = {end_of_data};
        true ->
          Index = Cursor#cursor.index - 1,
          RetVal = {ok, lists:nth (Index, Cursor#cursor.dataset) }
      end,
      ets:insert (CursorTID,
                  {CID, Cursor#cursor { index = Index,
                                        last_usage_time = erlang:now () }});
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({nth_cursor, CID, Index}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, #cursor {} = Cursor}] ->
      if
        (Index > Cursor#cursor.last_index) or
        (Index < 1) ->
          RetVal = {end_of_data};
        true ->
          RetVal = {ok, lists:nth (Index, Cursor#cursor.dataset) }
      end,
      ets:insert (CursorTID,
                  {CID, Cursor#cursor { index = Index,
                                        last_usage_time = erlang:now () }});
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({last_cursor, CID}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, #cursor {} = Cursor}] ->
      if
        Cursor#cursor.last_index == 0 ->
          Index = 1,
          RetVal = {end_of_data};
        true ->
          Index = Cursor#cursor.last_index,
          RetVal = {ok, lists:nth (Index, Cursor#cursor.dataset) }
      end,
      ets:insert (CursorTID,
                  {CID, Cursor#cursor { index = Index,
                                        last_usage_time = erlang:now () }});
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({index, CID}, _From,
             Status = #amnesia_status { cursors = CursorTID }) ->
  case ets:lookup (CursorTID, CID) of
    [{CID, #cursor {} = Cursor}] ->
      Index = Cursor#cursor.index,
      if
        (Index > Cursor#cursor.last_index) or
        (Index < 1) ->
          RetVal = {end_of_data};
        true ->
          RetVal = {ok, Index}
      end;
    _ ->
      RetVal = {error, no_cursor}
  end,
  cursors_gc (Status),
  {reply, RetVal, Status, ?GC_TIMEOUT};
%%
handle_call ({close}, _From, Status) ->
  {stop, normal, ok, Status}.


%% ===================================================================
%%  Function: handle_info/2
%% ===================================================================
handle_info (timeout, Status) ->
  cursors_gc (Status),
  {noreply, Status, ?GC_TIMEOUT}.



%% ===================================================================
%%  Function: terminate/2
%% ===================================================================
terminate (_, _Status = #amnesia_status { module = _Module,
                                          driver = Driver,
                                          driver_pid = DriverPID,
                                          tid = TID }) ->
  Driver:close (DriverPID),
  ets:delete (TID),
  ok.


%% -------------------------------------------------------------------
%%  Function: cursors_gc/1
%% -------------------------------------------------------------------
cursors_gc (#amnesia_status { cursors = CursorTID }) ->
  Cursors = ets:tab2list (CursorTID),
  lists:foreach (
    fun ({CID, #cursor {} = Cursor}) ->
        Last = Cursor#cursor.last_usage_time,
        Diff = timer:now_diff (erlang:now(), Last),
        if
          Diff >= ?GC_TIMEOUT ->
            ets:delete (CursorTID, CID);
          true ->
            nil
        end
    end, Cursors).


%% -------------------------------------------------------------------
%%  Function: extract_types/1
%% -------------------------------------------------------------------
extract_types (TableData) ->
  lists:map (fun (#'$refers_to' {}) -> integer;
                 ({_,X,_}) -> X
             end, TableData).


%% -------------------------------------------------------------------
%%  Function: extract_names/1
%% -------------------------------------------------------------------
extract_names (TableData) ->
  lists:map (fun
               (#'$refers_to' { to = To}) ->
                 lists:flatten ([atom_to_list (To), "_id"]);
               ({X, _ ,_}) ->
                 atom_to_list (X)
             end, TableData).


%% -------------------------------------------------------------------
%%  Function: extract_references/1
%% -------------------------------------------------------------------
extract_references (TableData) ->
  lists:map (fun
               (#'$refers_to' { to = To }) -> {relation, To};
               (_) -> nil
             end, TableData).


%% -------------------------------------------------------------------
%%  Function: map_to_record/2
%% -------------------------------------------------------------------
% map_to_record (RecordName, RetrievedData) ->
%   lists:map (fun (X) ->
%                  list_to_tuple ([RecordName | X])
%              end, RetrievedData).


%% -------------------------------------------------------------------
%%  Function: resolve_relations/2
%% -------------------------------------------------------------------
resolve_relations ([], []) -> [];
resolve_relations ([Value | T1], [{relation, To} | T2])
  when is_tuple (Value) ->
  [To, Id | _] = tuple_to_list (Value),
  %% the tuple should be a record of the same type of table "To"
  %% then get the record id
  [Id | resolve_relations (T1, T2)];
resolve_relations ([Value | T1], [_ | T2]) ->
  [Value | resolve_relations (T1, T2)].



%% -------------------------------------------------------------------
%%  Function: load_referenced/7
%% -------------------------------------------------------------------
load_referenced (_Driver, _DriverPID, _TID, _DBName, _TableName, [], []) ->
  [];
%%
load_referenced (Driver, DriverPID, TID, DBName, TableName,
                 [Value | T1], [{relation, To} | T2]) ->
    ConnectedRecord =
        case fetch_all (Driver, DriverPID, TID, DBName,
                        To, {"id = $1", [Value]}, []) of
            {ok, [X]} -> X;
            _ -> Value
        end,
  [ ConnectedRecord | load_referenced (Driver, DriverPID, TID, DBName,
                                       TableName, T1, T2) ];
%%
load_referenced (Driver, DriverPID, TID, DBName, TableName,
                 [Value | T1], [_ | T2]) ->
  [ Value | load_referenced (Driver, DriverPID,
                             TID, DBName, TableName, T1, T2) ].
%%



%% -------------------------------------------------------------------
%%  Function: check_table_reference/3
%% -------------------------------------------------------------------
check_table_reference (TID, T1, T2) ->
  RetVal = check_table_reference_3 (TID, T1, T2),
  case RetVal of
    {error, {no_reference, _}} ->
      RV2 = check_table_reference_3 (TID, T2, T1),
      RV2;
    _ ->
      RetVal
  end.

check_table_reference_3 (TID, TableName, ConnectedTable) ->
  case ets:lookup (TID, ConnectedTable) of
    [] -> {error, {invalid_record, ConnectedTable}};
    [{ConnectedTable, #table_structure { field_relations = Ref }}] ->
      case lists:filter (
             fun ({relation, T}) -> T == TableName;
                 (_) -> false
             end, Ref) of
        [_] ->
          {ok, {lists:flatten ([atom_to_list (TableName), ".id"]),
                lists:flatten ([atom_to_list (ConnectedTable), ".",
                                atom_to_list (TableName), "_id"])}};
        _ ->
          {error, {no_reference, {TableName, ConnectedTable}}}
      end
  end.


%% -------------------------------------------------------------------
%%  Function: fetch_all/8
%% -------------------------------------------------------------------
fetch_all (Driver, DriverPID, TID, _DBName, TableName, Predicate, Options) ->
    TN =
        if
            not is_list (TableName) ->
                Join = false,
                [ TableName ];
            true ->
                Join = true,
                TableName
        end,

    try
        [_ | FieldList] = lists:flatten (build_field_part (TID, TN)),
        From = lists:flatten (build_from_part (Driver, TID, TN)),
        FieldTypes = build_field_types (TID, TN),
        RecordTypes = build_record_types (TN),

        Where = case make_where_predicate (Predicate) of
                    [] -> [];
                    P -> [" where ", P, " "]
                end,

        _Aggregate =
        {AggregationMode, AggregationType, AggregationField,
         AggregationFunction, HavingClause} = get_aggregate_function (Options),

%%         io:format ("Field and record types: ~p,~p~n",
%%                    [FieldTypes, RecordTypes]),
%%         io:format ("Aggregated function ~p~n~n", [Aggregate]),

        case AggregationMode of
            no_aggregation ->
                FTypes = FieldTypes,
                RTypes = RecordTypes,
                SelectStatement =
                    lists:flatten (["select ",
                                    FieldList,
                                    " from ",
                                    From,
                                    " ",
                                    Where,
                                    get_option (order_by, Options),
                                    get_option (limit, Options)]);
            simple -> % something like "select count(*) from ...."
                FTypes = [[AggregationType]],
                RTypes = ['$$norecord$$'],
                SelectStatement =
                    lists:flatten (["select ",
                                    AggregationFunction,
                                    " from ",
                                    From,
                                    " ",
                                    Where]);
            group_by ->
                %% something like "select count(*), .. from .... group by
                %%                 ... having ..."
                FTypes = [[AggregationType] | FieldTypes],
                RTypes = ['$$norecord$$' | RecordTypes],
                SelectStatement =
                    lists:flatten (["select ",
                                    AggregationFunction,
                                    ",",
                                    FieldList,
                                    " from ",
                                    From,
                                    " ",
                                    Where,
                                    " group by ",
                                    AggregationField,
                                    " ",
                                    HavingClause,
                                    " ",
                                    get_option (order_by, Options),
                                    get_option (limit, Options)])
        end,

        %%io:format ("~p~n", [SelectStatement]),

        case Driver:execute (DriverPID, SelectStatement) of
            {ok, Rows} ->
                %% io:format ("Data: ~p~n", [Rows]),
                Data = lists:map (
                         fun (R) ->
                                 %% io:format ("Data: ~p~n", [R]),
                                 X = translate_sql_record (R, FTypes,
                                                           RTypes),
                                 %% io:format ("Translated Data: ~p~n", [X]),
                                 if
                                     Join -> X;
                                     AggregationMode == group_by -> X;
                                     true -> [Rec] = X, Rec
                                 end
                         end, Rows),
                {ok, Data};
            Other ->
                Other
        end
    catch
        throw:{invalid_record, RecordName} ->
            {error, {invalid_record, RecordName}};
          throw:{unsupported_join, JoinType} ->
            {error, {unsupported_join, JoinType}};
          throw:{no_reference, NoRef} ->
            {error, {no_reference, NoRef}}
    end.


%% fetch_all (Driver, TID, DBName, TableName, Predicate, Options) ->
%%   case ets:lookup (TID, TableName) of
%%     [] -> {error, invalid_record};
%%     [{TableName, #table_structure { field_names = Names,
%%                                     field_types = Types }}] ->
%%       %% io:format ("~p,~p~n", [TableData, FieldNames]),

%%       [_ | FieldList] = lists:flatten (
%%                           lists:foldl (fun (X, Acc) ->
%%                                            [Acc, ",", X]
%%                                        end, "", ["id" | Names])),

%%       Where = case make_where_predicate (Predicate) of
%%                 [] -> [];
%%                 P -> [" where ", P, " "]
%%               end,

%%       SelectStatement =
%%         lists:flatten (["select ",
%%                         FieldList,
%%                         " from ",
%%                         if is_atom (TableName) -> atom_to_list (TableName);
%%                            true -> TableName
%%                         end,
%%                         Where,
%%                         get_option (order_by, Options),
%%                         get_option (limit, Options)]),

%%       %% io:format ("~p~n", [SelectStatement]),

%%       T = [integer | Types],
%%       case Driver:execute (DBName, SelectStatement) of
%%         {ok, Rows} ->
%%           CastedRows = lists:map (fun (R) ->
%%                                       cast_record_from_list (R, T)
%%                                   end, Rows),
%%           {ok, map_to_record (TableName, CastedRows)};
%%         Other ->
%%           Other
%%       end
%%   end.


%% -------------------------------------------------------------------
%%  Function: translate_sql_record/3
%% -------------------------------------------------------------------
translate_sql_record (_, [], []) -> [];
translate_sql_record (Row, [ Types ], [ RecordName ]) ->
    CastedRows = cast_record_from_list (Row, Types),
    %% io:format ("translate_sql_record: ~p, ~p~n", [RecordName, CastedRows]),
    if
        RecordName == '$$norecord$$' ->
            [Data] = CastedRows;
            %% Data = list_to_tuple (CastedRows);
        true ->
            Data = list_to_tuple ([RecordName | CastedRows])
    end,
    [Data];
translate_sql_record (Row, [ Types | TypesRest], [RecordName | RecordRest]) ->
    {First, Last} = lists:split (length (Types), Row),
    CastedRows = cast_record_from_list (First, Types),
    %% io:format ("translate_sql_record: ~p, ~p~n", [RecordName, CastedRows]),
    if
        RecordName == '$$norecord$$' ->
            [Data] = CastedRows;
            %% Data = list_to_tuple (CastedRows);
        true ->
            Data = list_to_tuple ([RecordName | CastedRows])
    end,
    [ Data | translate_sql_record (Last, TypesRest, RecordRest) ].



%% -------------------------------------------------------------------
%%  Function: build_from_part/2, 3
%% -------------------------------------------------------------------
build_from_part (Driver, TID, Op) ->
  build_from_part (Driver, TID, Op, first).
%%

%%
build_from_part (_Driver, _TID, [], _) -> [];
%%
build_from_part (Driver, TID,
                 [T1, #join{ using = undefined, on = undefined }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (inner, on),
  case check_table_reference (TID, T1, T2) of
    {ok, {Left, Right}} ->
      [inner_join (T1, T2,
                   {on, lists:flatten ([Left, " = ", Right])},
                   Modifier) |
       build_from_part (Driver, TID, [T2 | Rest], continue)];
    {error, Throw} ->
      throw (Throw)
  end;
%%
%%
build_from_part (Driver, TID,
                 [T1, #join{ using = U, on = undefined }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (inner, using),
  [inner_join (T1, T2, {using, U}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
build_from_part (Driver, TID,
                 [T1, #join{ using = undefined, on = On }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (inner, on),
  [inner_join (T1, T2, {on, On}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
%%
build_from_part (Driver, TID,
                 [T1, #left_join{ using = undefined, on = undefined }, T2
                  | Rest],
                 Modifier) ->
  Driver:has_join (left, on),
  case check_table_reference (TID, T1, T2) of
    {ok, {Left, Right}} ->
      [left_outer_join (T1, T2,
                        {on, lists:flatten ([Left, " = ", Right])},
                        Modifier) |
       build_from_part (Driver, TID, [T2 | Rest], continue)];
    {error, Throw} ->
      throw (Throw)
  end;
%%
build_from_part (Driver, TID,
                 [T1, #left_join{ using = U, on = undefined }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (left, using),
  [left_outer_join (T1, T2, {using, U}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
build_from_part (Driver, TID,
                 [T1, #left_join{ using = undefined, on = On }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (left, on),
  [left_outer_join (T1, T2, {on, On}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
%%
build_from_part (Driver, TID,
                 [T1, #right_join{ using = undefined, on = undefined }, T2
                  | Rest],
                 Modifier) ->
  Driver:has_join (right, on),
  case check_table_reference (TID, T1, T2) of
    {ok, {Left, Right}} ->
      [right_outer_join (T1, T2,
                         {on, lists:flatten ([Left, " = ", Right])},
                         Modifier) |
       build_from_part (Driver, TID, [T2 | Rest], continue)];
    {error, Throw} ->
      throw (Throw)
  end;
%%
build_from_part (Driver, TID,
                 [T1, #right_join{ using = U, on = undefined }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (right, using),
  [right_outer_join (T1, T2, {using, U}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
build_from_part (Driver, TID,
                 [T1, #right_join{ using = undefined, on = On }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (right, on),
  [right_outer_join (T1, T2, {on, On}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
%%
build_from_part (Driver, TID,
                 [T1, #full_join{ using = undefined, on = undefined }, T2
                  | Rest],
                 Modifier) ->
  Driver:has_join (full, on),
  case check_table_reference (TID, T1, T2) of
    {ok, {Left, Right}} ->
      [full_outer_join (T1, T2,
                        {on, lists:flatten ([Left, " = ", Right])},
                        Modifier) |
       build_from_part (Driver, TID, [T2 | Rest], continue)];
    {error, Throw} ->
      throw (Throw)
  end;
%%
build_from_part (Driver, TID,
                 [T1, #full_join{ using = U, on = undefined }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (full, using),
  [full_outer_join (T1, T2, {using, U}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
build_from_part (Driver, TID,
                 [T1, #full_join{ using = undefined, on = On }, T2 | Rest],
                 Modifier) ->
  Driver:has_join (full, on),
  [full_outer_join (T1, T2, {on, On}, Modifier) |
   build_from_part (Driver, TID, [T2 | Rest], continue)];
%%
%%
build_from_part (Driver, TID, [T | Rest], first) ->
  [ atom_to_list (T) | build_from_part (Driver, TID, Rest, continue)];
%%
build_from_part (Driver, TID, [_T | Rest], continue) ->
  build_from_part (Driver, TID, Rest, continue);
%%
build_from_part (_Driver, _TID, T, _) when is_atom (T)->
  [ atom_to_list (T) ].


%% -------------------------------------------------------------------
%%  Function: inner_join/4
%% -------------------------------------------------------------------
inner_join (T1, T2, {using, U}, first) ->
  [ " ", atom_to_list (T1) , " INNER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
inner_join (_T1, T2, {using, U}, _) ->
  [ " INNER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
inner_join (T1, T2, {on, On}, first) ->
  [ atom_to_list (T1) , " INNER JOIN ", atom_to_list (T2), " ON ", On, " "];
inner_join (_T1, T2, {on, On}, _) ->
  [ " INNER JOIN ", atom_to_list (T2), " ON ", On, " "].


%% -------------------------------------------------------------------
%%  Function: left_outer_join/4
%% -------------------------------------------------------------------
left_outer_join (T1, T2, {using, U}, first) ->
  [ " ", atom_to_list (T1) , " LEFT OUTER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
left_outer_join (_T1, T2, {using, U}, _) ->
  [ " LEFT OUTER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
left_outer_join (T1, T2, {on, On}, first) ->
  [ atom_to_list (T1) , " LEFT OUTER JOIN ", atom_to_list (T2),
    " ON ", On, " "];
left_outer_join (_T1, T2, {on, On}, _) ->
  [ " LEFT OUTER JOIN ", atom_to_list (T2),
    " ON ", On, " "].


%% -------------------------------------------------------------------
%%  Function: right_outer_join/4
%% -------------------------------------------------------------------
right_outer_join (T1, T2, {using, U}, first) ->
  [ " ", atom_to_list (T1) , " RIGHT OUTER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
right_outer_join (_T1, T2, {using, U}, _) ->
  [ " RIGHT OUTER JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
right_outer_join (T1, T2, {on, On}, first) ->
  [ atom_to_list (T1) , " RIGHT OUTER JOIN ", atom_to_list (T2),
    " ON ", On, " "];
right_outer_join (_T1, T2, {on, On}, _) ->
  [ " RIGHT OUTER JOIN ", atom_to_list (T2), " ON ", On, " "].


%% -------------------------------------------------------------------
%%  Function: full_outer_join/4
%% -------------------------------------------------------------------
full_outer_join (T1, T2, {using, U}, first) ->
  [ " ", atom_to_list (T1) , " FULL JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
full_outer_join (_T1, T2, {using, U}, _) ->
  [ " FULL JOIN ", atom_to_list (T2),
    " USING(", if is_atom (U) -> atom_to_list (U);
                 true -> U
              end, ") "];
full_outer_join (T1, T2, {on, On}, first) ->
  [ atom_to_list (T1) , " FULL JOIN ", atom_to_list (T2),
    " ON ", On, " "];
full_outer_join (_T1, T2, {on, On}, _) ->
  [ " FULL JOIN ", atom_to_list (T2),
    " ON ", On, " "].


%% -------------------------------------------------------------------
%%  Function: build_field_part/2
%% -------------------------------------------------------------------
build_field_part (_TID, []) -> [];
%%
build_field_part (TID, [X, T2 | Rest]) when is_record (X, join);
                                            is_record (X, left_join);
                                            is_record (X, right_join);
                                            is_record (X, full_join)
                                            ->
  [ build_field_part (TID, T2) | build_field_part (TID, Rest) ];
%%
build_field_part (TID, [T1, X, T2 | Rest]) when is_record (X, join);
                                                is_record (X, left_join);
                                                is_record (X, right_join);
                                                is_record (X, full_join)
                                                ->
  [ build_field_part (TID, T1), build_field_part (TID, T2)
    | build_field_part (TID, Rest) ];
%%
build_field_part (TID, [TableName | Rest]) ->
  %%io:format ("Table name ~p~n", [TableName]),
  case ets:lookup (TID, TableName) of
    [] -> throw ({invalid_record, TableName});
    [{TableName, #table_structure { field_names = Names }}] ->
      %% io:format ("~p,~p~n", [TableData, FieldNames]),

      FieldList = lists:flatten (
                    lists:foldl (fun (X, Acc) ->
                                     [Acc, ",",
                                      atom_to_list (TableName), ".", X]
                                 end, "", ["id" | Names])),

      [FieldList | build_field_part (TID, Rest)]
  end;
%%
build_field_part (TID, TableName) when is_atom (TableName) ->
  build_field_part (TID, [ TableName ]).



%% -------------------------------------------------------------------
%%  Function: build_field_types/2
%% -------------------------------------------------------------------
build_field_types (_TID, []) -> [];
%%
build_field_types (TID, [X, T2 | Rest])
  when is_record (X, join);
       is_record (X, left_join);
       is_record (X, right_join);
       is_record (X, full_join)
       ->
  [ build_field_types (TID, T2) | build_field_types (TID, Rest) ];
%%
build_field_types (TID, [T1, X, T2 | Rest])
  when is_record (X, join);
       is_record (X, left_join);
       is_record (X, right_join);
       is_record (X, full_join)
       ->
  [ build_field_types (TID, T1), build_field_types (TID, T2)
    | build_field_types (TID, Rest) ];
%%
build_field_types (TID, [TableName | Rest]) ->
  case ets:lookup (TID, TableName) of
    [] -> throw ({invalid_record, TableName});
    [{TableName, #table_structure { field_types = Types }}] ->
      [ [integer | Types] | build_field_types (TID, Rest)]
  end;
%%
build_field_types (TID, TableName) when is_atom (TableName) ->
  [ T ] = build_field_types (TID, [ TableName ]),
  T.


%% -------------------------------------------------------------------
%%  Function: build_record_types/1
%%  Build the type of the records resulting on the basis of the
%%  query passed to the "fetch" statement
%% -------------------------------------------------------------------
build_record_types ([]) -> [];
%%
build_record_types ([X, T2 | Rest])
  when is_record (X, join);
       is_record (X, left_join);
       is_record (X, right_join);
       is_record (X, full_join)
       ->
  [ build_record_types (T2) | build_record_types (Rest) ];
%%
build_record_types ([T1, X, T2 | Rest])
  when is_record (X, join);
       is_record (X, left_join);
       is_record (X, right_join);
       is_record (X, full_join)
       ->
  [ build_record_types (T1), build_record_types (T2)
    | build_record_types (Rest) ];
%%
build_record_types ([TableName | Rest]) ->
  [ TableName | build_record_types (Rest)];
%%
build_record_types (TableName) when is_atom (TableName) ->
  [ T ] = build_record_types ([ TableName ]),
  T.



%% -------------------------------------------------------------------
%%  Function: cast_record_from_list/6 The sql data list to erlang record
%% -------------------------------------------------------------------
cast_record_from_list (Record, Types) ->
  lists:map (fun ({V, T}) ->
                 cast_from_list (V, T)
             end, lists:zip (Record, Types)).

cast_from_list (null, _) -> null;
cast_from_list (V, integer) -> list_to_integer (V);
cast_from_list (V, int) -> list_to_integer (V);
cast_from_list (V, decimal) -> list_to_float (V);
cast_from_list (V, {decimal, _, _}) -> list_to_float (V);
cast_from_list (V, date) -> sql_date_to_erlang_date (V);
cast_from_list (V, datetime) -> [D, T] = string:tokens (V, " "),
                                sql_date_time_to_erlang_date_time (D, T);
cast_from_list (V, term) -> 
  string_to_term(V);
cast_from_list (V, {term, _}) -> 
  string_to_term(V);
cast_from_list (V, T) when T == boolean; T == bool ->
    case lists:last(V) of
        1 -> true;
        _ -> false
    end;
cast_from_list (V, _) -> V.

string_to_term(Str) ->
  {ok, Tokens, _} = erl_scan:string(Str ++ "."),
  {ok, Term} = erl_parse:parse_term(Tokens),
  Term.


%% ===================================================================
%%  Function: sql_quote/1
%%  Quoting "'" to "\'"
%% ===================================================================
sql_quote (X) -> sql_quote (X, []).

sql_quote ([], Acc) -> lists:reverse (Acc);
sql_quote ([39 | T], Acc) -> sql_quote (T, [39, $\\ | Acc]); % quote '
sql_quote ([H | T], Acc) -> sql_quote (T, [H | Acc]).



%% ===================================================================
%%  Function: cast_to_sql_fields/2
%% ===================================================================
cast_to_sql_fields ([], []) -> [];
cast_to_sql_fields ([Value | T1], [Type | T2]) ->
  NewValue = cast_to_sql (Value, Type),
  [NewValue | cast_to_sql_fields (T1, T2)].



%% ===================================================================
%%  Function: cast_to_sql/2
%% ===================================================================
cast_to_sql (null, _) -> "null";
cast_to_sql (Value, Type) when Type == varchar;
                               Type == char;
                               Type == text ->
  lists:flatten (["'", sql_quote (Value), "'"]);
% Added - Bruce Kissinger - 2012
cast_to_sql (Value, {varchar, _}) ->
  lists:flatten (["'", sql_quote (Value), "'"]);
cast_to_sql (Value, {char, _}) ->
  lists:flatten (["'", sql_quote (Value), "'"]);
cast_to_sql (Value, {text, _}) ->
  lists:flatten (["'", sql_quote (Value), "'"]);
% end of Addition
cast_to_sql (Value, term) -> 
  lists:flatten (["'", lists:flatten(io_lib:format("~w", [Value])), "'"]);
cast_to_sql (Value, {term, _}) -> 
  lists:flatten (["'", lists:flatten(io_lib:format("~w", [Value])), "'"]);
cast_to_sql (Value, int) -> integer_to_list (Value);
cast_to_sql (Value, integer) -> integer_to_list (Value);
cast_to_sql (Value, decimal) when is_integer(Value) -> cast_to_sql (Value * 1.0, decimal);
cast_to_sql (Value, decimal) -> float_to_list (Value);
cast_to_sql (Value, {decimal, A, B}) when is_integer (Value) -> cast_to_sql (Value * 1.0, {decimal, A, B});
cast_to_sql (Value, {decimal, _, _}) -> float_to_list (Value);
cast_to_sql ({_Y, _M, _D} = Value, date) ->
  lists:flatten (["'", erlang_to_sql_date (Value), "'"]);
cast_to_sql ({{_Y, _M, _D} = Value, {_HH, _MM, _SS}}, date) ->
  lists:flatten (["'", erlang_to_sql_date (Value), "'"]);
cast_to_sql ({{_Y, _M, _D} , {_HH, _MM, _SS}} = Value, datetime) ->
  lists:flatten (["'", erlang_to_sql_date_time (Value), "'"]);
cast_to_sql (true, T) when T == boolean; T == bool -> "true";
cast_to_sql (false, T) when T == boolean; T == bool -> "false";
cast_to_sql (Value, _) -> Value.
%%



%% ===================================================================
%%  Function: type_check/2
%% ===================================================================
% type_check (null, _) -> true;
% type_check (Value, Type) when ((Type == varchar) or
%                                (Type == char) or
%                                (Type == text)) and
%                               (is_list (Value)) -> true;
% type_check (Value, int) when is_integer (Value) -> true;
% type_check (Value, integer) when is_integer (Value) -> true;
% type_check (Value, decimal) when is_float (Value) -> true;
% type_check (Value, {decimal, _, _}) when is_float (Value) -> true;
% type_check ({Y, M, D}, date) when is_integer (Y),
%                                   is_integer (M),
%                                   is_integer (D) -> true;
% type_check ({{Y, M, D}, {_HH, _MM, _SS}}, date) when is_integer (Y),
%                                                      is_integer (M),
%                                                      is_integer (D) -> true;
% type_check ({{Y, M, D} , {HH, MM, SS}}, datetime) when is_integer (Y),
%                                                        is_integer (M),
%                                                        is_integer (D),
%                                                        is_integer (HH),
%                                                        is_integer (MM),
%                                                        is_integer (SS) -> true;
% type_check (_, _) -> false.
%%



%% ===================================================================
%%
%%                  DATE-TIME CONVERSION FUNCTIONS
%%
%% ===================================================================
%%  Function: erlang_to_sql_date/1
%% ===================================================================
erlang_to_sql_date ({Year, Month, Day}) ->
  lists:flatten ([ padding (integer_to_list (Year), 4, $0),
                   "-",
                   padding (integer_to_list (Month), 2, $0),
                   "-",
                   padding (integer_to_list (Day), 2, $0)]).


%% ===================================================================
%%  Function: erlang_to_sql_date_time/1
%% ===================================================================
erlang_to_sql_date_time ({{Year, Month, Day}, {HH, MM, SS}}) ->
  lists:flatten ([ padding (integer_to_list (Year), 4, $0),
                   "-",
                   padding (integer_to_list (Month), 2, $0),
                   "-",
                   padding (integer_to_list (Day), 2, $0),
                   " ",
                   padding (integer_to_list (HH), 2, $0),
                   ":",
                   padding (integer_to_list (MM), 2, $0),
                   ":",
                   padding (integer_to_list (SS), 2, $0)]).


%% ===================================================================
%%  Function: sql_date_to_erlang_date/1
%% ===================================================================
sql_date_to_erlang_date ([Y1, Y2, Y3, Y4, _, M1, M2, _, D1, D2]) ->
  {list_to_integer ([Y1, Y2, Y3, Y4]),
   list_to_integer ([M1, M2]),
   list_to_integer ([D1, D2])}.


%% ===================================================================
%%  Function: sql_date_to_erlang_date_time/1
%% ===================================================================
% sql_date_to_erlang_date_time ([Y1, Y2, Y3, Y4, _, M1, M2, _, D1, D2]) ->
%   { {list_to_integer ([Y1, Y2, Y3, Y4]),
%      list_to_integer ([M1, M2]),
%      list_to_integer ([D1, D2])},
%     {0,0,0} }.


%% ===================================================================
%%  Function: sql_date_time_to_erlang_date_time/1
%% ===================================================================
sql_date_time_to_erlang_date_time ([Y1, Y2, Y3, Y4, _, M1, M2, _, D1, D2],
                                   [H1, H2, _, MM1, MM2, _, S1, S2]) ->
  { {list_to_integer ([Y1, Y2, Y3, Y4]),
     list_to_integer ([M1, M2]),
     list_to_integer ([D1, D2])},
    {list_to_integer ([H1, H2]),
     list_to_integer ([MM1, MM2]),
     list_to_integer ([S1, S2])} }.


%% ===================================================================
%%  Function: padding/3
%% ===================================================================
padding (String, Size, Char) ->
  padding (String, length (String), Size, Char).
%%
padding (String, Len, Size, Char) when Len < Size ->
  [ Char | padding (String, Len + 1, Size, Char) ];
padding (String, _Len, _Size, _Char)  ->
  String.
%%



%% ===================================================================
%%  Function: make_where_predicate/1
%% ===================================================================
make_where_predicate ([]) -> [];
make_where_predicate ({}) -> [];
make_where_predicate ({Format, Data}) ->
  Params = ["$1", "$2", "$3", "$4", "$5", "$6", "$7", "$8", "$9" ],
  Len = length (Data),
  %% io:format ("~p, ~p~n", [Data, Len]),
  Predicate =
    lists:foldl (
      fun (P, Acc) ->
          [_ | ParamIndex] = P,
          %% get parameter index
          Index = list_to_integer (ParamIndex),
          FormattedData =
            if
              Index > Len -> "";
              true -> dump (lists:nth (Index, Data))
                      %% transform parameter into a string
            end,
          NewAcc = string_replace (Acc,
                                   P,
                                   FormattedData),
          NewAcc
      end, Format, Params),
  lists:flatten (Predicate).



%% ===================================================================
%%  Function: string_replace/3
%% ===================================================================
string_replace (String, ToRepl, Repl) ->
  ReplLen = string:len(ToRepl),
  case string:str(String, ToRepl) of
    0 ->
      String;
    Index ->
      Tail = string:substr(String , Index + ReplLen),
      Start = string:concat (string:substr(String, 1, Index-1), Repl),
      Res = string:concat (Start, Tail),
      string_replace (Res, ToRepl, Repl)
  end.


dump (X) when is_atom (X) -> atom_to_list (X);
dump (X) when is_integer (X) -> integer_to_list (X);
dump (X) when is_float (X) -> float_to_list (X);
dump (X) when is_list (X) ->
    case io_lib:printable_list (X) of
        true -> case X of
                    [$' | _] -> X;
                    [$" | _] -> X;
                    _ ->  "'" ++ X ++ "'"
                end;

        _ -> X
    end.



%% ===================================================================
%%  Function: get_option/2
%% ===================================================================
get_option (_, []) -> "";
%%
get_option (order_by, [{order_by, FieldList} | _]) when is_list (FieldList) ->
  [_ | CommaFL] =
    lists:foldl (fun (X, Acc) ->
                     lists:flatten ([Acc, ", ", get_sort_option (X)])
                 end, [], FieldList),
  [" order by ", CommaFL];
%%
get_option (order_by, [{order_by, Field} | _]) when is_atom (Field) ->
  [" order by ", get_sort_option (Field)];
%%
get_option (order_by, [{order_by, Field, asc} | _]) when is_atom (Field) ->
  [" order by ", get_sort_option ({Field, asc})];
%%
get_option (order_by, [{order_by, Field, desc} | _]) ->
  [" order by ", get_sort_option ({Field, desc})];
%%
get_option (limit, [{limit, N} | _]) ->
  [" limit ", integer_to_list (N)];
%%
get_option (limit, [{limit, N, M} | _]) ->
  [" limit ", integer_to_list (N), ",", integer_to_list (M)];
%%
get_option (Optname, [_ | T]) ->
  get_option (Optname, T).


%% ===================================================================
%%  Function: get_sort_option/1
%% ===================================================================
get_sort_option ({Field, asc}) -> [atom_to_list (Field), " asc"];
get_sort_option ({Field, desc}) -> [atom_to_list (Field), " desc"];
get_sort_option (Field) -> atom_to_list (Field).


%% ===================================================================
%%  Function: get_aggregate_function/1
%% ===================================================================
get_aggregate_function ([]) -> {no_aggregation, nil, "", "", ""};
get_aggregate_function ([{aggregate, Function, Type} | _]) ->
    {simple, Type, nil,
     lists:flatten ([Function, " as __aggregated_data__ "]),
     ""};
%%
get_aggregate_function ([{aggregate, Function, Type,
                          {_HavingClause, _HavingData} = H} | _]) ->
    {simple, Type, nil,
     lists:flatten ([Function, " as __aggregated_data__ "]),
     lists:flatten ([" having ", make_where_predicate (H)])};
%%
get_aggregate_function ([{aggregate, Function, Type, Name} | _])
  when is_atom (Name) ->
    {group_by, Type, atom_to_list (Name),
     lists:flatten ([Function, " as __aggregated_data__ "]),
     ""};
%%
get_aggregate_function ([{aggregate, Function, Type,
                          Name, {_HavingClause, _HavingData} = H} | _])
    when is_atom (Name) ->
    {group_by, Type, atom_to_list (Name),
     lists:flatten ([Function, " as __aggregated_data__ "]),
     lists:flatten ([" having ", make_where_predicate (H)])};
%%
get_aggregate_function ([_ | T]) -> get_aggregate_function (T).


%% ===================================================================
%%  Function: db_tool/2
%% ===================================================================
db_tool (DBName, Options) ->
  F = driver_info,
  DriverInfo = DBName:F (),
  amnesia:generate (DBName, DriverInfo ++ Options).

%% ===================================================================
%%  Function: db_tool_a/1
%% ===================================================================
db_tool_a (String) ->
  [DBName | Options] = lists:reverse (string:tokens (String, " ")),
  DBOpts = make_db_opts (lists:reverse (Options), []),
  db_tool (list_to_atom (DBName), DBOpts).


%% ===================================================================
%%  Function: make_db_opts/2
%% ===================================================================
make_db_opts ([], DBOpts) ->
  DBOpts;
make_db_opts (["--make_header", Dir | T], DBOpts) ->
  make_db_opts (T, [ {make_hdr, Dir} | DBOpts]);
make_db_opts (["--make_headers", Dir | T], DBOpts) ->
  make_db_opts (T, [ {make_hdr, Dir} | DBOpts]);
make_db_opts (["--make_hdr", Dir | T], DBOpts) ->
  make_db_opts (T, [ {make_hdr, Dir} | DBOpts]);
make_db_opts (["--make_db" | T], DBOpts) ->
  make_db_opts (T, [ make_db | DBOpts]).

