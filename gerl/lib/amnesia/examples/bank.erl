%%
%% bank.erl
%%
-module (bank).

-include_lib ("amnesia/include/amnesia_db_def.hrl").

%%

driver_info () ->
    [{driver, mysql_drv},
     {host, "localhost"},
     {user, "bank_admin"},
     {password, "banking"},
     {logging, false}].

%%

tables () ->
    [customer,
     account,
     owner,
     operation_type,
     operation].

%%
table (customer) ->
    [{customer_code, integer, [not_null, unique]},
     {name, varchar, not_null},
     {surname, varchar, not_null},
     {is_a_company, boolean, [not_null, {default, false}]}];
%%
table (account) ->
    [{account_code, integer, [not_null, unique]},
     {account_description, varchar, not_null}];
%%
table (owner) ->
    [refers_to (customer),
     refers_to (account)];
%%
table (operation_type) ->
    [{operation_code, varchar, [not_null, unique]},
     {operation_code_description, varchar, not_null}];
%%
table (operation) ->
    [refers_to (account),
     refers_to (operation_type),
     {operation_description, varchar, not_null},
     {operation_date, date, not_null},
     {amount, {decimal, 9, 2}, not_null}].
%%


