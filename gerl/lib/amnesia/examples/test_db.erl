-module (test_db).

-include_lib ("amnesia/include/amnesia_db_def.hrl").

%%

driver_info () ->
  [{driver, mysql_drv},
   {host, "localhost"},
   {user, "test"},
   {password, "test"},
   {logging, true}].

%%

tables () ->
  [customer,
   product,
   orders,
   order_line].

%%
table (customer) ->
  [ {name, varchar, [unique, not_null]},
    {address, varchar, not_null} ];
%%
table (product) ->
  [ {product_code, varchar, [unique, not_null]},
    {description, varchar, not_null},
    {price, {decimal, 10, 2}, not_null} ];
%%
table (orders) ->
  [ {order_number, integer, [unique, not_null]},
    {order_date, date, not_null},
    refers_to (customer) ];
%%
table (order_line) ->
  [ refers_to (orders),
    refers_to (product),
    {quantity, integer, not_null} ].
%%

