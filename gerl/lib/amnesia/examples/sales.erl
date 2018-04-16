-module (sales).

-include_lib ("amnesia/include/amnesia_db_def.hrl").

%%

driver_info () ->
  [{driver, mysql_drv},
   {host, "localhost"},
   {user, "sales_office"},
   {password, "sales"},
  {logging, true}].

%%

tables () ->
  [customer,
   product,
   orders,
   order_line].

%%
table (customer) ->
  [ {customer_code, integer, [unique, not_null]},
    {name, varchar, not_null},
    {address, varchar, not_null},
    {email, varchar, [not_null, {default, ""}]}];
%%
table (product) ->
  [ {product_code, varchar, [unique, not_null]},
    {description, varchar, not_null},
    {price, {decimal, 10, 2}, [not_null, {default, 1.0}]} ];
%%
table (orders) ->
  [ {order_number, integer, [unique, not_null]},
    {order_date, date, [not_null, {default, {2007, 7, 12}}]},
    refers_to (customer) ];
%%
table (order_line) ->
  [ refers_to (orders),
    refers_to (product),
    {quantity, integer, not_null} ].
%%

