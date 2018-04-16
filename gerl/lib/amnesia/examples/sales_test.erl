-module (sales_test).

-include ("sales.hrl").
-include_lib ("amnesia/include/amnesia.hrl").

-compile ([export_all]).

populate() ->
    amnesia:open({local, sales}, sales),

    {ok, Cust1} = amnesia:add_new (sales,
                                   #customer {customer_code = 102341,
                                              name = "John",
                                              address = "XXXXX"}),
    {ok, Cust2} = amnesia:add_new (sales,
                                   #customer {customer_code = 394021,
                                              name = "Corrado",
                                              address = "YYYYYY",
                                              email = "corrado@yyy"}),
    {ok, Cust3} = amnesia:add_new (sales,
                                   #customer {customer_code = 102391,
                                              name = "Dave",
                                              address = "Dave's home",
                                              email = "dave@zzz"}),

    {ok, P1} = amnesia:add_new (sales,
                                #product { product_code = "001",
                                           description = "CPU Intel",
                                           price = 231.10 }),

    {ok, P2} = amnesia:add_new (sales,
                                #product { product_code = "002",
                                           description = "Compact Flash 4G",
                                           price = 57.90 }),

    {ok, P3} = amnesia:add_new (sales,
                                #product { product_code = "003",
                                           description = "Hard Disk 500G",
                                           price = 190.77 }),

    {ok, Order} = amnesia:add_new (sales,
                                   #orders { order_number = 30,
                                             order_date = {2008, 7, 17},
                                             customer = Cust2 }),

    amnesia:add_new (sales, #order_line { orders = Order,
                                          product = P2,
                                          quantity = 3 }),

    amnesia:add_new (sales, #order_line { orders = Order,
                                          product = P1,
                                          quantity = 10 }),

    amnesia:add_new (sales,
                     [#product { product_code = "004",
                                 description = "Data Server",
                                 price = 5200.00 },
                      #orders { order_number = 31,
                                customer = Cust1},
                      #order_line { orders = '$2',
                                    product = P3,
                                    quantity = 2} ,
                      #order_line { orders = '$2',
                                    product = '$1',
                                    quantity = 11 }
                     ]),
    ok.


test_join () ->
    amnesia:fetch (sales, [customer, ?JOIN, orders, ?JOIN, order_line]).

test_join (Pid) ->
    amnesia:fetch (Pid, [customer, ?JOIN, orders, ?JOIN, order_line]).

test_connections () ->
    {ok, [Order]} = amnesia:fetch (sales, orders, {"order_number = 31", []}),
    io:format ("Order #31 is: ~p~n", [Order]),
    {ok, OrderWithCust} = amnesia:load_referenced (sales, Order),
    io:format ("Order #31 with customer explicited is: ~p~n", [OrderWithCust]),
    {ok, OrderLines} = amnesia:load_referenced (sales, Order, order_line),
    io:format ("The items of order #31 are: ~p~n", [OrderLines]),
    OrderLinesWithProduct =
        lists:map (fun (Line) ->
                           {ok, LineWithProduct} =
                               amnesia:load_referenced (sales, Line),
                   LineWithProduct
                   end, OrderLines),
    io:format ("The items of order #31, with products explicited, are:~n~p~n",
               [OrderLinesWithProduct]),
    ok.


test_fetch () ->
    {ok, X1} = amnesia:fetch (sales, customer),
    io:format ("SIMPLE FETCH = ~p~n~n", [X1]),

    {ok, X2} = amnesia:fetch (sales,
                              [customer, ?JOIN, orders, ?JOIN, order_line]),
    io:format ("FETCH WITH JOINS = ~p~n~n", [X2]),

    {ok, X3} = amnesia:fetch (sales, orders, {"order_number = $1", [30]}),
    io:format ("SIMPLE FETCH WITH SELECTION = ~p~n~n", [X3]),

    {ok, X4} = amnesia:fetch (sales,
                              [customer, ?JOIN, orders, ?JOIN, order_line],
                              {"name = $1", ["Corrado"]}),
    io:format ("FETCH WITH JOINS AND SELECTION = ~p~n~n", [X4]),

    {ok, X5} = amnesia:fetch (sales, customer,
                              {}, [{order_by, name}]),
    io:format ("SIMPLE FETCH WITH ORDERING = ~p~n~n", [X5]),

    {ok, X6} = amnesia:fetch (sales,
                              [customer, ?JOIN, orders],
                              {}, [{order_by, order_number}]),
    io:format ("FETCH WITH JOINS AND ORDERING = ~p~n~n", [X6]),

    ok.


test_aggregate() ->
    {ok, X1} = amnesia:fetch (sales, customer, {},
                              [{aggregate, "count(*)", integer}]),
    io:format ("SIMPLE COUNT = ~p~n~n", [X1]),

    {ok, X2} = amnesia:fetch (sales, product, {},
                              [{aggregate, "max(price)", decimal}]),
    io:format ("SIMPLE MAX = ~p~n~n", [X2]),

    {ok, X3} = amnesia:fetch (sales, product, {},
                              [{aggregate, "count(*)",
                                integer, product_code}]),
    io:format ("COUNT WITH AGGREGATION (GROUP BY) = ~p~n~n", [X3]),

    {ok, X4} = amnesia:fetch (sales, [product, ?JOIN, order_line], {},
                              [{aggregate, "sum(quantity)",
                                integer, product_code}]),
    io:format ("COUNT WITH AGGREGATION (GROUP BY) AND JOIN = ~p~n~n", [X4]),

    {ok, X5} = amnesia:fetch (sales, [product, ?JOIN, order_line], {},
                              [{aggregate, "sum(quantity)",
                                integer, product_code,
                                {"__aggregated_data__ > $1", [5]}}]),
    io:format ("COUNT WITH AGGREGATION (GROUP BY), JOIN AND HAVING= ~p~n~n",
               [X5]),

    {ok, X6} = amnesia:fetch (sales, [product, ?JOIN, order_line], {},
                              [{aggregate, "sum(quantity)",
                                integer, product_code},
                               {order_by, '__aggregated_data__', desc}]),
    io:format ("COUNT WITH AGGREGATION (GROUP BY), JOIN AND ORDERING= ~p~n~n",
               [X6]),

    {ok, X7} = amnesia:fetch (sales,
                              [product, ?JOIN, order_line, ?JOIN, orders], {},
                              [{aggregate, "sum(quantity * price)",
                                decimal, order_number}]),
    io:format ("~p~n~n",
               [X7]),

    X7.


test_cursor () ->
    {ok, CursorID} =
        amnesia:create_cursor (
          sales,
          amnesia:fetch (sales,
                         [customer, ?JOIN, orders, ?JOIN, order_line] )),
    io:format ("CURSOR ID = ~p~n~n", [CursorID]),

    show_cursor_data (CursorID, 1).


show_cursor_data (CursorID, N) ->
    case amnesia:nth (sales, CursorID, N) of
        {end_of_data} -> amnesia:delete_cursor (sales, CursorID);
        {ok, X} ->
            io:format ("Item #~p = ~p~n~n", [N, X]),
            show_cursor_data (CursorID, N + 1)
    end.
