%%
%% test.erl
%%
-module (test).

-include ("test_db.hrl").

-export ([create/0, populate/0, dump/0]).


create() ->
  amnesia:generate(test_db, [{driver, mysql_drv},
                             {host, "localhost"},
                             {user, "root"},
                             {password, "---"},
                             make_db,
                             make_hdr]).


populate() ->
  amnesia:open(test_db),

  {ok, Cust1} = amnesia:add_new (test_db,
                                 #customer {name = "Ciccio",
                                            address = "Via X"}),
  {ok, Cust2} = amnesia:add_new (test_db,
                                 #customer {name = "Pippo",
                                            address = "Via Y"}),
  {ok, Cust3} = amnesia:add_new (test_db,
                                 #customer {name = "Pluto",
                                            address = "Via Z"}),

  {ok, P1} = amnesia:add_new (test_db,
                              #product { product_code = "001",
                                         description = "CPU Intel",
                                         price = 231.10 }),

  {ok, P2} = amnesia:add_new (test_db,
                              #product { product_code = "002",
                                         description = "Compact Flash 4G",
                                         price = 57.90 }),

  {ok, P3} = amnesia:add_new (test_db,
                              #product { product_code = "003",
                                         description = "Hard Disk 500G",
                                         price = 190.77 }),

  {ok, Order} = amnesia:add_new (test_db,
                                 #orders { order_number = 30,
                                           order_date = {2008, 7, 17},
                                           customer = Cust2 }),

  amnesia:add_new (test_db, #order_line { orders = Order,
                                          product = P2,
                                          quantity = 3 }),

  amnesia:add_new (test_db, #order_line { orders = Order,
                                          product = P1,
                                          quantity = 10 }),

  amnesia:add_new (test_db,
                   [#product { product_code = "004",
                               description = "Data Server",
                               price = 5200.00 },
                    #orders { order_number = 31,
                              order_date = {2008, 7, 20},
                              customer = Cust1},
                    #order_line { orders = '$2',
                                  product = P3,
                                  quantity = 2} ,
                    #order_line { orders = '$2',
                                  product = '$1',
                                  quantity = 11 }
                    ]),
  ok.


dump() ->
  amnesia:open(test_db),
  amnesia:fetch (test_db, customer).

