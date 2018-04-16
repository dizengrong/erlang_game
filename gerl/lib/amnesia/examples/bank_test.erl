%%
%% bank_test.erl
%%
-module (bank_test).

-include ("bank.hrl").
-include_lib ("amnesia/include/amnesia.hrl").

-compile ([export_all]).
%%
%% TO COMPILE AND RUN THE EXAMPLE, FIRST GENERATE THE DATABASE
%% WITH:
%%   amnesia:generate (bank, [make_db, {make_hdr, "."},
%%                            {dba_user, "<your_root_user>"},
%%                            {dba_password, "<your_root_password>"}]).
%%

populate () ->
    amnesia:open ({local, bank}, bank),

    %% Add customers
    CustomerData = [{1, "Mario", "Rossi"},
                    {2, "Giuseppe",  "Verdi"},
                    {3, "Giuseppe",  "Spampinato"},
                    {4, "Giuseppe", "Russo"},
                    {11, "ACME Inc.", "", true},
                    {12, "COMP Ltd.", "", true},
                    {13, "Salvatore", "Russo"},
                    {21, "Paolo", "Rossi"},
                    {22, "Fabio", "Verdi"},
                    {31, "Giuseppe", "Bianchi"},
                    {14, "Mario", "Bianchi"},
                    {23, "Fabio", "Rossi"},
                    {32, "Salvatore", "Verdi"}],
    amnesia:add_new (bank,
                     lists:map (fun ({A, B, C}) ->
                                        #customer { customer_code = A,
                                                    name = B,
                                                    surname = C };
                                    ({A, B, C, D}) ->
                                        #customer { customer_code = A,
                                                    name = B,
                                                    surname = C,
                                                    is_a_company = D}
                                end, CustomerData )),

    %% Add bank accounts
    AccountData = [{10048, "Libretto di risparmio"},
                   {10049, "Conto Aziendale"},
                   {10050, "Conto Personale"},
                   {10051, "Conto Giovani"}],
    amnesia:add_new (bank,
                     [ #account {account_code = A,
                                 account_description = B} ||
                         {A, B} <- AccountData ]),

    %% Add account owners (link customers and accounts)
    OwnerData = [{10048, 1},
                 {10049, 2},
                 {10050, 3},
                 {10050, 4},
                 {10051, 11},
                 {10051, 12},
                 {10051, 21},
                 {10051, 1}],
    lists:foreach (
      fun ({AccountCode, OwnerCode}) ->
              {ok, [Account]} =
                  amnesia:fetch (bank, account,
                                 {"account_code = $1", [AccountCode]}),
              {ok, [Owner]} =
                  amnesia:fetch (bank, customer,
                                 {"customer_code = $1", [OwnerCode]}),
              amnesia:add_new (bank,
                               #owner { customer = Owner,
                                        account = Account })
      end, OwnerData),


    OperationType = [{"VE", "Versamento"},
                     {"PR", "Prelievo"},
                     {"PRBM", "Prelievo Bancomat"},
                     {"SPCC", "Spese mantenimento C/C"},
                     {"INAT", "Interessi attivi"},
                     {"INPS", "Interessi passivi"},
                     {"ASS", "Libretto assegni"},
                     {"AQBM", "Acquisto Bacomat"}],
    lists:foreach (
      fun ({OpCode, OpDesc}) ->
              amnesia:add_new (bank,
                               #operation_type
                               { operation_code = OpCode,
                                 operation_code_description = OpDesc })
      end, OperationType),

    Operations =
        [{10048, "VE", "Versamento iniziale", {2004,4,2},  5000.00},
         {10048, "PRBM",  "Prelevamento Bancomat", {2004,4,12},  -50.00},
         {10048, "INAT",  "Interessi maturati",  {2004,4,14},  5.36},
         {10048, "AQBM", "Supermercati", {2004,4,16},  -146.33},
         {10048, "AQBM",  "Abbigliamento", {2004,4,16},  -245.33},
         {10049, "VE",  "Versamento iniziale", {2004,11,1},  1500.00},
         {10049, "ASS", "Libretto assegni",  {2004,11,1}, -10.00},
         {10049, "AQBM",  "PC Store",  {2004,11,10},  -867.00},
         {10049, "AQBM",  "Abbigliamento", {2004,11,15},  -150.67},
         {10049, "AQBM",  "PC Store",  {2004,12,1},  -450.77},
         {10049, "AQBM",  "PC Store",  {2004,12,1},  -127.50},
         {10050, "VE",  "Versamento iniziale", {2004,7,1},  4000.00},
         {10050, "ASS", "Libretto assegni",  {2004,7,2},  -10.00},
         {10050, "PRBM",  "Prelevamento",  {2004,7,10},  -80.00},
         {10050, "PR",  "Prelevamento",  {2004,12,10},  -2000.00},
         {10051, "VE",  "Versamento iniziale", {2004,4,2},  5000.00},
         {10051, "ASS", "Libretto assegni",  {2004,4,10},  -10.00},
         {10051, "PRBM",  "Prelevamento Bancomat", {2004,4,14},  -80.00},
         {10051, "INAT",  "Interessi maturati",  {2004,4,14},  5.36},
         {10051, "AQBM",  "Supermercati",  {2004,4,16},  -146.33},
         {10051, "AQBM",  "Abbigliamento", {2004,4,16},  -245.33},
         {10051, "PR",  "Prelevamento",  {2004,5,1},  -1500.00},
         {10051, "PR",  "Prelevamento",  {2004,6,1},  -1500.00},
         {10051, "ASS", "Libretto assegni",  {2004,6,1},  -10.00},
         {10051, "AQBM",  "PC Store",  {2004,6,25},  -2574.00}],

    lists:foreach (
      fun ({AccCode, OpCode, OpDescr, OpDate, OpAmount}) ->
              {ok, [Account]} =
                  amnesia:fetch (bank, account,
                                 {"account_code = $1", [AccCode]}),
              {ok, [Operation]} =
                  amnesia:fetch (bank, operation_type,
                                 {"operation_code = $1", [OpCode]}),
              amnesia:add_new (
                bank, #operation { account = Account,
                                   operation_type = Operation,
                                   operation_description = OpDescr,
                                   operation_date = OpDate,
                                   amount = OpAmount })
      end, Operations),

    ok.


queries () ->
    %% All accounts with more than an owner
    {ok, Q1} =
        amnesia:fetch (bank, [owner, ?JOIN, account], {},
                       [{aggregate, "count(*)", integer, account_code,
                         {"__aggregated_data__ > 1", []}},
                        {order_by, '__aggregated_data__', desc}]),
    io:format ("All accounts with more than an owner:~n"),
    lists:foreach (
      fun ([Num, _Owner, Account]) ->
              io:format ("----- Account Code: ~p, Name: ~p, # of owners: ~p~n",
                         [Account#account.account_code,
                          Account#account.account_description,
                          Num])
      end, Q1),
    io:format ("~n"),

    %% All people with more than an account
    {ok, Q2} =
        amnesia:fetch (bank, [owner, ?JOIN, customer], {},
                       [{aggregate, "count(*)", integer, customer_code,
                         {"__aggregated_data__ > 1", []}}]),
    io:format ("All people with more than an account:~n"),
    lists:foreach (
      fun ([Num, _Owner, Cust]) ->
              io:format (
                "----- Customer Code: ~p, Name: ~p, Surname: ~p, # of accounts: ~p~n",
                [Cust#customer.customer_code,
                 Cust#customer.name,
                 Cust#customer.surname,
                 Num])
      end, Q2),
    io:format ("~n"),

    %% Total amount for each account
    {ok, Q3} =
        amnesia:fetch (bank, [operation, ?JOIN, account], {},
                       [{aggregate, "sum(amount)", decimal, account_code}]),
    io:format ("Total amount for each account:~n"),
    lists:foreach (
      fun ([Total, _Op, Acc]) ->
              io:format (
                "----- Account Code: ~p, Account Name: ~p, Total Amount: ~p~n",
                [Acc#account.account_code,
                 Acc#account.account_description,
                 Total])
      end, Q3),
    io:format ("~n"),

    %% All accounts with a negative total
    {ok, Q4} =
        amnesia:fetch (bank, [operation, ?JOIN, account], {},
                       [{aggregate, "sum(amount)", decimal, account_code,
                         {"__aggregated_data__ < 0", []}}]),
    io:format ("All accounts with a negative total:~n"),
    lists:foreach (
      fun ([Total, _Op, Acc]) ->
              io:format (
                "----- Account Code: ~p, Account Name: ~p, Total Amount: ~p~n",
                [Acc#account.account_code,
                 Acc#account.account_description,
                 Total])
      end, Q4),
    io:format ("~n"),

    ok.

