del log\*.* /f /q
erl -name "robot1@127.0.0.1" -pa ./ebin -pa ./deps/lager/ebin -pa ./deps/goldrush/ebin -s main 

