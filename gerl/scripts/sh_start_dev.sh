cp src/cache/gerl_cache.app.src ebin/gerl_cache.app
cp src/center_srv/gerl_center_srv.app.src ebin/gerl_center_srv.app
cp src/chat/gerl_chat.app.src ebin/gerl_chat.app
cp src/gateway/gerl_gateway.app.src ebin/gerl_gateway.app
cp src/map_srv/gerl_map_srv.app.src ebin/gerl_map_srv.app

rm -f log/*

erl -pa ./ebin -pa ./deps/goldrush/ebin -pa ./deps/lager/ebin -pa ./deps/ranch/ebin -pa ./lib/amnesia/ebin -name gerl1_1@127.0.0.1 -setcookie gerl -s reloader start -s main start 1
