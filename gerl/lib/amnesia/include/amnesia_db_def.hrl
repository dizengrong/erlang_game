
-export ([driver_info/0,
          tables/0,
          table/1,
          refers_to/1,
          refers_to/2]).

-include_lib ("amnesia/include/amnesia.hrl").

refers_to (TableRef) ->
    #'$refers_to' { to = TableRef }.

refers_to (TableRef, Options) ->
    OnUpdate = proplists:get_value (on_update, Options),
    OnDelete = proplists:get_value (on_delete, Options),
    #'$refers_to' { to = TableRef,
                    on_update = OnUpdate,
                    on_delete = OnDelete }.


