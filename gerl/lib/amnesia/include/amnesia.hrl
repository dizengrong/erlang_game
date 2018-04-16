
-record ('$refers_to', { from,
                         to,
                         on_update = null,
                         on_delete = null }).


-record (join, { using, on }).

-record (left_join, { using, on }).

-record (right_join, { using, on }).

-record (full_join, { using, on }).

-define (JOIN,          #join { using = undefined, on = undefined }).
-define (JOIN_USING(X), #join { using = X, on = undefined }).
-define (JOIN_ON(X),    #join { using = undefined, on = X }).

-define (LEFT_JOIN_USING(X),  #left_join { using = X, on = undefined }).
-define (LEFT_JOIN_ON(X),     #left_join { using = undefined, on = X }).

-define (RIGHT_JOIN_USING(X), #right_join { using = X, on = undefined }).
-define (RIGHT_JOIN_ON(X),    #right_join { using = undefined, on = X }).

-define (FULL_JOIN_USING(X),  #full_join { using = X, on = undefined }).
-define (FULL_JOIN_ON(X),     #full_join { using = undefined, on = X }).

