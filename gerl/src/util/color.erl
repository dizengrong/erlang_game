-module(color).
%% @doc come from: https://github.com/julianduque/erlang-color
%% print ANSI colors in console
%% usage:
% io:format("Hello, this is the ~s color~n", [ color:red("red") ]).
% io:format("Hello, this is on ~s~n", [ color:on_blue("blue blackground") ]).
% Make sure you use the ~s string type, ~p will escape the ANSI code.

% xterm 256 colors
% io:format("Hello, this color is ~s~n", [ color:rgb([0,1,0], "green") ]).
% io:format("Hello, this color is ~s~n", [ color:rgb([0,3,0], "greener") ]).
% io:format("Hello, this color is ~s~n", [ color:rgb([0,5,0], "greenest") ]).

% true 24-bit colors
% io:format("The solarized template is ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~s ~n", [
%   color:true("002B36", "base03"), color:true("073642", "base02"), color:true("586E75", "base01"),
%   color:true("657B83", "base00"), color:true("839496", "base0"),  color:true("93A1A1", "base1"),
%   color:true("EEE8D5", "base2"),  color:true("FDF6E3", "base3"),  color:true("B58900", "yellow"),
%   color:true("CB4B16", "orange"), color:true("DC322F", "red"),    color:true("D33682", "magenta"),
%   color:true("6C71C4", "violet"), color:true("268BD2", "blue"),   color:true("2AA198", "cyan"),
%   color:true("859900", "green")]).

-export([black/1, blackb/1, red/1, redb/1, green/1, greenb/1, blue/1, blueb/1]).
-export([yellow/1, yellowb/1, magenta/1, magentab/1, cyan/1, cyanb/1, white/1, whiteb/1]).
-export([on_black/1, on_red/1, on_green/1, on_blue/1, on_yellow/1, on_magenta/1, on_cyan/1, on_white/1]).
-export([rgb/2, on_rgb/2]).
-export([true/2, on_true/2]).

-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(BOLD, <<"1">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).

%% Colors
-define(BLACK, <<"30">>).
-define(RED, <<"31">>).
-define(GREEN, <<"32">>).
-define(YELLOW, <<"33">>).
-define(BLUE, <<"34">>).
-define(MAGENTA, <<"35">>).
-define(CYAN, <<"36">>).
-define(WHITE, <<"37">>).
-define(DEFAULT, <<"39">>).

%% Background colors
-define(BLACK_BG, <<"40">>).
-define(RED_BG, <<"41">>).
-define(GREEN_BG, <<"42">>).
-define(YELLOW_BG, <<"43">>).
-define(BLUE_BG, <<"44">>).
-define(MAGENTA_BG, <<"45">>).
-define(CYAN_BG, <<"46">>).
-define(WHITE_BG, <<"47">>).
-define(DEFAULT_BG, <<"49">>).

%% RGB
-define(RGB_FG, [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG, [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors
-define(TRUE_COLOR_FG, [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG, [<<"48">>, ?SEP, <<"2">>]).

black(Text)      -> [color(?BLACK),      Text, reset()].
blackb(Text)     -> [colorb(?BLACK),     Text, reset()].
red(Text)        -> [color(?RED),        Text, reset()].
redb(Text)       -> [colorb(?RED),       Text, reset()].
green(Text)      -> [color(?GREEN),      Text, reset()].
greenb(Text)     -> [colorb(?GREEN),     Text, reset()].
yellow(Text)     -> [color(?YELLOW),     Text, reset()].
yellowb(Text)    -> [colorb(?YELLOW),    Text, reset()].
blue(Text)       -> [color(?BLUE),       Text, reset()].
blueb(Text)      -> [colorb(?BLUE),      Text, reset()].
magenta(Text)    -> [color(?MAGENTA),    Text, reset()].
magentab(Text)   -> [colorb(?MAGENTA),   Text, reset()].
cyan(Text)       -> [color(?CYAN),       Text, reset()].
cyanb(Text)      -> [colorb(?CYAN),      Text, reset()].
white(Text)      -> [color(?WHITE),      Text, reset()].
whiteb(Text)     -> [colorb(?WHITE),     Text, reset()].
on_black(Text)   -> [color(?BLACK_BG),   Text, reset_bg()].
on_red(Text)     -> [color(?RED_BG),     Text, reset_bg()].
on_green(Text)   -> [color(?GREEN_BG),   Text, reset_bg()].
on_blue(Text)    -> [color(?BLUE_BG),    Text, reset_bg()].
on_yellow(Text)  -> [color(?YELLOW_BG),  Text, reset_bg()].
on_magenta(Text) -> [color(?MAGENTA_BG), Text, reset_bg()].
on_cyan(Text)    -> [color(?CYAN_BG),    Text, reset_bg()].
on_white(Text)   -> [color(?WHITE_BG),   Text, reset_bg()].

rgb(RGB, Text) ->
  [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

on_rgb(RGB, Text) ->
  [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(RGB), ?END, Text, reset()].

on_true(RGB, Text) ->
  [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(RGB), ?END, Text, reset()].

%% Internal
color(Color) ->
  <<?ESC/binary, Color/binary, ?END/binary>>.

colorb(Color) ->
  <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

rgb_color([R, G, B]) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
  integer_to_list(16 + (R * 36) + (G * 6) + B).

true_color([R1, R2, G1, G2, B1, B2]) ->
  R = erlang:list_to_integer([R1, R2], 16),
  G = erlang:list_to_integer([G1, G2], 16),
  B = erlang:list_to_integer([B1, B2], 16),
  true_color([R, G, B]);

true_color([R, G, B]) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
  [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].

reset() ->
  <<?ESC/binary, ?RST/binary, ?END/binary>>.

reset_bg() ->
  <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.