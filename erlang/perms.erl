-module(perms).

-export([ok/1, perms/1]).

ok(String) ->
    List = perms(String),
    io:format("~p\n", [List]).

perms([]) -> [ [] ];
perms(List) ->
    [ [Head | Tail] || Head <- List, Tail <- perms(List--[Head]) ].
