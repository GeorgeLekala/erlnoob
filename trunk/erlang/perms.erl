-module(perms).

-export([perms/1]).

perms([]) -> [ [] ];
perms(List) ->
    [ [Head | Tail] || Head <- List, Tail <- perms(List--[Head]) ].
