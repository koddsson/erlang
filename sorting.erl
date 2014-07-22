-module(sorting).
-export([mergesort/1]).

mergesort([E]) -> [E];
mergesort(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    merge(mergesort(L1), mergesort(L2)).

merge([], L) -> L;
merge(L, []) -> L;

merge([H1|T1], [H2|T2]) when H1 < H2 -> [H1] ++ merge(T1, [H2] ++ T2);
merge([H1|T1], [H2|T2]) when H1 > H2 -> [H2] ++ merge(T2, [H1] ++ T1).
