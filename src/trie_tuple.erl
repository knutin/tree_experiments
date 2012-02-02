-module(trie_tuple).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").


new() ->
    {trie, new_children()}.

insert({trie, Children}, Keys, V) ->
    {trie, insert(Children, Keys, V)};

insert(nil, [K], V) ->
    setelement(K+1, new_children(), {V, nil});

insert(Nodes, [K], V) ->
    case element(K+1, Nodes) of
        {_, Children} ->
            setelement(K+1, Nodes, {V, Children});
        nil ->
            setelement(K+1, Nodes, {V, nil})
    end;

insert(nil, Keys, V) ->
    insert(new_children(), Keys, V);

insert(Nodes, [K | Rest], V) ->
    case element(K+1, Nodes) of
        {Val, Children} ->
            setelement(K+1, Nodes, {Val, insert(Children, Rest, V)});
        nil ->
            setelement(K+1, Nodes, {nil, insert(new_children(), Rest, V)})
    end.

new_children() ->
    erlang:make_tuple(10, nil).


find({_, Children}, Keys) ->
    find(Children, Keys);

find(Children, [K]) ->
    element(1, element(K+1, Children));

find(Children, [K | Rest]) when size(Children) =:= 10 ->
    find(element(K+1, Children), Rest);

find(nil, _) ->
    not_found.





%% find(_, []) ->
%%     not_found;



%% find({_, Children}, [K]) ->
%%     element(1, element(K+1, Children));
%% %% find(Nodes, [K]) ->
%% %%     error_logger:info_msg("Nodes: ~p, K: ~p~n", [Nodes, K+1]),
%% %%     element(1, element(K+1, Nodes));

%% find({_, Children}, [K | Rest]) ->
%%     io:format("recursing, k: ~p, rest: ~p~n", [K, Rest]),
%%     find(element(K+1, Children), Rest).
%% %% find(Nodes, [K | Rest]) when size(Nodes) =:= 10 ->
%% %%     find(element(K+1, Nodes), Rest).



int2key(I) ->
    list2key(integer_to_list(I)).

list2key(L) ->
    lists:map(fun ($0) -> 0;
                  ($1) -> 1;
                  ($2) -> 2;
                  ($3) -> 3;
                  ($4) -> 4;
                  ($5) -> 5;
                  ($6) -> 6;
                  ($7) -> 7;
                  ($8) -> 8;
                  ($9) -> 9;
                  (_) -> throw(badarg)
              end, L).

%%
%% TESTS
%%


simple_test() ->
    T0 = insert(new(), [1], key1),
    ?assertEqual(key1, find(T0, [1])).

insert_test() ->
    T0 = insert(new(), [1, 1, 1], key1),
    T1 = insert(T0, [1, 1, 1, 1], key2),
    T2 = insert(T1, [1], key3),
    ?assertEqual(key1, find(T2, [1, 1, 1])),
    ?assertEqual(key2, find(T2, [1, 1, 1, 1])),
    ?assertEqual(key3, find(T2, [1])).


%% big_insert_test() ->
%%     Start = 100000000000000,
%%     N = 100000,
%%     Keys = lists:map(fun (I) -> int2key(I) end,
%%                      lists:seq(Start, Start+N)),

%%     Tree = lists:foldl(fun (K, T) ->
%%                                insert(T, K, <<255>>)
%%                        end, new(), Keys),
%%     error_logger:info_msg("~p keys in ~p mb~n",
%%                           [N, (erts_debug:flat_size(Tree) * 8) / 1024 / 1024]).


get_bench() ->
    Start = 100000000000000,
    N = 1000000,
    Keys = lists:map(fun (I) -> int2key(I) end,
                     lists:seq(Start, Start+N)),
    ReadKeys = lists:map(fun int2key/1,
                         lists:seq(Start, Start+1000)),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, K)
                       end, new(), Keys),
    spawn(
      fun() ->
              lists:foreach(
                fun (_) ->
                        StartTime = now(),
                        multi_search(Tree, ReadKeys),
                        io:format("Time: ~p us~n", [timer:now_diff(now(), StartTime)])
                end, lists:seq(1, 25))
      end).

multi_search(T, Keys) ->
    lists:map(fun (K) ->
                      find(T, K)
              end, Keys).


prop_test() ->
    ?assert(quickcheck(prop_insert_0())),
    ?assert(quickcheck(prop_insert_1())).

keys() ->
    ?SIZED(Size, list(choose(0, Size))).

prop_insert_0() ->
    ?FORALL({K, Xs}, {choose(0, 100000000), keys()},
            begin
                %%io:format("insert key: ~w, keys: ~w~n", [K, Xs]),
                Tree = lists:foldl(fun (I, T) -> insert(T, int2key(I), I) end,
                                   new(), Xs),
                %%io:format("tree: ~p~n", [Tree]),
                K =:= find(insert(Tree, int2key(K), K), int2key(K))
            end).

prop_insert_1() ->
    ?FORALL(K, choose(1, 100000000),
            begin
                K =:= find(insert(new(), int2key(K), K), int2key(K))
            end).
