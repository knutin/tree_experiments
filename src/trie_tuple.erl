-module(trie_tuple).
-compile([export_all]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(b2i(B), list_to_integer(binary_to_list(B))).
-define(i2b(I), list_to_binary(integer_to_list(I))).


-type key() :: [0..9].
-type value() :: any().
-type children() :: nil | {leaf(), leaf(), leaf(), leaf(), leaf(),
                           leaf(), leaf(), leaf(), leaf(), leaf()}.
-type compact_leaf() :: {value(), compact, key()}.
-type leaf() :: {value(), children()} | {value(), nil} | compact_leaf().

-type trie() :: {trie, children()}.

-spec new() -> trie().
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
    {V, compact, Keys};
    %%insert(new_children(), Keys, V);

insert(Nodes, [K | Rest], V) ->
    case element(K+1, Nodes) of
        {Val, Children} ->
            setelement(K+1, Nodes, {Val, insert(Children, Rest, V)});
        {_, compact, _} = C->
            setelement(K+1, Nodes, {nil, insert(unroll(C), Rest, V)});
        nil ->
            setelement(K+1, Nodes, insert(nil, Rest, V))
    end.

unroll({Val, compact, [K]}) ->
    setelement(K+1, new_children(), {Val, nil});
unroll({Val, compact, [K | Rest]}) ->
    setelement(K+1, new_children(), {Val, compact, Rest}).


new_children() ->
    erlang:make_tuple(10, nil).


find({_, Children}, Keys) ->
    find(Children, Keys);

find({V, compact, Keys}, Keys) ->
    V;

find(Children, [K]) ->
    element(1, element(K+1, Children));

find(Children, [K | Rest]) ->
    find(element(K+1, Children), Rest);

find(nil, _) ->
    not_found.





int2key(I) ->
    bin2key(<<I:64/integer>>).


%% bin2key(<<_B0, _B1, B2, B3, B4, B5, B6, B7>>) ->
%%     [B2, B3, B4, B5, B6, B7];

bin2key(<<>>) ->
    [];
bin2key(<<Byte, B/binary>>) ->
    [char2int(Byte) | bin2key(B)].

char2int($0) -> 0;
char2int($1) -> 1;
char2int($2) -> 2;
char2int($3) -> 3;
char2int($4) -> 4;
char2int($5) -> 5;
char2int($6) -> 6;
char2int($7) -> 7;
char2int($8) -> 8;
char2int($9) -> 9.

    

%%
%% TESTS
%%


simple_test() ->
    T0 = insert(new(), [1], key1),
    ?assertEqual(key1, find(T0, [1])),
    T0.

insert_test() ->
    T0 = insert(new(), [1, 1, 1], key1),
    T1 = insert(T0, [1, 1, 1, 1], key2),
    T2 = insert(T1, [1], key3),
    ?assertEqual(key1, find(T2, [1, 1, 1])),
    ?assertEqual(key2, find(T2, [1, 1, 1, 1])),
    ?assertEqual(key3, find(T2, [1])),
    T2.


compact_test() ->
    T0 = insert(new(), [1,2,3], val1),
    T1 = insert(T0, [1,2,4], val2),
    ?assertEqual(val1, find(T0, [1,2,3])),
    ?assertEqual(val2, find(T1, [1,2,4])).

unroll_test() ->
    C = {foo, compact, [1,2,3]},
    ?assertEqual({nil, {foo, compact, [2,3]}, nil, nil, nil,
                  nil, nil, nil, nil, nil}, unroll(C)).


big_insert_test() ->
    Start = 100000000000000,
    N = 1000000,
    Keys = lists:map(fun (I) -> bin2key(?i2b(I)) end,
                     lists:seq(Start, Start+N, 200)),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, <<255>>)
                       end, new(), Keys),
    error_logger:info_msg("~p keys in ~p mb~n",
                          [N, (erts_debug:flat_size(Tree) * 8) / 1024 / 1024]).

from_file(File) ->
    {ok, B} = file:read_file(File),
    Keys = lists:map(fun (K) -> bin2key(K) end,
                     binary_to_term(B)),
    ReadKeys = lists:sublist(Keys, 1000),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, <<255>>)
                       end, new(), Keys),
    io:format("size: ~p mb~n", [erts_debug:flat_size(Tree) * 8 / 1024 / 1024]),

    time_reads(Tree, ReadKeys).

time_reads(Tree, ReadKeys) ->
    spawn(
      fun() ->
              lists:foreach(
                fun (_) ->
                        StartTime = now(),
                        multi_search(Tree, ReadKeys),
                        io:format("Time: ~p us~n",
                                  [timer:now_diff(now(), StartTime)])
                end, lists:seq(1, 15))
      end).


get_bench() ->
    Start = 100000000000000,
    N = 100000,
    Spread = 50,
    Keys = lists:map(fun (I) -> bin2key(?i2b(I)) end,
                     lists:seq(Start, Start+N, Spread)),
    ReadKeys = lists:map(fun (I) -> bin2key(?i2b(I)) end,
                         lists:seq(Start, Start+1000, Spread)),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, K)
                       end, new(), Keys),

    time_reads(Tree, ReadKeys).

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
