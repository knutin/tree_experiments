-module(trie_gb).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

new() ->
    gb_trees:empty().

insert(Nodes, [K], V) ->
    case gb_trees:lookup(K, Nodes) of
        {value, {_, C}} ->
            gb_trees:enter(K, {V, C}, Nodes);
        none ->
            gb_trees:enter(K, {V, new()}, Nodes)
    end;

insert(Nodes, [K | Rest], V) ->
    {Value, Children} = case gb_trees:lookup(K, Nodes) of
                            {value, {Val, C}} -> {Val, C};
                            none          -> {undefined, new()}
                        end,
    gb_trees:enter(K, {Value, insert(Children, Rest, V)}, Nodes).



find(Nodes, [K]) ->
    case gb_trees:lookup(K, Nodes) of
        {value, {Val, _}} ->
            Val;
        none ->
            not_found
    end;

find(Nodes, [K | Rest]) ->
    case gb_trees:lookup(K, Nodes) of
        {value, {_, C}} ->
            find(C, Rest);
        none ->
            not_found
    end.

bin2key(<<>>) ->
    [];
bin2key(<<K:8/bitstring, Rest/bitstring>>) ->
    [K | bin2key(Rest)].

int2key(I) ->
    lists:map(fun (D) ->
                      element(1, string:to_integer([D]))
              end, integer_to_list(I)).


%%
%% TESTS
%%

insert_test() ->
    T0 = insert(new(), [1, 1, 1], <<255>>),
    T1 = insert(T0, [1, 1, 2], <<254>>),
    ?assertEqual(<<255>>, find(T1, [1, 1, 1])),
    ?assertEqual(<<254>>, find(T1, [1, 1, 2])).

bin2key_test() ->
    ?assertEqual([<<"5">>, <<"3">>, <<"8">>], bin2key(<<"538">>)).

big_insert_test() ->
    Start = 100000000000000,
    N = 1000000,
    Keys = lists:map(fun (I) -> int2key(I) end,
                     lists:seq(Start, Start+N)),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, <<255>>)
                       end, new(), Keys),
    error_logger:info_msg("~p keys in ~p mb~n",
                          [N, (erts_debug:flat_size(Tree) * 8) / 1024 / 1024]).
