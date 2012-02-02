-module(trie_tuple).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

new() ->
    erlang:make_tuple(10, nil).


insert(nil, [K], V) ->
    setelement(K+1, new(), {V, nil});

insert(Nodes, [K], V) ->
    case element(K+1, Nodes) of
        {_, Children} ->
            setelement(K+1, Nodes, {V, Children});
        nil ->
            setelement(K+1, Nodes, {V, nil})
    end;

insert(nil, Keys, V) ->
    insert(new(), Keys, V);

insert(Nodes, [K | Rest], V) ->
    setelement(K+1, Nodes, insert(element(K+1, Nodes), Rest, V)).




find(nil, _) ->
    not_found;
find(Nodes, [K]) ->
    element(1, element(K+1, Nodes));
find(Nodes, [K | Rest]) ->
    find(element(K+1, Nodes), Rest).




int2key(I) ->
    lists:map(fun (D) ->
                      element(1, string:to_integer([D]))
              end, integer_to_list(I)).


%%
%% TESTS
%%

insert_test() ->
    T0 = insert(new(), [1, 1, 1], <<255>>),
    T1 = insert(T0, [1, 1, 1, 1], <<254>>),
    ?assertEqual(<<255>>, find(T1, [1, 1, 1])),
    ?assertEqual(<<254>>, find(T1, [1, 1, 1, 1])).


big_insert_test() ->
    Start = 100000000000000,
    N = 100000,
    Keys = lists:map(fun (I) -> int2key(I) end,
                     lists:seq(Start, Start+N)),

    Tree = lists:foldl(fun (K, T) ->
                               insert(T, K, <<255>>)
                       end, new(), Keys),
    error_logger:info_msg("~p keys in ~p mb~n",
                          [N, (erts_debug:flat_size(Tree) * 8) / 1024 / 1024]).


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
