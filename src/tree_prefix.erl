-module(tree_prefix).
-compile([export_all]).

new() ->
    erlang:make_tuple(10, undefined).

insert(undefined, [K], V) ->
    setelement(K+1, mk_children(), V);

%% insert(Nodes, [K], V) ->
%%     io:format("setting value: ~p~n", [Nodes]),
%%     setelement(K+1, Nodes, V);

insert(undefined, Keys, V) ->
    insert(mk_children(), Keys, V);

insert(Nodes, [K], V) ->
    setelement(K+1, Nodes, V);

insert(Nodes, [K | Rest], V) ->
    setelement(K+1, Nodes, insert(element(K+1, Nodes), Rest, V)).



mk_children() ->
    erlang:make_tuple(10, undefined).


search(V, []) ->
    V;

search(undefined, _) ->
    not_found;
search(Nodes, [K]) ->
    element(K+1, Nodes);

search(Nodes, [K | Rest]) ->
    search(element(K+1, Nodes), Rest).



search_test() ->
    search(insert_test(), [1, 1, 1, 1]).

insert_test() ->
    insert(new(), [1, 1, 1, 1], <<1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1, 1:1>>).

big_insert_test(From, To) ->
    lists:foldl(fun (Int, T) ->
                        insert(T, int_to_key(Int), Int)
                end, new(), lists:seq(From, To)).


int_to_key(I) ->
    lists:map(fun (D) ->
                      element(1, string:to_integer([D]))
              end, integer_to_list(I)).

size(Start, N) ->
    erts_debug:flat_size(big_insert_test(Start, Start+N)) * 8 / 1024 / 1024.

get_bench(Start, N) ->
    spawn(
      fun() ->
              T = big_insert_test(Start, Start+N),
              ReadKeys = lists:map(fun int_to_key/1,
                                   lists:seq(Start, Start+1000)),
              lists:foreach(fun (_) ->
                                    {Time, _Res} = timer:tc(?MODULE, multi_search, [T, ReadKeys]),
                                    io:format("Time: ~p us~n", [Time])
                            end, lists:seq(1, 25))
          end).

multi_search(T, Keys) ->
    lists:map(fun (K) ->
                      search(T, K)
              end, Keys).
