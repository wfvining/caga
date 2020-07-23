-module(ca).
-export([new/1, update/3, iterate/4, eca/1, table/1, show/1, state_counts/1]).
-export_type([ca/0]).

-type state()  :: atom() | pos_integer().
-type rule()   :: fun((list(state())) -> state()).
-type radius() :: pos_integer().
-type transition_table() :: list({list(state()), state()}).
-opaque ca() :: array:array().

%% @doc Create a new CA from a list of cell states.
-spec new(list(atom())) -> ca().
new(Cells) ->
    array:from_list(Cells).

-spec update(ca(), rule(), radius()) -> ca().
update(Cells, Rule, Radius) ->
    NumCells = array:size(Cells),
    Neighborhood =
        fun(Center) ->
                [ array:get(wrap_index(X, NumCells), Cells)
                  || X <- lists:seq(Center - Radius, Center + Radius)]
        end,
    Neighborhoods = [ Neighborhood(Center)
                      || Center <- lists:seq(0, NumCells-1) ],
    array:from_list(lists:map(Rule, Neighborhoods)).

-spec iterate(ca(), rule(), radius(), pos_integer()) -> ca().
iterate(Cells, Rule, Radius, Iterations) ->
    lists:reverse(
      lists:foldl(
        fun(_, States=[Prev|_]) ->
                [update(Prev, Rule, Radius)|States]
        end,
        [Cells], lists:seq(0, Iterations))).

-spec show(ca()) -> ok.
show(Cells) ->
    io:format("~s~n", [lists:flatten([ io_lib:format("~p", [C]) || C <- array:to_list(Cells) ])]).

-spec state_counts(ca()) -> maps:map(state(), pos_integer()).
state_counts(Cells) ->
    array:foldl(
      fun(_Index, State, Acc) ->
              maps:update_with(State, fun(Count) -> Count + 1 end, 1, Acc)
      end, #{}, Cells).

-spec eca(pos_integer()) -> rule().
eca(Number) when Number < 128 ->
    fun([L, C, R]) ->
            <<Ix:3>> = <<L:1, C:1, R:1>>,
            case nth_bit_set(Ix, Number) of
                true -> 1;
                false -> 0
            end
    end.

nth_bit_set(N, X) ->
    (X band (1 bsl N)) /= 0.

-spec table(transition_table()) -> state().
table(TransitionTable) ->
    TTMap = maps:from_list(TransitionTable),
    fun(State) ->
            maps:get(State, TTMap)
    end.

-spec wrap_index(integer(), pos_integer()) -> pos_integer().
wrap_index(X, Max) when X < 0 ->
    Max + X;
wrap_index(X, Max) when X >= 0 ->
    X rem Max.
