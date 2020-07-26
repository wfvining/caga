-module(ca_ga).
-behavior(gen_ga).
-export([init/1, fitness/1, mutate/1, crossover/2]).

-define(P_MUTATION, 0.05).
-define(STATES, [0, 1, 2]).
-define(RADIUS, 5).
-define(NUM_ICS, 100).
-define(CA_SIZE, 128).

express(RuleTable) ->
    ca:table(
      lists:zip(
        [ [L5, L4, L3, L2, L1, C, R1, R2, R3, R4, R5]
          || L5 <- ?STATES,
             L4 <- ?STATES,
             L3 <- ?STATES,
             L2 <- ?STATES,
             L1 <- ?STATES,
             C  <- ?STATES,
             R1 <- ?STATES,
             R2 <- ?STATES,
             R3 <- ?STATES,
             R4 <- ?STATES,
             R5 <- ?STATES ],
        RuleTable)).

uniform_half() ->
    rand:uniform_real() * 0.5.

generate_ic() ->
    Fraction0 = uniform_half(),
    Fraction1 = uniform_half(),
    Fraction2 = (1.0 - (Fraction0 + Fraction1)),
    generate_ic(Fraction0, Fraction1, Fraction2).

%% TODO: Make sure that the maximum of F1, F2, F3 is not a tie.
generate_ic(F0, F1, _F2) ->
    GenerateState =
        fun() ->
                P = rand:uniform(),
                case rand:uniform() of
                    P when P < F0 -> 0;
                    P when P < F0 + F1 -> 1;
                    _ -> 2
                end
        end,
    % ??? It might be good to generate random CA_SIZE
    [ GenerateState() || _ <- lists:seq(1, ?CA_SIZE) ].

generate_ics(NumICs) ->
    [ generate_ic() || _ <- lists:seq(1, NumICs) ].

fitness(RuleTable) ->
    CARule = express(RuleTable),
    Results = [ evaluate(CARule, IC) || IC <- generate_ics(?NUM_ICS) ],
    lists:foldl(fun(true, Acc) -> 1 + Acc;
                 (false, Acc) -> Acc
              end, 0, Results) / length(Results).

plurality(Map) ->
    sets:from_list(
      [ K || {K, _} <- maps:fold(
                         fun(_, V, Acc=[{_, VMax}|_]) when V < VMax ->
                                 Acc;
                            (K, V, [{_, VMax}|_]) when V > VMax ->
                                 [{K, V}];
                            (K, V, Acc=[{_, VMax}|_]) when V == VMax ->
                                 [{K, V}|Acc];
                            (K, V, []) ->
                                 [{K, V}]
                         end,
                         [], Map)]).

is_solved(StartingPlurality, EndingPlurality) ->
    case sets:size(EndingPlurality) of
        1 -> sets:is_element(
               hd(sets:to_list(EndingPlurality)),
               StartingPlurality);
        _ -> false
    end.

evaluate(Rule, InitialState) ->
    CAStart = ca:new(InitialState),
    CAEvolution = ca:iterate(
                    CAStart,
                    Rule,
                    ?RADIUS,
                    trunc(rand:normal(5*?CA_SIZE, ?CA_SIZE))),
    CAEnd = lists:last(CAEvolution),
    StartingPlurality = plurality(ca:state_counts(CAStart)),
    EndingPlurality = plurality(ca:state_counts(CAEnd)),
    is_solved(StartingPlurality, EndingPlurality).

mutate(RuleTable) ->
    lists:map(
      fun ({P, _}) when P < ?P_MUTATION ->
              lists:nth(rand:uniform(length(?STATES)), ?STATES);
          ({_, T}) ->
              T
      end,
      [ {rand:uniform_real(), Transition} || Transition <- RuleTable ]).

crossover(LeftRules, RightRules) ->
    CutIndex = rand:uniform(length(LeftRules)),
    {LeftBegin, LeftEnd} = lists:split(CutIndex, LeftRules),
    {RightBegin, RightEnd} = lists:split(CutIndex, RightRules),
    Coin = rand:uniform_real(),
    if Coin < 0.5 ->
            LeftBegin ++ RightEnd;
       Coin >= 0.5 ->
            RightBegin ++ LeftEnd
    end.

random_rule() ->
    [ lists:nth(rand:uniform(length(?STATES)), ?STATES)
      || _ <- lists:seq(1, trunc(math:pow(length(?STATES), (?RADIUS * 2) + 1)))].

init(NumGenomes) ->
    [ random_rule() || _ <- lists:seq(1, NumGenomes) ].
