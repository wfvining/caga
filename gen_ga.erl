-module(gen_ga).
-export([new/2, step/1]).
-export_type([ga/0]).

-type genotype() :: any().
-type fitness() :: any().
-type phenotype() :: {fitness(), genotype()}.

-record(ga, {population=[] :: list(genotype()),
             module :: module(),
             tournament_size :: pos_integer(),
             history=[] :: list(phenotype())}).

-opaque ga() :: #ga{}.

-callback init(pos_integer()) -> list(genotype()).
-callback fitness(Genotype :: genotype()) -> fitness().
-callback mutate(Genotype :: genotype()) -> genotype().
-callback crossover(LeftParent :: genotype(),
                    RightParent :: genotype()) -> genotype().

-spec new(module(),
          proplist:proplist()) -> ga().
new(GAMod, Options) ->
    #ga{module=GAMod,
        population=GAMod:init(proplists:get_value(population_size, Options, 100)),
        tournament_size=proplists:get_value(tournament_size, Options, 4)}.

-spec step(ga()) -> ga().
step(GAState=#ga{module=GAMod,
                 population=Population}) ->
    Phenotypes = evaluate(Population, GAMod),
    NextGeneration =
        [ GAMod:mutate(GAMod:crossover(LeftParent, RightParent))
          || {LeftParent, RightParent} <-
                 lists:zip(select(Phenotypes, 4), select(Phenotypes, 4))],
    GAState#ga{population=NextGeneration,
               history=[Phenotypes|GAState#ga.history]}.

-spec evaluate(list(genotype()), module()) -> list(phenotype()).
evaluate(Population, GAMod) ->
    [{GAMod:fitness(Genome), Genome} || Genome <- Population].

-spec select(list(phenotype()), pos_integer()) -> list(genotype()).
select(Phenotypes, TournamentSize) ->
    select(Phenotypes, TournamentSize, []).

select(Phenotypes, _, NewPopulation)
  when length(Phenotypes) == length(NewPopulation) ->
    NewPopulation;
select(Phenotypes, TournamentSize, NewPopulation) ->
    select(Phenotypes, TournamentSize,
           [tournament(Phenotypes, TournamentSize)|NewPopulation]).

tournament(Phenotypes, Size) ->
    MaxIndex = length(Phenotypes),
    Tournament = [lists:nth(rand:uniform(MaxIndex), Phenotypes)
                  || _ <- lists:seq(1, Size)],
    element(2, hd(lists:reverse(lists:keysort(1, Tournament)))).
