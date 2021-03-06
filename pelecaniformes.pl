/*
 * CPSC 449 - Prolog Assignment
 * Group Members: Steven Vi, Philip Chow-Wah, Matthew Hylton, Max Zunti
*/

/*
*** Logical taxonomy: ***

order(pelecaniformes).

  family(pelecanidae).
    genus(pelecanus).
      species(pelecanus_erythrorhynchos).
      species(pelecanus_occidentalis).

  family(ardeidae).
    genus(botaurus).
      species(botaurus_lentiginosus).
    genus(ixobrychus).
      species(ixobrychus_exilis).
    genus(ardea).
      species(ardea_herodias).
      species(ardea_alba).
    genus(egretta).
      species(egretta_thula).
      species(egretta_caerulea).
      species(egretta_tricolor).
      species(egretta_rufescens).
    genus(bubulcus).
      species(bubulcus_ibis);
    genus(butorides).
      species(butorides_virescens).
    genus(nycticorax).
      species(nycticorax_nycticorax).
    genus(nyctanassa).
      species(nyctanassa_violacea).

    family(threskiornithdae).
      genus(eudocimus).
        species(eudocimus_albus).
      genus(plegadis).
        species(plegadis_falcinellus).
        species(plegadis_chihi).
      genus(platalea).
        species(platalea_ajaja).
*/

% Define the base order, families, genuses, and species
order(pelecaniformes).

family(pelecanidae).
family(ardeidae).
family(threskiornithdae).

genus(pelecanus).
genus(botaurus).
genus(ixobrychus).
genus(ardea).
genus(egretta).
genus(bubulcus).
genus(butorides).
genus(nycticorax).
genus(nyctanassa).
genus(eudocimus).
genus(plegadis).
genus(platalea).

species(erythrorhynchos).
species(occidentalis).
species(lentiginosus).
species(exilis).
species(herodias).
species(alba).
species(thula).
species(caerulea).
species(tricolor).
species(rufescens).
species(ibis).
species(virescens).
species(nycticorax).
species(violacea).
species(albus).
species(falcinellus).
species(chihi).
species(ajaja).

%%%%%%%%%% HasParent %%%%%%%%%%

% Parents of family
hasParent(pelecanidae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(threskiornithdae, pelecaniformes).

% Parents of genus
hasParent(pelecanus, pelecanidae).
hasParent(botaurus, ardeidae).
hasParent(ixobrychus, ardeidae).
hasParent(ardea, ardeidae).
hasParent(egretta, ardeidae).
hasParent(bubulcus, ardeidae).
hasParent(butorides, ardeidae).
hasParent(nycticorax, ardeidae).
hasParent(nyctanassa, ardeidae).
hasParent(eudocimus, threskiornithdae).
hasParent(plegadis, threskiornithdae).
hasParent(platalea, threskiornithdae).

% Parents of Species
hasParent(erythrorhynchos, pelecanus).
hasParent(occidentalis, pelecanus).
hasParent(lentiginosus, botaurus).
hasParent(exilis, ixobrychus).
hasParent(herodias, ardea).
hasParent(alba, ardea).
hasParent(thula, egretta).
hasParent(caerulea, egretta).
hasParent(tricolor, egretta).
hasParent(rufescens, egretta).
hasParent(ibis, bubulcus).
hasParent(virescens, butorides).
hasParent(nycticorax, nycticorax).
hasParent(violacea, nyctanassa).
hasParent(albus, eudocimus).
hasParent(falcinellus, plegadis).
hasParent(chihi, plegadis).
hasParent(ajaja, platalea).

%%%%%%%%%% HasParent 2 %%%%%%%%%%
% Like hasParent, but only accepts compound names (since hasParent ONLY takes raw names
% but isAStrict/isa ONLY take compound names)

% Parents of family
hasParent2(pelecanidae, pelecaniformes).
hasParent2(ardeidae, pelecaniformes).
hasParent2(threskiornithdae, pelecaniformes).

% Parents of genus
hasParent2(pelecanus, pelecanidae).
hasParent2(botaurus, ardeidae).
hasParent2(ixobrychus, ardeidae).
hasParent2(ardea, ardeidae).
hasParent2(egretta, ardeidae).
hasParent2(bubulcus, ardeidae).
hasParent2(butorides, ardeidae).
hasParent2(nycticorax, ardeidae).
hasParent2(nyctanassa, ardeidae).
hasParent2(eudocimus, threskiornithdae).
hasParent2(plegadis, threskiornithdae).
hasParent2(platalea, threskiornithdae).

% Parents of species
hasParent2(pelecanus_erythrorhynchos, pelecanus).
hasParent2(pelecanus_occidentalis, pelecanus).
hasParent2(botaurus_lentiginosus, botaurus).
hasParent2(ixobrychus_exilis, ixobrychus).
hasParent2(ardea_herodias, ardea).
hasParent2(ardea_alba, ardea).
hasParent2(egretta_thula, egretta).
hasParent2(egretta_caerulea, egretta).
hasParent2(egretta_tricolor, egretta).
hasParent2(egretta_rufescens, egretta).
hasParent2(bubulcus_ibis, bubulcus).
hasParent2(butorides_virescens, butorides).
hasParent2(nycticorax_nycticorax, nycticorax).
hasParent2(nyctanassa_violacea, nyctanassa).
hasParent2(eudocimus_albus, eudocimus).
hasParent2(plegadis_falcinellus, plegadis).
hasParent2(plegadis_chihi, plegadis).
hasParent2(platalea_ajaja, platalea).


%%%%%%%%%% hasCommonName %%%%%%%%%%
hasCommonName(pelecanus, pelican).
hasCommonName(pelecanus_erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus_occidentalis, brownPelican).
hasCommonName(botaurus, bittern).
hasCommonName(botaurus_lentiginosus, americanBittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ixobrychus_exilis, leastBittern).
hasCommonName(ardea, heron).
hasCommonName(ardea_herodias, greatBlueHeron).
hasCommonName(ardea_alba, greatEgret).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(egretta_thula, snowyEgret).
hasCommonName(egretta_caerulea, littleBlueHeron).
hasCommonName(egretta_tricolor, tricoloredHeron).
hasCommonName(egretta_rufescens, reddishEgret).
hasCommonName(bubulcus, egret).
hasCommonName(bubulcus_ibis, cattleEgret).
hasCommonName(butorides, heron).
hasCommonName(butorides_virescens, greenHeron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nycticorax_nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(nyctanassa_violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus, ibis).
hasCommonName(eudocimus_albus, whiteIbis).
hasCommonName(plegadis, ibis).
hasCommonName(plegadis_falcinellus, glossyIbis).
hasCommonName(plegadis_chihi, whiteFacedIbis).
hasCommonName(platalea, spoonbill).
hasCommonName(platalea_ajaja, roseateSpoonbill).

hasCommonName(pelecanus, erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus, occidentalis, brownPelican).
hasCommonName(botaurus, lentiginosus, americanBittern).
hasCommonName(ixobrychus, exilis, leastBittern).
hasCommonName(ardea, herodias, greatBlueHeron).
hasCommonName(ardea, alba, greatEgret).
hasCommonName(egretta, thula, snowyEgret).
hasCommonName(egretta, caerulea, littleBlueHeron).
hasCommonName(egretta, tricolor, tricoloredHeron).
hasCommonName(egretta, rufescens, reddishEgret).
hasCommonName(bubulcus, ibis, cattleEgret).
hasCommonName(butorides, virescens, greenHeron).
hasCommonName(nycticorax, nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus, albus, whiteIbis).
hasCommonName(plegadis, falcinellus, glossyIbis).
hasCommonName(plegadis, chihi, whiteFacedIbis).
hasCommonName(platalea, ajaja, roseateSpoonbill).

%%%%%%%%%% hasSciName %%%%%%%%%%
hasSciName(C, N) :- hasCommonName(N, C), hasCompoundName(X, Y, N), !.
hasSciName(C, N) :- hasCommonName(N, C), genus(N); family(N); order(N).

%%%%%%%%%% hasCompoundName %%%%%%%%%%
hasCompoundName(G, S, N) :- hasCommonName(G, S, X), hasCommonName(N, X), \+(G = N), \+(S = N).

%%%%%%%%%% isaStrict %%%%%%%%%%
isaStrict(A, B) :- hasParent2(A,B).                             % Check for a direct parent
isaStrict(A, A) :- hasParent2(A,_).                             % Ensure that every valid entry is a parent of itself
isaStrict(A, B) :- hasParent2(A,X) , isaStrict(X,B), \+(X=B).   % Try to find SOME parent X of A, then recurse to see if B is an ancestor of X

%%%%%%%%%% isa %%%%%%%%%%
isa(A, B) :- isaHelper(A, B).     % First, simply check if B is an ancestor of A (difference from isaStrict mentioned in isaHelper)
% The below heads are only reached if either A or B is a common name
% We nonvar to determine whether or not we're querying using variables, and thus whether we should return common names in a variable query
isa(A, B) :- nonvar(A) , hasCommonName(X, A) , isaHelper(X, B). % IF A is NOT a variable, then find the specific name X associated with common name A and see if it has ancestor B
isa(A, B) :- nonvar(B) , hasCommonName(Y, B) , isaHelper(A, Y). % IF B is NOT a variable, then find the specific name Y associated with common name B and see if A has parent Y
isa(A, B) :- nonvar(A) , nonvar(B) , hasCommonName(X, A), hasCommonName(Y, B), isaHelper(X, Y). % IF NEITHER A nor B are variables (and are both common names), find their specific
                                                                                                % names and check for ancestry

% Works like isaStrict, but with more specific rules for self-matching
isaHelper(A, B) :- hasParent2(A,B).
% We only allow names which are either order names, family names, and compound species names
isaHelper(A, A) :- order(A) ; family(A) ; hasParent2(A, X) ; (var(A) , hasCommonName(Y, A)).
isaHelper(A, B) :- hasParent2(A,X) , isaStrict(X,B), \+(X=B).

%%%%%%%%%% synonym %%%%%%%%%%
synonym(A, B) :- hasCommonName(B, A), A \= B.                    %A is a common name of scientific name B
synonym(A, B) :- hasCommonName(A, B), A \= B.
synonym(A, B) :- hasCommonName(C, A), hasCommonName(C, B), A \= B.

%%%%%%%%%% countSpecies %%%%%%%%%%
countSpecies(A, 0) :- \+order(A), \+family(A), \+genus(A), \+hasCompoundName(_,_,A).                  % If A is some name that isn't order, family, genus, or compound species name, then N is 0.
countSpecies(A, 1) :- hasCompoundName(_, S, A), species(S).                                           % If A is a species, then n is 1
countSpecies(A, N) :- atom(A), (order(A) ; family(A) ; genus(A)), makeList(A, List), length(List, N). % N is equal to the length of a list with all species who parents are A
makeList(A, List) :- findall(species(X), makeListGoal(A,X), List ).                                   % make a list of all species who parents are A
makeListGoal(A, S) :-  isaStrict(C, A), hasCompoundName(_, S, C), species(S).                         % Set goal for findAll, find all species that whos parents are A

%%%%%%%%%% rangesTo %%%%%%%%%%
rangesTo(pelecanus_erythrorhynchos,canada).
rangesTo(pelecanus_erythrorhynchos,alberta).
rangesTo(botaurus_lentiginosus,canada).
rangesTo(botaurus_lentiginosus,alberta).
rangesTo(ardea_herodias, canada).
rangesTo(ardea_herodias, alberta).
rangesTo(ardea_alba, canada).
rangesTo(bubulcus_ibis, canada).
rangesTo(butorides_virescens, canada).
rangesTo(nycticorax_nycticorax, canada).
rangesTo(nycticorax_nycticorax, alberta).
%If A is a genus, family, or order, then return the species under A range
rangesTo(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), rangesTo(C,B).

%%%%%%%%%% habitat %%%%%%%%%%
habitat(pelecanus_erythrorhynchos,lakePond).
habitat(pelecanus_occidentalis,ocean).
habitat(botaurus_lentiginosus,marsh).
habitat(ixobrychus_exilis,marsh).
habitat(ardea_herodias, marsh).
habitat(ardea_alba, marsh).
habitat(egretta_thula, marsh).
habitat(egretta_caerulea, marsh).
habitat(egretta_tricolor, marsh).
habitat(egretta_rufescens, marsh).
habitat(bubulcus_ibis, marsh).
habitat(butorides_virescens, marsh).
habitat(nycticorax_nycticorax, marsh).
habitat(nyctanassa_violacea, marsh).
habitat(eudocimus_albus, marsh).
habitat(plegadis_falcinellus, marsh).
habitat(plegadis_chihi, marsh).
habitat(platalea_ajaja, marsh).
%If A is a genus, family, or order, then return the species under A habitat
habitat(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), habitat(C,B).

%%%%%%%%%% food %%%%%%%%%%
food(pelecanus_erythrorhynchos,fish).
food(pelecanus_occidentalis,fish).
food(botaurus_lentiginosus,fish).
food(ixobrychus_exilis,fish).
food(ardea_herodias, fish).
food(ardea_alba, fish).
food(egretta_thula, fish).
food(egretta_caerulea, fish).
food(egretta_tricolor, fish).
food(egretta_rufescens, fish).
food(bubulcus_ibis, insects).
food(butorides_virescens, fish).
food(nycticorax_nycticorax, fish).
food(nyctanassa_violacea, insects).
food(eudocimus_albus, insects).
food(plegadis_falcinellus, insects).
food(plegadis_chihi, insects).
food(platalea_ajaja, fish).
%If A is a genus, family, or order, then return the species under A food
food(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), food(C,B).

%%%%%%%%%% nesting %%%%%%%%%%
nesting(pelecanus_erythrorhynchos,ground).
nesting(pelecanus_occidentalis,tree).
nesting(botaurus_lentiginosus,ground).
nesting(ixobrychus_exilis,ground).
nesting(ardea_herodias, tree).
nesting(ardea_alba, tree).
nesting(egretta_thula, tree).
nesting(egretta_caerulea, tree).
nesting(egretta_tricolor, tree).
nesting(egretta_rufescens, tree).
nesting(bubulcus_ibis, tree).
nesting(butorides_virescens, tree).
nesting(nycticorax_nycticorax, tree).
nesting(nyctanassa_violacea, tree).
nesting(eudocimus_albus, tree).
nesting(plegadis_falcinellus, ground).
nesting(plegadis_chihi, ground).
nesting(platalea_ajaja, tree).
%If A is a genus, family, or order, then return the species under A nesting
nesting(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), nesting(C,B).

%%%%%%%%%% behavior %%%%%%%%%%
behavior(pelecanus_erythrorhynchos,surfaceDive).
behavior(pelecanus_occidentalis,aerialDive).
behavior(botaurus_lentiginosus,stalking).
behavior(ixobrychus_exilis,stalking).
behavior(ardea_herodias, stalking).
behavior(ardea_alba, stalking).
behavior(egretta_thula, stalking).
behavior(egretta_caerulea, stalking).
behavior(egretta_tricolor, stalking).
behavior(egretta_rufescens, stalking).
behavior(bubulcus_ibis, surfaceDive).
behavior(butorides_virescens, stalking).
behavior(nycticorax_nycticorax, stalking).
behavior(nyctanassa_violacea, stalking).
behavior(eudocimus_albus, surfaceDive).
behavior(plegadis_falcinellus, surfaceDive).
behavior(plegadis_chihi, probing).
behavior(platalea_ajaja, groundForager).
%If A is a genus, family, or order, then return the species under A behavior
behavior(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), behavior(C,B).

%%%%%%%%%% conservation %%%%%%%%%%
conservation(pelecanus_erythrorhynchos,lc).
conservation(pelecanus_occidentalis,lc).
conservation(botaurus_lentiginosus,lc).
conservation(ixobrychus_exilis,lc).
conservation(ardea_herodias, lc).
conservation(ardea_alba, lc).
conservation(egretta_thula, lc).
conservation(egretta_caerulea, lc).
conservation(egretta_tricolor, lc).
conservation(egretta_rufescens, nt).
conservation(bubulcus_ibis, lc).
conservation(butorides_virescens, lc).
conservation(nycticorax_nycticorax, lc).
conservation(nyctanassa_violacea, lc).
conservation(eudocimus_albus, lc).
conservation(plegadis_falcinellus, lc).
conservation(plegadis_chihi, lc).
conservation(platalea_ajaja, lc).
%If A is a genus, family, or order, then return the species under A conservation
conservation(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), conservation(C,B).
