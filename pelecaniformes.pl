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

% Order
hasParent(pelecanidae, pelecaniformes).
hasParent(ardeidae, pelecaniformes).
hasParent(threskiornithdae, pelecaniformes).

% Family
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

% Genus
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

% hasParent2
% Like hasParent, but only accepts compound names (since hasParent ONLY takes raw names
% but isAStrict/isa ONLY take compound names)
% Order
hasParent2(pelecanidae, pelecaniformes).
hasParent2(ardeidae, pelecaniformes).
hasParent2(threskiornithdae, pelecaniformes).

% Family
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

% Species
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


% hasCommonName
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
habitat(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), habitat(C,B).

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
rangesTo(A,B) :- atom(A), (order(A) ; family(A) ; genus(A)) , isaStrict(C,A), hasCompoundName(_,S,C), species(S), rangesTo(C,B).

hasCompoundName(G, S, N) :- hasCommonName(G, S, X), hasCommonName(N, X), \+(G = N), \+(S = N).

hasSciName(C, N) :- hasCommonName(N, C), hasCompoundName(X, Y, N), !.
hasSciName(C, N) :- hasCommonName(N, C), genus(N); family(N); order(N).

isaStrict(A, B) :- hasParent2(A,B).
isaStrict(A, B) :- hasParent2(A,X) , isaStrict(X,B).
isaStrict(A, A).

isa(A, B) :- isaHelper(A, B).
% Use nonvar to determine whether or not we're querying with variables and thus should return anything
isa(A, B) :- nonvar(A) , hasCommonName(X, A) , isaHelper(X, B).
isa(A, B) :- nonvar(B) , hasCommonName(Y, B) , isaHelper(A, Y).
isa(A, B) :- nonvar(A) , nonvar(B) , hasCommonName(X, A), hasCommonName(Y, B), isaHelper(X, Y).

% Like isaStrict, but with more specific rules for self-matching
isaHelper(A, B) :- hasParent2(A,B).
isaHelper(A, B) :- hasParent2(A,X) , isaStrict(X,B).
isaHelper(A, A) :- order(A) ; family(A) ; hasParent2(A, X) ; (var(A) , hasCommonName(Y, A)).


synonym(A, B) :- \+(A = B), hasCommonName(B, A); hasCommonName(A, B).
synonym(A, B) :- \+(A = B), hasCommonName(C, A), hasCommonName(C, B).

countSpecies(A, N) :- \+order(A), \+family(A), \+genus(A), \+hasCompoundName(G, S, A), N = 0.
countSpecies(A, N) :- hasCompoundName(_, S, A), species(S), N = 1.
%countSpecies(A, N) :- N = 0.
%countSpecies(A, N) :- species(X), isaStrict(X, A), countSpecies(A, N), N is N+1.

%rangesTo(A, P).

%food(A, B).

%nesting(A, B).
          
%behavior(A, B).

%conservation(A, B).
