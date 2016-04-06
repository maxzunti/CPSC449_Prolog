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
hasParent(threskiornthdae, pelecaniformes).

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
hasParent(eudocimus, threskiornthdae).
hasParent(plegadis, threskiornthdae).
hasParent(platalea, threskiornthdae).

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

% hasCompoundParent
% Like hasParent, but only accepts compound names (since hasParent ONLY takes raw names
% but isAStrict/isa ONLY take compound names)
% Order
hasCompoundParent(pelecanidae, pelecaniformes).
hasCompoundParent(ardeidae, pelecaniformes).
hasCompoundParent(threskiornthdae, pelecaniformes).

% Family
hasCompoundParent(pelecanus, pelecanidae).
hasCompoundParent(botaurus, ardeidae).
hasCompoundParent(ixobrychus, ardeidae).
hasCompoundParent(ardea, ardeidae).
hasCompoundParent(egretta, ardeidae).
hasCompoundParent(bubulcus, ardeidae).
hasCompoundParent(butorides, ardeidae).
hasCompoundParent(nycticorax, ardeidae).
hasCompoundParent(nyctanassa, ardeidae).
hasCompoundParent(eudocimus, threskiornithdae).
hasCompoundParent(plegadis, threskiornithdae).
hasCompoundParent(platalea, threskiornithdae).

% Species
hasCompoundParent(pelecanus_erythrorhynchos, pelecanus).
hasCompoundParent(pelecanus_occidentalis, pelecanus).
hasCompoundParent(botaurus_lentiginosus, botaurus).
hasCompoundParent(ixobrychus_exilis, ixobrychus).
hasCompoundParent(ardea_herodias, ardea).
hasCompoundParent(ardea_alba, ardea).
hasCompoundParent(egretta_thula, egretta).
hasCompoundParent(egretta_caerulea, egretta).
hasCompoundParent(egretta_tricolor, egretta).
hasCompoundParent(egretta_rufescens, egretta).
hasCompoundParent(bubulcus_ibis, bubulcus).
hasCompoundParent(butorides_virescens, butorides).
hasCompoundParent(nycticorax_nycticorax, nycticorax).
hasCompoundParent(nyctanassa_violacea, nyctanassa).
hasCompoundParent(eudocimus_albus, eudocimus).
hasCompoundParent(plegadis_falcinellus, plegadis).
hasCompoundParent(plegadis_chihi, plegadis).
hasCompoundParent(platalea_ajaja, platalea).


% hasCommonName
hasCommonName(pelecanus, pelican).
hasCommonName(pelecanus_erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus_occidentalis, brownPelican).
hasCommonName(erythrorhynchos, americanWhitePelican).
hasCommonName(occidentalis, brownPelican).
hasCommonName(botaurus, bittern).
hasCommonName(botaurus_lentiginosus, americanBittern).
hasCommonName(lentiginosus, americanBittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ixobrychus_exilis, leastBittern).
hasCommonName(exilis, leastBittern).
hasCommonName(ardea, heron).
hasCommonName(ardea_herodias, greatBlueHeron).
hasCommonName(ardea_alba, greatEgret).
hasCommonName(herodias, greatBlueHeron).
hasCommonName(alba, greatEgret).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(egretta_thula, snowyEgret).
hasCommonName(egretta_caerulea, littleBlueHeron).
hasCommonName(egretta_tricolor, tricoloredHeron).
hasCommonName(egretta_rufescens, reddishEgret).
hasCommonName(thula, snowyEgret).
hasCommonName(caerulea, littleBlueHeron).
hasCommonName(tricolor, tricoloredHeron).
hasCommonName(rufescens, reddishEgret).
hasCommonName(bubulcus, egret).
hasCommonName(bubulcus_ibis, cattleEgret).
hasCommonName(ibis, cattleEgret).
hasCommonName(butorides, heron).
hasCommonName(butorides_virescens, greenHeron).
hasCommonName(virescens, greenHeron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nycticorax_nycticorax, blackCrownedNightHeron).
hasCommonName(nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(nyctanassa_violacea, yellowCrownedNightHeron).
hasCommonName(violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus, ibis).
hasCommonName(eudocimus_albus, whiteIbis).
hasCommonName(albus, whiteIbis).
hasCommonName(plegadis, ibis).
hasCommonName(plegadis_falcinellus, glossyIbis).
hasCommonName(plegadis_chihi, whiteFacedIbis).
hasCommonName(falcinellus, glossyIbis).
hasCommonName(chihi, whiteFacedIbis).
hasCommonName(platalea, spoonbill).
hasCommonName(platalea_ajaja, roseateSpoonbill).
hasCommonName(ajaja, roiseateSpoonbill).

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

hasCompoundName(G, S, N) :- hasCommonName(G, S, X), hasCommonName(N, X), \+(G = N), \+(S = N).

hasSciName(C, N) :- hasCommonName(N, C), hasCompoundName(X, Y, N).
hasSciName(C, N) :- hasCommonName(N, C), genus(N); family(N); order(N).

isaStrict(A, B) :- hasCompoundParent(A,B).
isaStrict(A, B) :- hasCompoundParent(A,X) , isaStrict(X,B).
isaStrict(A, A).

isa(A, B) :- isaStrict(A, B).
%isa(A, B) :- isaStrict(A, B), !, fail.
isa(A, B) :- hasCommonName(X, A) , isaStrict(X, B).
isa(A, B) :- hasCommonName(Y, B) , isaStrict(A, Y).
isa(A, B) :- hasCommonName(X, A), hasCommonName(Y, B), isaStrict(X, Y).


synonym(A, B) :- \+(A = B), hasCommonName(B, A).                    %A is a common name of scientific name B
synonym(A, B) :- \+(A = B), hasCommonName(A, B).
synonym(A, B) :- \+(A = B), hasCommonName(C, A), hasCommonName(C, B).

%countSpecies(A, N) :- \+order(A), \+family(A), \+genus(A), \+hasCompoundName(G, S, A), N is 0.
countSpecies(A, N) :- hasCompoundName(_, S, A), species(S), N = 1.
countSpecies(A, N) :- N = 0.
%countSpecies(A, N) :- isaStrict(X, A), species(X), N is N+1, countSpecies(A, N).

%rangesTo(A, P).

%habitat(A, B).

%food(A, B).

%nesting(A, B).
          
%behavior(A, B).

%conservation(A, B).
