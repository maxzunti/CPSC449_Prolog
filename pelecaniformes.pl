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
family(threskiornthdae).

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
species(thulaegretta).
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
hasParent(thulaegretta, egretta).
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


hasCommonName(pelecanus, pelican).
hasCommonName(pelecanusErythrorhynchos, americanWhitePelican).
hasCommonName(pelecanusOccidentalis, brownPelican).
hasCommonName(erythrorhynchos, americanWhitePelican).
hasCommonName(occidentalis, brownPelican).
hasCommonName(botaurus, bittern).
hasCommonName(botaurusLentiginosus, americanBittern).
hasCommonName(lentiginosus, americanBittern).
hasCommonName(ixobrychus, bittern).
hasCommonName(ixobrychusExilis, leastBittern).
hasCommonName(exilis, leastBittern).
hasCommonName(ardea, heron).
hasCommonName(ardeaHerodias, greatBlueHeron).
hasCommonName(ardeaAlba, greatEgret).
hasCommonName(herodias, greatBlueHeron).
hasCommonName(alba, greatEgret).
hasCommonName(egretta, heron).
hasCommonName(egretta, egret).
hasCommonName(egrettaThulaegretta, snowyEgret).
hasCommonName(egrettaCaerulea, littleBlueHeron).
hasCommonName(egrettaTricolor, tricoloredHeron).
hasCommonName(egrettaRufescens, reddishEgret).
hasCommonName(thulaegretta, snowyEgret).
hasCommonName(caerulea, littleBlueHeron).
hasCommonName(tricolor, tricoloredHeron).
hasCommonName(rufescens, reddishEgret).
hasCommonName(bubulcus, egret).
hasCommonName(bubulcusIbis, cattleEgret).
hasCommonName(ibis, cattleEgret).
hasCommonName(butorides, heron).
hasCommonName(butoridesVirescens, greenHeron).
hasCommonName(virescens, greenHeron).
hasCommonName(nycticorax, nightHeron).
hasCommonName(nycticoraxNycticorax, blackCrownedNightHeron).
hasCommonName(nycticorax, blackCrownedNightHeron).
hasCommonName(nyctanassa, nightHeron).
hasCommonName(nyctanassaViolacea, yellowCrownedNightHeron).
hasCommonName(violacea, yellowCrownedNightHeron).
hasCommonName(eudocimus, ibis).
hasCommonName(eudocimusAlbus, whiteIbis).
hasCommonName(albus, whiteIbis).
hasCommonName(plegadis, ibis).
hasCommonName(plegadisFalcinellus, glossyIbis).
hasCommonName(plegadisChihi, whiteFacedIbis).
hasCommonName(falcinellus, glossyIbis).
hasCommonName(chihi, whiteFacedIbis).
hasCommonName(platalea, spoonbill).
hasCommonName(plataleaAjaja, roeateSpoonbill).
hasCommonName(ajaja, roeateSpoonbill).

hasCommonName(pelecanus, erythrorhynchos, americanWhitePelican).
hasCommonName(pelecanus, occidentalis, brownPelican).
hasCommonName(botaurus, lentiginosus, americanBittern).
hasCommonName(ixobrychus, exilis, leastBittern).
hasCommonName(ardea, herodias, greatBlueHeron).
hasCommonName(ardea, alba, greatEgret).
hasCommonName(egretta, thulaegretta, snowyEgret).
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
hasCommonName(platalea, ajaja, roeateSpoonbill).

hasCompoundName(G, S, N) :- hasCommonName(G, S, X), hasCommonName(N, X), \+(G = N), \+(S = N).

hasSciName(C, N) :- hasCommonName(N, C), hasCompoundName(X, Y, N).
hasSciName(C, N) :- hasCommonName(N, C), genus(N); family(N); order(N).

isa(A, B) :- hasParent(A, B).
isa(A, B) :- hasParent(A, C), hasParent(C, B).
isa(A, B) :- hasParent(A, C), hasParent(C, D), hasParent(D, B).
isa(A, A).

isaStrict(A, B) :- hasParent(A, B).
isaStrict(A, B) :- hasParent(A, C), hasParent(C, B).
isaStrict(A, B) :- hasParent(A, C), hasParent(C, D), hasParent(D, B).
isaStrict(A, A).

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
