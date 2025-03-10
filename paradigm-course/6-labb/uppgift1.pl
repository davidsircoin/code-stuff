% In the binomial nomenclature for species names, "latin name", the
% first part is the genus and the second part, the specific name,
% identifies the species within that genus. That means that humans
% belong to the genus "Homo" and the species is "Homo sapiens".

% The predicate latin/3 names the common name, the genus, and the
% specific name.
latin("African elephant", "Loxodonta", "africana").
latin("Asian elephant", "Elephas", "maximus").
latin("mammoth", "Mammuthus", "primigenius").
latin("dog", "Canis", "lupus familiaris").
latin("cow", "Bos", "taurus").
latin("horse", "Equus", "ferus caballus").
latin("zebra", "Equus", "quagga").
latin("giraffe", "Giraffa", "giraffa").
latin("camel", "Camelus", "bactrianus").
latin("llama", "Lama", "glama").
latin("rhinoceros", "Diceros", "bicornis").
latin("chimpanzee", "Pan", "troglodytes").
latin("orangutan", "Pongo", "pygmeus").
latin("gorilla", "Gorilla", "gorilla").
latin("human", "Homo", "sapiens").
latin("neanderthal", "Homo", "neanderthalensis").
latin("hobbit", "Homo", "floresiensis").

% Taxonomic order. part_of/2 encodes taxonomic relationships:
% part_of(A, B) means that A is a part of B.
part_of("Elephas", "Elephantina").
part_of("Elephantina", "Elephantidae").
part_of("Mammoth", "Elephantina").
part_of("Loxodonta", "Elephantidae").
part_of("Canis", "Carnivora").
part_of("Carnivora", "Mammalia").
part_of("Bos", "Artiodactyla").
part_of("Artiodactyla", "Mammalia").
part_of("Equus", "Perissodactyla").
part_of("Perissodactyla", "Mammalia").
part_of("Sus", "Suidae").
part_of("Suidae", "Artiodactyla").
part_of("Camelus", "Camelidae").
part_of("Lama", "Camelidae").
part_of("Camelidae", "Artiodactyla").
part_of("Diceros", "Perissodactyla").
part_of("Pan", "Hominini").
part_of("Homo", "Hominini").
part_of("Gorilla", "Hominiae").
part_of("Hominiae", "Hominidae").
part_of("Hominini", "Hominidae").
part_of("Pongo", "Hominidae").
part_of("Hominidae", "Primates").
part_of("Primates", "Mammalia").


is_in_genus(X,Y) :- latin(X,Y,_).

all_in_genus(X,Y) :- findall(Z,is_in_genus(Z,X),Y).


ancestor_rec(X,Y) :- part_of(X,Y).
ancestor_rec(X,Y) :- part_of(X,Z), ancestor_rec(Z,Y).

ancestor(X,Y) :- is_in_genus(X,Y).
ancestor(X,Y) :- is_in_genus(X,A), ancestor_rec(A,Y).
ancestor(X,Y) :- ancestor_rec(X,Y).

common_ancestor(X,Y,Z) :- ancestor(X,Z), ancestor(Y,Z).




