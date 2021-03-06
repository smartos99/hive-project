:- module(deck, [add_deck/9,remove_deck/9,get_deck/1,get_decks/1,get_bug_count/3,reduce_bug_count/2, cleanup_decks/0]).

:- dynamic deck/9.
%la estructura deck que no es mas que la mano del jugador y su color
%esta compuesta por la cantidad de bichos de cada tipo de bicho.

%define la mano de un jugador de la base de datos
add_deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug) :-  assertz(deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug)).
%elimina la mano de un jugador de la base de datos
remove_deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug) :- retract(deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug)).
cleanup_decks :- retractall(deck(_,_,_,_,_,_,_,_,_)).
%devuelve la mano de un jugador
get_deck(deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug)) :- deck(Color, Ant, Grasshopper, Beetle, Spider, QueenBee, Mosquito, LadyBug, PillBug).
%devuelve las manos de los jugadores
get_decks(Decks) :- findall(Deck, get_deck(Deck), Decks).

%devuelve la cantidad de cada bicho en mano
get_bug_count(Color,ant,Count) :-
    get_deck(deck(Color,Ant,_,_,_,_,_,_,_)),!,
    Count is Ant.
get_bug_count(Color,grasshopper,Count) :-
    get_deck(deck(Color,_,Grasshopper,_,_,_,_,_,_)),!,
    Count is Grasshopper.
get_bug_count(Color,beetle,Count) :-
    get_deck(deck(Color,_,_,Beetle,_,_,_,_,_)),!,
    Count is Beetle.
get_bug_count(Color,spider,Count) :-
    get_deck(deck(Color,_,_,_,Spider,_,_,_,_)),!,
    Count is Spider.
get_bug_count(Color,queenbee,Count) :-
    get_deck(deck(Color,_,_,_,_,Queenbee,_,_,_)),!,
    Count is Queenbee.
get_bug_count(Color,mosquito,Count) :-
    get_deck(deck(Color,_,_,_,_,_,Mosquito,_,_)),!,
    Count is Mosquito.
get_bug_count(Color,ladybug,Count) :-
    get_deck(deck(Color,_,_,_,_,_,_,LadyBug,_)),!,
    Count is LadyBug.
get_bug_count(Color,pillbug,Count) :-
    get_deck(deck(Color,_,_,_,_,_,_,_,PillBug)),!,
    Count is PillBug.

reduce_bug_count(Color,ant) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    A is Ant - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,A,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug).
reduce_bug_count(Color,grasshopper) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    G is Grasshopper - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,G,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug).
reduce_bug_count(Color,beetle) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    B is Beetle - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,B,Spider,QueenBee,Mosquito,LadyBug,PillBug).
reduce_bug_count(Color,spider) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    S is Spider - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,Beetle,S,QueenBee,Mosquito,LadyBug,PillBug).
reduce_bug_count(Color,queenbee) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    Q is QueenBee - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,Beetle,Spider,Q,Mosquito,LadyBug,PillBug).
reduce_bug_count(Color,mosquito) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    M is Mosquito - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,M,LadyBug,PillBug).
reduce_bug_count(Color,ladybug) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    L is LadyBug - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,L,PillBug).
reduce_bug_count(Color,pillbug) :-
    get_deck(deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug)),!,
    P is PillBug - 1,
    remove_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,PillBug),
    add_deck(Color,Ant,Grasshopper,Beetle,Spider,QueenBee,Mosquito,LadyBug,P).
