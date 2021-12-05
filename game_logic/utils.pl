:- [cell].
:- [gameBoard].

cycle(X) :-
    cycle(X, []).

cycle(Curr, Visited) :-
    member(Curr, Visited),!.

cycle(Curr,Visited) :-
    edge(Curr,Next),
    cycle(Next, [Curr|Visited]).

path(A,Z, Graph,Path) :-
    path1(A,[Z], Graph,Path).

path1(A,[Y|Path1],Graph,Path) :-
    get_neighbor_cell(Y,Ncell),
    \+ member(Ncell,Path1),
    path1(A,[Ncell,Y|Path1],Graph,Path).

hamiltonian(Graph,Path) :-
    path(cell(Bo,Coo,Po,Ro,Co),cell(Bd,Cod,Pd,Rd,Cd),Graph,Path),
    covers(Path,Graph).

covers(Path,Graph) :-
    \+ (member(get_cell(Bug,Color,PiledUp,Row,Column),Graph), \+ member(get_cell(Bug,Color,PiledUp,Row,Column),Path)).

solve(N,[N]):-
    goal(N).

solve(N,[N1|Sol]):-
    s(N, N1),
    solve(N1,Sol1).

solve(Node, Solution) :-
    depthfirst([],Node,Solution).

depthfirst(Path,Node,[Node|Path]) :-
    goal(Node).

depthfirst(Path, Node, Sol):-
    s(Node,Node1),
    \+ member(Node1,Path),
    depthfirst([Node|Path],Node1,Sol).

hamiltonian_graph([]):-
    .

