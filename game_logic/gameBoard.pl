:- module(gameBoard, [neighbor_n/2, neighbor_s/2, neighbor_ne/2, neighbor_se/2,
                      neighbor_nw/2, neighbor_sw/2, get_neighbor_cell/2,
                      get_neighborhood/2, valid_cell/2,
                      connected_to_hive_without_origin_cell/2, hamiltonian_path/1,
                      is_surrounded/1, connected_to_hive/1]).

:- [cell].
%                         N
%                      NO   NE
%                         C
%                      SO   SE
%                         S

%devuelve el vecino al norte de la celda dada
neighbor_n(cell(_,_,_,Row,Column),Ncell) :-
    Nrow is Row - 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Nrow,Column)),
     Ncell = cell(Bug,Color,PiledUp,Nrow,Column),!;
     Ncell = cell(none,none,0,Nrow,Column)
     ).
%devuelve el vecino al sur de la celda declarada
neighbor_s(cell(_,_,_,Row,Column),Ncell) :-
    Nrow is Row + 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Nrow,Column)),
     Ncell = cell(Bug,Color,PiledUp,Nrow,Column),!;
     Ncell = cell(none,none,0,Nrow,Column)
     ).
%devuelve el vecino al noreste de la celda declarada
neighbor_ne(cell(_,_,_,Row,Column),Ncell) :-
    Nrow is Row - 1,
    Ncolumn is Column + 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Nrow,Ncolumn)),
     Ncell = cell(Bug,Color,PiledUp,Nrow,Ncolumn),!;
     Ncell = cell(none,none,0,Nrow,Ncolumn)
     ).
%devuelve el vecino al sureste de la celda declarada
neighbor_se(cell(_,_,_,Row,Column),Ncell) :-
    Ncolumn is Column + 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Row,Ncolumn)),
     Ncell = cell(Bug,Color,PiledUp,Row,Ncolumn),!;
     Ncell = cell(none,none,0,Row,Column)
     ).
%devuelve el vecino al noroeste de la celda declarada
neighbor_nw(cell(_,_,_,Row,Column),Ncell) :-
    Ncolumn is Column - 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Row,Ncolumn)),
     Ncell = cell(Bug,Color,PiledUp,Row,Ncolumn),!;
     Ncell = cell(none,none,0,Row,Ncolumn)
     ).
%devuelve el vecino al suroeste de la celda declarada
neighbor_sw(cell(_,_,_,Row,Column),Ncell) :-
    Nrow is Row + 1,
    Ncolumn is Column - 1,
    PiledUp is 0,
    (
     cell:get_cell(cell(Bug,Color,PiledUp,Nrow,Ncolumn)),
     Ncell = cell(Bug,Color,PiledUp,Nrow,Ncolumn),!;
     Ncell = cell(none,none,0,Nrow,Ncolumn)
     ).
%devuelve un vecino de la celda
get_neighbor_cell(C,Ncell) :-
    neighbor_n(C,Ncell);
    neighbor_s(C,Ncell);
    neighbor_ne(C,Ncell);
    neighbor_se(C,Ncell);
    neighbor_nw(C,Ncell);
    neighbor_sw(C,Ncell).
%devuelve los seis vecinos de la celda
get_neighborhood(C, Ncells) :- findall(Ncell, get_neighbor_cell(C, Ncell), Ncells).

is_surrounded(cell(Bug,Color,PiledUp,Row,Column)) :-
    get_neighborhood(cell(Bug,Color,PiledUp,Row,Column), NCells),
    \+member(cell(none,none,_,_,_),NCells).

neighbors_non_visited(Cell,Visited,Ncell):-
    get_neighbor_cell(Cell,Ncell),
    \+empty_cell(Ncell),
    \+member(Ncell,Visited).

dfs([],Visited,Visited) :- !.
dfs([FCell|Cells],Visited,Path) :-
    !,
    dfs(FCell,Visited,NVisited),
    dfs(Cells,[FCell|NVisited],Path).
dfs(Cell,Visited,Path) :-
    findall(Ncell, neighbors_non_visited(Cell,Visited,Ncell), Ncells),
    %append(NCells, Visited, NVisited),
    dfs(Ncells,Visited,Path).

hamiltonian_path(cell(Bug,Color,PiledUp,Row,Column)) :-
    remove_cell(Bug,Color,PiledUp,Row,Column),
    get_cells_list([FCell|Cells]),
    dfs(FCell,[FCell],Path),
    add_cell(Bug,Color,PiledUp,Row,Column),
    length([FCell|Cells],L),
    length(Path,L).

valid_cell(Color,Cell) :-
    get_cell(cell(Bug,Color,PiledUp,Row,Column)),
    PiledUp = 0,
    get_neighbor_cell(cell(Bug,Color,PiledUp,Row,Column),Cell),
    empty_cell(Cell),
    get_neighborhood(Cell,NCells),
    same_color_neighbor(Color,NCells).

same_color_neighbor(Color,[]) :- Color.
same_color_neighbor(Color,[cell(_,C,PiledUp,Row,Column)|Cells]) :-
    (
     (
      PiledUp > 0,
      get_cell(cell(_,PC,0,Row,Column)),!,
      PC is Color
     );
     C is Color;
     C is none
    ),
    same_color_neighbor(Color,Cells).

connected_to_hive(Cell) :-
    get_cells_list(C),
    get_neighbor_cell(Cell,NCell),
    member(NCell,C).

connected_to_hive_without_origin_cell(OCell,Cell):-
    get_cells_list(C),
    get_neighbor_cell(Cell,NCell),
    delete(C,OCell,NC),
    member(NCell,NC).
