:- module(gameBoard, [bridge/3, neighbor_n/2, neighbor_s/2, neighbor_ne/2,
                      neighbor_se/2, neighbor_nw/2, neighbor_sw/2,
                      get_neighbor_cell/2, get_neighborhood/2, valid_cell/2,
                      connected_to_hive_without_origin_cell/2, can_slide/2 ,
                      stunne_cell/2, remove_stunne/2, is_last/2,
                      hamiltonian_path/1, is_stunned/1, last_cell_placed/2,
                      is_surrounded/1, connected_to_hive/1,update_last_cell/2,
                      update_stunned_pieces/0]).

:- dynamic stunned/2.
:- dynamic last/2.

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
     Ncell = cell(none,none,0,Row,Ncolumn)
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

is_surrounded(Cell) :-
    findall(NCell,neighbors_non_empty(Cell,NCell),NCells),
    length(NCells,6).

neighbors_non_empty(Cell,NCell) :-
    get_neighbor_cell(Cell,NCell),
    \+empty_cell(NCell).

neighbors_non_visited(Cell,Visited,Ncell):-
    get_neighbor_cell(Cell,Ncell),
    \+empty_cell(Ncell),
    \+member(Ncell,Visited).

dfs([],Visited,Visited) :- !.
dfs([FCell|Cells],Visited,Path) :-
    !,
    dfs(FCell,[FCell|Visited],NVisited),
    dfs(Cells,NVisited,Path).
dfs(Cell,Visited,Path) :-
    findall(Ncell, neighbors_non_visited(Cell,Visited,Ncell), Ncells),
    %append(NCells, Visited, NVisited),
    dfs(Ncells,Visited,Path).

hamiltonian_path(cell(Bug,Color,PiledUp,Row,Column)) :-
    remove_cell(Bug,Color,PiledUp,Row,Column),
    get_top_cells_list([FCell|Cells]),
    dfs(FCell,[FCell],Path),
    add_cell(Bug,Color,PiledUp,Row,Column),
    forall(member(MCell,[FCell|Cells]), member(MCell,Path)).
    %length([FCell|Cells],L),
    %length(Path,L).

valid_cell(Color,Cell) :-
    get_cell(cell(Bug,Color,PiledUp,Row,Column)),
    PiledUp = 0,
    get_neighbor_cell(cell(Bug,Color,PiledUp,Row,Column),VCell),
    empty_cell(VCell),
    get_neighborhood(VCell,NCells),
    forall(member(NCell,NCells),same_color_neighbor(Color,NCell)),
    Cell = VCell.

same_color_neighbor(Color,cell(_,C,_,_,_)) :-
     C = Color;
     C = none.

connected_to_hive(Cell) :-
    get_cells_list(C),
    get_neighbor_cell(Cell,NCell),
    member(NCell,C),!.

connected_to_hive_without_origin_cell(OCell,Cell):-
    get_cells_list(C),
    get_neighbor_cell(Cell,NCell),
    delete(C,OCell,NC),
    member(NCell,NC),!.

bridge(PCell,MCell,BCell) :-
    get_neighbor_cell(MCell,NCell),
    get_neighbor_cell(PCell,NCell),
    \+empty_cell(NCell),
    top_of_pile(NCell),
    BCell = NCell.

can_slide(OCell,DCell) :-
    get_neighbor_cell(OCell,NCell),
    get_neighbor_cell(DCell,NCell),
    empty_cell(NCell).

is_stunned(Cell) :- stunned(Cell,_).
get_stunned_cell(Cell,Turn) :- stunned(Cell,Turn).
stunne_cell(Cell,Turn) :- assertz(stunned(Cell,Turn)).
remove_stunne(Cell,Turn) :- retract(stunned(Cell,Turn)).
update_stunned_pieces :-
    findall((Cell,Turn),get_stunned_cell(Cell,Turn), Cells),
    update_turn_count(Cells).

update_turn_count([(Cell,Turn)|Cells]) :-
    NTurn is Turn - 1,
    (
      (
          NTurn =:= 0,
          remove_stunne(Cell,Turn)
      );
      (
          NTurn > 0,
          remove_stunne(Cell,Turn),
          stunne_cell(Cell,NTurn)
      )
     ),
    update_turn_count(Cells).

update_turn_count([]) :- !.


is_last(Cell, Color) :- last(Cell,Color).
last_cell_placed(Cell,Color) :- assertz(last(Cell,Color)).
update_last_cell(NCell,Color) :-
    last(Cell,Color),
    retract(last(Cell,Color));
    last_cell_placed(NCell,Color).
