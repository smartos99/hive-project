:- module(cell, [add_cell/5, remove_cell/5, is_piled_up/4,
                 piledUp_update_leave/2, piledUp_update_arrival/2,get_cell/1,
                 queen_on_field/1,get_cells_list/1,get_top_cells_list/1,
                 is_mosquito/1,empty_cell/1,
                 top_of_pile/1, cleanup_cells/0]).

:- dynamic cell/5.
% La estructura de celda: tipo de bicho, color del jugador, la posicion
% en la que esta en caso de estar en una pila, sino es 0, fila, columna

%define una celda en la base de datos
add_cell(Bug,Color,PiledUp,Row,Column) :-  assertz(cell(Bug,Color,PiledUp,Row,Column)).
%elimina la definicion de una celda en la base de datos
remove_cell(Bug,Color,PiledUp,Row,Column) :- retract(cell(Bug,Color,PiledUp,Row,Column)).
cleanup_cells :- retractall(cell(_,_,_,_,_)).
%devuelve una celda
get_cell(cell(Bug,Color,PiledUp,Row,Column)) :- cell(Bug,Color,PiledUp,Row,Column).
%devuelve la lista de celdas
get_cells_list(C) :- findall(Cell, get_cell(Cell), C).
get_top_cells_list(C) :- findall(Cell, get_top_cell(Cell), C).

get_top_cell(cell(Bug,Color,PiledUp,Row,Column)) :- cell(Bug,Color,0,Row,Column), PiledUp is 0.

is_piled_up(Bug, Color, Row, Column) :-
    get_cell(cell(Bug,Color,PiledUp,Row,Column)),
    PiledUp > 0.

is_mosquito(cell(Bug,_,_,_,_)) :- Bug = mosquito.

queen_on_field(Color) :-
    get_cell(cell(queenbee,Color,_,_,_)).

empty_cell(cell(Bug,_,_,_,_)) :- Bug = none.

top_of_pile(cell(_,_,P,R,C)) :-
    P < 1,
    get_cell(cell(_,_,1,R,C)).

decrease_piledUp([cell(B,Co,P,R,C)|Cells]) :-
    NP is P - 1,
    remove_cell(B,Co,P,R,C),
    add_cell(B,Co,NP,R,C),
    decrease_piledUp(Cells).
decrease_piledUp([cell(B,Co,P,R,C)|Cells]) :-
    NP is P - 1,
    remove_cell(B,Co,P,R,C),
    add_cell(B,Co,NP,R,C).
decrease_piledUp([]) :- !.


increase_piledUp([cell(B,Co,P,R,C)|Cells]) :-
    NP is P + 1,
    remove_cell(B,Co,P,R,C),
    add_cell(B,Co,NP,R,C),
    increase_piledUp(Cells).
increase_piledUp([cell(B,Co,P,R,C)]) :-
    NP is P + 1,
    remove_cell(B,Co,P,R,C),
    add_cell(B,Co,NP,R,C).
increase_piledUp([]) :- !.

piledUp_update_leave(RO,CO) :-
    findall(PCell,piledUp_cell(RO,CO,PCell),PCells),
    decrease_piledUp(PCells).

piledUp_update_arrival(RD,CD) :-
    findall(PCell,piledUp_cell(RD,CD,PCell),PCells),
    increase_piledUp(PCells).

piledUp_cell(RD,CD,PCell) :-
    get_cell(cell(Bug,Color,PiledUp,RD,CD)),
    \+empty_cell(cell(Bug,Color,PiledUp,RD,CD)),
    PCell = cell(Bug,Color,PiledUp,RD,CD).













