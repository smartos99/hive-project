:- [cell].
:- [gameBoard].
:- [deck].
:- [instructions].

bugs(B) :- B = [ant, grasshopper, beetle, spider, queenbee, mosquito, ladybug, pillbug].
%borrador
initiate_PvsP :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,0,1,1,1).
initiate_PvsIA :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,1,1,1,1).

%el jugador selcciona una ficha a mover
piece_selection(Color) :-
    repeat,
    write("\nEnter Piece Location in form 'd_bug.' (where d is for deck and bug is de name of the bug selected) or 'b_row_column. (where b is for board and row and column is the coordenates of the piece in the board.)' \n"),
    read(Input),
    string_lower(Input,Processed),
    write(Processed),
    split_string(Processed, "_", "", L),
    write(L),
    parse_input(Color,L).

parse_input(Color,[T|L]) :-
    string_chars(T,[Choice]),
    (
     (
      Choice = d,
      write(L),
      deck_piece(Color,L)
     );
     (
      Choice = b,
      write(L),
      board_piece(Color,L)
     )
    ).

% comprobar que sea un bicho valido y q haya al menos uno en la mano y
% retornar movimientos validos.
deck_piece(Color, [Bug]) :-
   bugs(B),
   write(Bug),
   atom_string(SBug,Bug),
   member(SBug, B),
   write(B),
   write(SBug),
   get_bug_count(Color,SBug,Count),
   write(Count),
   Count > 0,
   \+is_piled_up(queenbee,Color,_,_).

testing(P) :-
        initiate_PvsP,
        %get_bug_count(black,mosquito,Count),
        %write(Count),
        %add_cell(queenbee,white,1,3,2),
        %get_cell(cell(queenbee,white,PiledUp,Row,Column)),
        %piece_selection(black),
        %instructions(Game),
        add_cell(ladybug,white,0,5,5),
	add_cell(ant,white,0,4,5),
        add_cell(ant,white,0,4,6),
        add_cell(ant,white,0,5,4),
        %get_neighborhood(cell(ant,white,0,1,1),Ncells),
	%get_cells_list(P),
        %hamiltonian_path(cell(ladybug,white,0,5,5)),
        cell(ant,white,0,5,4) \== cell(ant,white,0,4,6).

