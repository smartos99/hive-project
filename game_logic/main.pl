:- [cell].
:- [gameBoard].
:- [deck].
:- [instructions].
:- [moves].

bugs(B) :- B = [ant, grasshopper, beetle, spider, queenbee, mosquito, ladybug, pillbug].
%borrador
initiate_PvsP :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,1,1,1,1).
initiate_PvsIA :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,1,1,1,1).

select_first_piece(white, Position, Possibilities) :-
    repeat,
    write("\nEnter first piece to locate in the board 'bug.' (where bug is the name of the bug selected))' \n"),
    read(Input),
    string_lower(Input,Bug),
    write(Bug),
    bugs(B),
    atom_string(SBug,Bug),
    member(SBug, B),
    Position is SBug,
    Possibilities = [(15,15)].

select_first_piece(black, Position, Possibilities) :-
    repeat,
    write("\nEnter first piece to locate in the board 'bug.' (where bug is the name of the bug selected))' \n"),
    read(Input),
    string_lower(Input,Bug),
    write(Bug),
    bugs(B),
    atom_string(SBug,Bug),
    member(SBug, B),
    Position is SBug,
    Possibilities = [(14,15),(14,16),(15,14),(15,16),(16,14),(15,14)].

%el jugador selcciona una ficha a mover
piece_selection(Color, Position, Possibilities) :-
    repeat,
    write("\nEnter Piece Location in form 'd_bug.' (where d is for deck and bug is de name of the bug selected) or 'b_row_column. (where b is for board and row and column is the coordenates of the piece in the board. You can quit the game by typing 'q.')' \n"),
    read(Input),
    string_lower(Input,Processed),
    write(Processed),
    split_string(Processed, "_", "", L),
    write(L),
    parse_input(Color,L,Position,Possibilities).

parse_input(Color,[T|L],Position,Possibilities) :-
    string_chars(T,[Choice]),
    (
     (
      Choice = d,
      write(L),
      deck_piece(Color,L,Position,Possibilities)
     );
     (
      Choice = b,
      write(L),
      board_piece(Color,L,Position,Possibilities)
     );
     (
      Choice = q,
      write("\n"),
      write("\n You have quit the game, let's play again next time (^.^)o[~] \n")
     ,halt
     )
    ).

% comprobar que sea un bicho valido y q haya al menos uno en la mano y
% retornar movimientos validos.
deck_piece(Color, [Bug], Position, Possibilities) :-
    bugs(B),
    atom_string(SBug,Bug),
    member(SBug, B),
    get_bug_count(Color,SBug,Count),
    Count > 0,
    \+is_piled_up(queenbee,Color,_,_),
    Position is SBug,
    initial_positions(Color,Possibilities).

%retornar movimientos validos.
board_piece(Color, [Row,Column],Position,Possibilities) :-
    queen_on_field(Color),
    atom_number(Row,R),
    atom_number(Column,C),
    get_cell(cell(Bug,Color,PiledUp,R,C)),
    Position is (R,C),
    (
     (
      PiledUp > 0,
      Possibilities = []
      );
     (
      PiledUp < 1,
      hamiltonian_path(cell(Bug,Color,PiledUp,R,C)),
      piece_helper(cell(Bug,Color,PiledUp,R,C),Possibilities)
     )
    ).

% agregar un turno para cada jugador para comprobar lo de la reina q no
% haya sido puesta y es el cuarto turno. Comprobar tambienq si la reina
% esta apilada no puedes poner una ficha desde tu mano, asi q en caso de
% no tener ficha q jugar, es el turno de la otra persona.
play_game(white,Turn) :-
    is_surrounded(cell(queenbee,white,_,_,_)),
    write("\nBLACK WINS!").

play_game(black,Turn) :-
    is_surrounded(cell(queenbee,black,_,_,_)),
    write("\nWHITE WINS!").

% primer movimiento de las blancas, lo pone en el centro cualquier ficha
% seleccionada.
play_game(white,1) :-
    write("\nIT IS WHITE'S TURN"),
    select_first_piece(white,Position,Possibilities),
    move_piece(white,Position,Possibilities),
    play_game(black,1).

play_game(black,1) :-
    write("\nIT IS WHITE'S TURN"),
    select_first_piece(black,Position,Possibilities),
    move_piece(black,Position,Possibilities),
    play_game(white,2).

play_game(white,4) :-
    write("\nIT IS WHITE'S TURN"),
    (
     (
      \+queen_on_field(white),
      repeat,
      write("\nEnter 'queenbee.' to place the Queen Beeon the board cause it
      is the 4th turn)' \n"),
      read(Input),
      string_lower(Input,Bug),
      write(Bug),
      atom_string(SBug,Bug),
      SBug = queenbee,
      Position is SBug,
      initial_positions(white,Possibilities)
     );
    (
      queen_on_field(white),
      repeat,
      piece_selection(white,Position,Possibilities)
     )
    ),
    move_piece(white,Position,Possibilities),
    play_game(black,4).

play_game(black,4) :-
    write("\nIT IS BLACK'S TURN"),
    (
     (
      \+queen_on_field(black),
      repeat,
      write("\nEnter 'queenbee.' to place the Queen Beeon the board cause it
      is the 4th turn)' \n"),
      read(Input),
      string_lower(Input,Bug),
      write(Bug),
      atom_string(SBug,Bug),
      SBug = queenbee,
      Position is SBug,
      initial_positions(black,Possibilities)
     );
    (
      queen_on_field(black),
      repeat,
      piece_selection(black,Position,Possibilities)
     )
    ),
    move_piece(black,Position,Possibilities),
    play_game(white,5).

play_game(white,Turn) :-
    write("\nIT IS WHITE'S TURN"),
    repeat,
    piece_selection(white,Position,Possibilities),
    move_piece(white,Position,Possibilities),
    play_game(black,Turn).

play_game(black,Turn) :-
    write("\nIT IS BLACK'S TURN"),
    repeat,
    piece_selection(black,Position,Possibilities),
    move_piece(black,Position,Possibilities),
    NTurn is Turn + 1,
    play_game(white,NTurn).

start :- instructions(Game), begin.
begin :- initiate_PvsP,play_game(white,1).

%ver lo de piled up en ambos casos
move_piece(Color,Bug,Possibilities) :-
    %lenght(Possiilities,L),
    %L > 0,
    repeat,
    write("\nEnter Movement in form 'row_column. (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(Input),
    string_lower(Input,Processed),
    split_string(Processed, "_", "", [Row,Column]),
    atom_number(Row,R),
    atom_number(Column,C),
    member((R,C),Possibilities),
    add_cell(Bug,Color,0,R,C),
    reduce_bug_count(Color,Bug).

move_piece(Color,(RO,CO),Possibilities) :-
    %lenght(Possiilities,L),
    %L > 0,
    repeat,
    write("\nEnter Movement in form 'row_column. (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(Input),
    string_lower(Input,Processed),
    split_string(Processed, "_", "", [Row,Column]),
    atom_number(Row,RD),
    atom_number(Column,CD),
    member((RD,CD),Possibilities),
    get_cell(cell(Bug,Color,PiledUp,RO,CO)),
    remove_cell(Bug,Color,PiledUp,RO,CO),
    piledUp_update_leave(RO,CO),
    piledUp_update_arrival(RD,CD),
    add_cell(Bug,Color,PiledUp,RD,CD).

