:- module(main,[start/0,play_game/2]).

:- [cell].
:- [gameBoard].
:- [deck].
:- [instructions].
:- [moves].

bugs(B) :- B = [ant, grasshopper, beetle, spider, queenbee, mosquito, ladybug, pillbug].
%borrador
initiate_PvsP :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,1,1,1,1).
initiate_IAvsIA :- add_deck(white,3,3,2,2,1,1,1,1), add_deck(black,3,3,2,2,1,1,1,1).

select_first_piece(white, Position, Possibilities) :-
    repeat,
    write("\nEnter first piece to locate in the board 'bug.' (where bug is the name of the bug selected))' \n"),
    read(Input),
    string_lower(Input,Bug),
    write(Bug),
    bugs(B),
    atom_string(SBug,Bug),
    member(SBug, B),
    Position = [SBug],
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
    Position = [SBug],
    Possibilities = [(14,15),(16,15),(14,16),(15,16),(15,14),(16,14)].

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
      write("\n You have quit the game, let's play again next time (^.^)o[~]\n")
     ,halt
     )
    ).

deck_piece(Color, [Bug], Position, Possibilities) :-
    bugs(B),
    atom_string(SBug,Bug),
    member(SBug, B),
    get_bug_count(Color,SBug,Count),
    Count > 0,
    \+is_piled_up(queenbee,Color,_,_),
    Position = [SBug],
    initial_positions(Color,Possibilities).
%forall cell Color, piece_helper cell, Possibilities = [] => next player

board_piece(Color, [Row,Column],Position,Possibilities) :-
    queen_on_field(Color),
    atom_number(Row,R),
    atom_number(Column,C),
    get_cell(cell(Bug,Color,0,R,C)),
    \+is_stunned(cell(Bug,Color,0,R,C)),
    (
       Bug = pillbug,
       Position = [pillbug,R,C];
       Bug \== pillbug,
       Position = [R,C]
    ),
    (  \+top_of_pile(cell(Bug,Color,0,R,C)),
       hamiltonian_path(cell(Bug,Color,0,R,C));
       top_of_pile(cell(Bug,Color,0,R,C))
    )
    ,
    piece_helper(cell(Bug,Color,0,R,C),Possibilities).

move_piece(Color,[pillbug,RO,CO],[Possibilities,Bugs,Moves]) :-
    (
     Bugs = [];
     Moves = []
    ),
    Position = [RO,CO],
    move_piece(Color,Position,Possibilities).

move_piece(Color,[pillbug,RO,CO],[Possibilities,Bugs,Moves]) :-
    \+Bugs = [],
    \+Moves = [],
    repeat,
    write("\nSelect Pillbug Action in form 'm.' for moving in board or 'b_row_column.' for move the bug located en (row,column) to another space.  \n"),
    read(Input),
    string_lower(Input,Processed),
    write(Processed),
    split_string(Processed, "_", "", [S,Row,Column]),
    write(S),
    string_chars(S,[Choice]),
    atom_number(Row,RB),
    atom_number(Column,CB),
    (
     (
      Choice = m,
      Position = [RO,CO],
      move_piece(ColorB,Position,Possibilities)
     );
     (
      Choice = b,
      member(cell(Bug,ColorB,0,RB,CB),Bugs),
      bug_moved_by_pillbug(cell(Bug,ColorB,0,RB,CB),Moves)
     )).

move_piece(Color,[RO,CO],Possibilities) :-
    repeat,
    write("\nEnter Movement in form 'row.' (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(RD),
    write("\nEnter Movement in form 'column'. (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(CD),
    member((RD,CD),Possibilities),
    get_cell(cell(Bug,Color,0,RO,CO)),
    remove_cell(Bug,Color,0,RO,CO),
    piledUp_update_leave(RO,CO),
    piledUp_update_arrival(RD,CD),
    add_cell(Bug,Color,0,RD,CD),
    update_last_cell(cell(Bug,Color,0,RD,CD),Color).

move_piece(Color,[Bug],Possibilities) :-
    repeat,
    write("\nEnter Movement in form 'row.' (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(R),
    write("\nEnter Movement in form 'column'. (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(C),
    member((R,C),Possibilities),
    add_cell(Bug,Color,0,R,C),
    reduce_bug_count(Color,Bug).

bug_moved_by_pillbug(cell(Bug,Color,P,RB,CB),Moves):-
    repeat,
    write("\nEnter Movement in form 'row.' (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(RD),
    write("\nEnter Movement in form 'column'. (where row and column is the
          coordenates of the piece in the board.)' \n"),
    read(CD),
    member((RD,CD),Moves),
    remove_cell(Bug,Color,P,RB,CB),
    add_cell(Bug,Color,P,RD,CD),
    stunne_cell(cell(Bug,Color,P,RD,CD),2).


%  asi q en caso de
% no tener ficha q jugar, es el turno de la otra persona.

play_game(white,1) :-
    write("\nIT IS WHITE'S TURN"),
    select_first_piece(white,Position,Possibilities),
    move_piece(white,Position,Possibilities),
    play_game(black,1).

play_game(black,1) :-
    write("\nIT IS BLACK'S TURN"),
    select_first_piece(black,Position,Possibilities),
    move_piece(black,Position,Possibilities),
    play_game(white,2).

play_game(white,4) :-
    write("\nIT IS WHITE'S TURN"),
    (
     (
      \+queen_on_field(white),
      repeat,
      write("\nEnter 'queenbee.' to place the Queen Bee on the board cause it
      is the 4th turn)' \n"),
      read(Input),
      string_lower(Input,Bug),
      write(Bug),
      atom_string(SBug,Bug),
      SBug = queenbee,
      Position = [SBug],
      initial_positions(white,Possibilities)
     );
    (
      queen_on_field(white),
      repeat,
      piece_selection(white,Position,Possibilities)
     )
    ),
    move_piece(white,Position,Possibilities),
    update_stunned_pieces,
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
      Position = [SBug],
      initial_positions(black,Possibilities)
     );
    (
      queen_on_field(black),
      repeat,
      piece_selection(black,Position,Possibilities)
     )
    ),
    move_piece(black,Position,Possibilities),
    update_stunned_pieces,
    play_game(white,5).

play_game(white,Turn) :-
    Turn > 4,
    get_cell(cell(queenbee,white,P,R,C)),
    is_surrounded(cell(queenbee,white,P,R,C)),
    write("\nBLACK WINS!").

play_game(black,Turn) :-
    Turn > 4,
    get_cell(cell(queenbee,black,P,R,C)),
    is_surrounded(cell(queenbee,black,P,R,C)),
    write("\nWHITE WINS!").

play_game(white,Turn) :-
    write("\nIT IS WHITE'S TURN"),
    repeat,
    piece_selection(white,Position,Possibilities),
    move_piece(white,Position,Possibilities),
    update_stunned_pieces,
    play_game(black,Turn).

play_game(black,Turn) :-
    write("\nIT IS BLACK'S TURN"),
    repeat,
    piece_selection(black,Position,Possibilities),
    move_piece(black,Position,Possibilities),
    NTurn is Turn + 1,
    update_stunned_pieces,
    play_game(white,NTurn).

start :- instructions(Game), begin.
begin :- initiate_PvsP,play_game(white,1).



