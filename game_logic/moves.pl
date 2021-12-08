:- module(moves, [piece_helper/2, initial_positions/2]).
:- [cell].
:- [gameBoard].

% da las posiciones en el tablero q son posibles para una ficha en la
% mano.
valid_positions([cell(_,_,_,Row,Column)| Cells],InList,Positions) :-
    append([(Row,Column)],InList,NPositions),
    valid_positions(Cells,NPositions,Positions).
valid_positions([cell(_,_,_,Row,Column)],InList,Positions):-
    append([(Row,Column)],InList,Positions).
valid_positions([],Positions,Positions).

initial_positions(Color, Positions) :-
    findall(Cell, valid_cell(Color,Cell), Cells),
    valid_positions(Cells,[],Positions).

return_moves([cell(_,_,_,R,C)|T],InList,Possibilities):-
    append([(R,C)],InList,MyList),
    return_moves(T,MyList,Possibilities).
return_moves([cell(_,_,_,R,C)],InList,Possibilities):-
    append([(R,C)],InList,Possibilities).
return_moves([],Possibilities,Possibilities).

% piece_helper to validate the rules for the pieces
piece_helper(cell(ant,Color,PiledUp,Row,Column),Possibilities) :-
    \+is_surrounded(cell(ant,Color,PiledUp,Row,Column)),
    ant_valid_moves(cell(ant,Color,PiledUp,Row,Column),WCells),
    return_moves(WCells,[],Possibilities).
piece_helper(cell(grasshopper,Color,PiledUp,Row,Column),Possibilities) :-
    findall(JCell, grasshopper_valid_moves(cell(grasshopper,Color,PiledUp,Row,Column),JCell), JCells),
    return_moves(JCells,[],Possibilities).
piece_helper(cell(beetle,Color,PiledUp,Row,Column),Possibilities) :-
    findall(DCell, beetle_valid_moves(cell(beetle,Color,PiledUp,Row,Column),DCell), DCells),
    return_moves(DCells,[], Possibilities).
piece_helper(cell(spider,Color,PiledUp,Row,Column),Possibilities) :-
    \+is_surrounded(cell(spider,Color,PiledUp,Row,Column)),
    findall(DCell, spider_valid_moves(cell(spider,Color,PiledUp,Row,Column),DCell),DCells),
    return_moves(DCells,[],Possibilities).
piece_helper(cell(queenbee,Color,PiledUp,Row,Column),Possibilities) :-
    \+is_surrounded(cell(queenbee,Color,PiledUp,Row,Column)),
    findall(DCell, queenbee_valid_moves(cell(queenbee,Color,PiledUp,Row,Column),DCell), DCells),
    return_moves(DCells,[], Possibilities).
piece_helper(cell(mosquito,Color,PiledUp,Row,Column),Possibilities) :-
    mosquito_valid_moves(cell(mosquito,Color,PiledUp,Row,Column),Possibilities).
piece_helper(cell(ladybug,Color,PiledUp,Row,Column),Possibilities) :-
    findall(DCell, ladybug_valid_moves(cell(ladybug,Color,PiledUp,Row,Column),DCell),DCells),
    return_moves(DCells,[],Possibilities).
piece_helper(cell(pillbug,Color,PiledUp,Row,Column),Possibilities) :-
    \+is_surrounded(cell(pillbug,Color,PiledUp,Row,Column)),
     findall(DCell, pillbug_valid_moves(cell(pillbug,Color,PiledUp,Row,Column),DCell), DCells),
     return_moves(DCells,[], Moves),
     findall(MCell, bug_movable_by_pillbug(cell(pillbug,Color,PiledUp,Row,Column),Color,MCell),MCells),
     findall(VCell, valid_position_to_move_a_bug(cell(pillbug,Color,PiledUp,Row,Column),VCell),VCells),
     return_moves(VCells,[],ECells),
     append([Moves],[MCells],MyList),
     append(MyList,[ECells],Possibilities).

beetle_valid_moves(OCell,DCell):-
    get_neighbor_cell(OCell,NCell),
    empty_cell(NCell),
    (
      (
        top_of_pile(OCell),
        connected_to_hive(NCell)
      );
      (
        \+top_of_pile(OCell),
        can_slide(OCell,NCell),
        connected_to_hive_without_origin_cell(OCell,NCell)
      )
    ),
    DCell = NCell.

beetle_valid_moves(OCell,DCell):-
    get_neighbor_cell(OCell,NCell),
    \+empty_cell(NCell),
    DCell = NCell.

queenbee_valid_moves(OCell,DCell) :-
    get_neighbor_cell(OCell,NCell),
    empty_cell(NCell),
    can_slide(OCell,NCell),
    connected_to_hive_without_origin_cell(OCell,NCell),
    DCell = NCell.

spider_valid_moves(OCell,DCell) :-
    get_neighbor_cell(OCell,Step1),
    empty_cell(Step1),
    can_slide(OCell,Step1),
    connected_to_hive_without_origin_cell(OCell,Step1),
    get_neighbor_cell(Step1,Step2),
    empty_cell(Step2),
    can_slide(Step1,Step2),
    connected_to_hive_without_origin_cell(OCell,Step2),
    Step2 \== OCell,
    get_neighbor_cell(Step2,Step3),
    empty_cell(Step3),
    can_slide(Step2,Step3),
    connected_to_hive_without_origin_cell(OCell,Step3),
    Step3 \== Step1,
    DCell = Step3.

ladybug_valid_moves(OCell,DCell) :-
    get_neighbor_cell(OCell,Step1),
    \+ empty_cell(Step1),
    get_neighbor_cell(Step1,Step2),
    \+ empty_cell(Step2),
    Step2 \== OCell,
    get_neighbor_cell(Step2,Step3),
    empty_cell(Step3),
    DCell = Step3.

mosquito_valid_moves(cell(mosquito,Co,P,R,C),Moves) :-
    top_of_pile(cell(mosquito,Co,P,R,C)),
    piece_helper(cell(beetle,Co,P,R,C),Moves).

mosquito_valid_moves(cell(mosquito,Co,P,R,C),Moves) :-
    \+top_of_pile(cell(mosquito,Co,P,R,C)),
    findall(Cell,valid_neighbor_mosquito(cell(mosquito,Co,P,R,C),Cell),NCells),
    get_mosquito_moves(cell(mosquito,Co,P,R,C),NCells,[],Moves).

valid_neighbor_mosquito(MCell,NCell) :-
    get_neighbor_cell(MCell,Cell),
    \+empty_cell(Cell),
    \+is_mosquito(Cell),
    NCell = Cell.

get_mosquito_moves(cell(mosquito,Co,P,R,C),[cell(Bug,_,_,_,_)|T],InList,Moves):-
    remove_cell(mosquito,Co,P,R,C),
    add_cell(Bug,Co,P,R,C),
    piece_helper(cell(Bug,Co,P,R,C),OutList),
    append(OutList,InList,MyList),
    remove_cell(Bug,Co,P,R,C),
    add_cell(mosquito,Co,P,R,C),
    get_mosquito_moves(cell(mosquito,Co,P,R,C),T,MyList,Moves).
get_mosquito_moves(cell(mosquito,Co,P,R,C),[cell(Bug,_,_,_,_)],InList,Moves):-
    remove_cell(mosquito,Co,P,R,C),
    add_cell(Bug,Co,P,R,C),
    piece_helper(cell(Bug,Co,P,R,C),OutList),
    remove_cell(Bug,Co,P,R,C),
    add_cell(mosquito,Co,P,R,C),
    append(OutList,InList,Moves).
get_mosquito_moves(cell(mosquito,_,_,_,_),[],Moves,Moves).

ant_valid_moves(cell(ant,Co,P,R,C),WCells) :-
    remove_cell(ant,Co,P,R,C),
    ant_walk_dfs(cell(ant,Co,P,R,C),[],PCells),
    add_cell(ant,Co,P,R,C),
    delete(PCells,cell(none,none,0,R,C),WCells).

cell_non_visited_by_ant(Cell,Visited,Ncell):-
    get_neighbor_cell(Cell,Ncell),
    empty_cell(Ncell),
    can_slide(Cell,Ncell),
    connected_to_hive(Ncell),
    \+member(Ncell,Visited),!.

ant_walk_dfs([],Visited,Visited) :- !.
ant_walk_dfs([FCell|Cells],Visited,Path) :-
    !,
    ant_walk_dfs(FCell,[FCell|Visited],NVisited),
    ant_walk_dfs(Cells,NVisited,Path).
ant_walk_dfs(Cell,Visited,Path) :-
    findall(Ncell, cell_non_visited_by_ant(Cell,Visited,Ncell), Ncells),
    ant_walk_dfs(Ncells,Visited,Path).

pillbug_valid_moves(OCell,DCell) :-
    get_neighbor_cell(OCell,NCell),
    empty_cell(NCell),
    can_slide(OCell,NCell),
    connected_to_hive_without_origin_cell(OCell,NCell),
    DCell = NCell.

bug_movable_by_pillbug(PCell,Color,MCell) :-
    get_neighbor_cell(PCell,NCell),
    \+empty_cell(NCell),
    \+top_of_pile(NCell),
    (
       (
        Color = white,
        \+is_last(NCell,black)
       );
       (
         Color = black,
        \+is_last(NCell,white)
       )
    ),
    \+is_stunned(NCell),
    findall(BCell,bridge(PCell,NCell,BCell),Bridge),
    \+length(Bridge,2),
    hamiltonian_path(NCell),
    MCell = NCell.

valid_position_to_move_a_bug(PCell,VCell):-
    get_neighbor_cell(PCell,NCell),
    empty_cell(NCell),
    VCell = NCell.

grasshopper_valid_moves(OCell,JCell) :-
    neighbor_n(OCell,NCell),
    \+empty_cell(NCell),
    jump_north(NCell,JCell).

grasshopper_valid_moves(OCell,JCell) :-
    neighbor_s(OCell,SCell),
    \+empty_cell(SCell),
    jump_south(SCell,JCell).

grasshopper_valid_moves(OCell,JCell) :-
    neighbor_ne(OCell,NECell),
    \+empty_cell(NECell),
    jump_northEast(NECell,JCell).

grasshopper_valid_moves(OCell,JCell) :-
    neighbor_se(OCell,SECell),
    \+empty_cell(SECell),
    jump_southEast(SECell,JCell).

grasshopper_valid_moves(OCell,JCell) :-
    neighbor_nw(OCell,NWCell),
    \+empty_cell(NWCell),
    jump_northWest(NWCell,JCell).

grasshopper_valid_moves(cell(grasshopper,Co,P,R,C),JCell) :-
    neighbor_sw(cell(grasshopper,Co,P,R,C),SWCell),
    \+empty_cell(SWCell),
    jump_southWest(SWCell,JCell).

jump_north(Cell,JCell) :-
    neighbor_n(Cell,NCell),
    empty_cell(NCell),
    JCell = NCell,!.

jump_north(Cell,JCell) :-
    neighbor_n(Cell,NCell),
    \+ empty_cell(NCell),
    jump_north(NCell,JCell).

jump_south(Cell,JCell) :-
    neighbor_s(Cell,SCell),
    empty_cell(SCell),
    JCell = SCell,!.

jump_south(Cell,JCell) :-
    neighbor_s(Cell,SCell),
    \+ empty_cell(SCell),
    jump_south(SCell,JCell).

jump_northEast(Cell,JCell) :-
    neighbor_ne(Cell,NECell),
    empty_cell(NECell),
    JCell = NECell,!.

jump_northEast(Cell,JCell) :-
    neighbor_ne(Cell,NECell),
    \+ empty_cell(NECell),
    jump_northEast(NECell,JCell).

jump_southEast(Cell,JCell) :-
    neighbor_se(Cell,SECell),
    empty_cell(SECell),
    JCell = SECell,!.

jump_southEast(Cell,JCell) :-
    neighbor_se(Cell,SECell),
    \+ empty_cell(SECell),
    jump_southEast(SECell,JCell).

jump_northWest(Cell,JCell) :-
    neighbor_nw(Cell,NWCell),
    empty_cell(NWCell),
    JCell = NWCell,!.

jump_northWest(Cell,JCell) :-
    neighbor_nw(Cell,NWCell),
    \+ empty_cell(NWCell),
    jump_northWest(NWCell,JCell).

jump_southWest(Cell,JCell) :-
    neighbor_sw(Cell,SWCell),
    empty_cell(SWCell),
    JCell = SWCell,!.

jump_southWest(Cell,JCell) :-
    neighbor_sw(Cell,SWCell),
    \+ empty_cell(SWCell),
    jump_southWest(SWCell,JCell).





