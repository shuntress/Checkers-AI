%%%%%%%%%%%%%%%%%
%  Checkers.erl %
%%%%%%%%%%%%%%%%%

-module(checkers).
-compile(export_all).

% A checkers AI using alpha-beta pruning to evaluate game states

%%%%%%%%%%

% Returns the adjacent squares on the checkers board. Board is
% numbered as follows, with numbers beginning from black's
% end of the board.

%+-----------------------+
%|  |32|  |31|  |30|  |29|
%|28|  |27|  |26|  |25|  |
%|  |24|  |23|  |22|  |21|
%|20|  |19|  |18|  |17|  |
%|  |16|  |15|  |14|  |13|
%|12|  |11|  |10|  |09|  |
%|  |08|  |07|  |06|  |05|
%|04|  |03|  |02|  |01|  |
%+-----------------------+

adjacent_squares(Current) ->
  % Just a simple lookup table,
  % this is probably a good candidate for a refactoring
  case Current of
    1 -> [5, 6];
    2 -> [6, 7];
    3 -> [7, 8];
    4 -> [8];
    5 -> [1, 9];
    6 -> [1, 2, 9, 10];
    7 -> [2, 3, 10, 11];
    8 -> [3, 4, 11, 12];
    9 -> [5, 6, 13, 14];
    10 -> [6, 7, 14, 15];
    11 -> [7, 8, 15, 16];
    12 -> [8, 16];
    13 -> [9, 17];
    14 -> [9, 10, 17, 18];
    15 -> [10, 11, 18, 19];
    16 -> [11, 12, 19, 20];
    17 -> [13, 14, 21, 22];
    18 -> [14, 15, 22, 23];
    19 -> [15, 16, 23, 24];
    20 -> [16, 24];
    21 -> [17, 25];
    22 -> [17, 18, 25, 26];
    23 -> [18, 19, 26, 27];
    24 -> [19, 20, 27, 28];
    25 -> [21, 22, 29, 30];
    26 -> [22, 23, 30, 31];
    27 -> [23, 24, 31, 32];
    28 -> [24, 32];
    29 -> [25];
    30 -> [25, 26];
    31 -> [26, 27]; 
    32 -> [27, 28]
  end.
 
% Returns a piece tuple for a given Color and Type
get_piece(Color, Type) ->
  {piece, Color, Type}.

% Returns next available moves for Player
successors(Board, Player) ->
  % Find all moves from squares to empty squares
  Moves = [ {move, Type, From, To} || 
              {From, {piece, P, Type}} <- Board,  % Pull Player's pieces from the board
              {To, empty} <- Board,               % Take To from all empty squares
               P =:= Player,
               lists:member(To, adjacent_squares(From))], % Ensure the empty square belong to the adj
                                                          % squares of "From"
  
  % Remove invalid moves for regular pieces
  lists:filter(fun({move, man, F, T}) -> case Player of black -> F < T; white -> F > T end end, Moves).

  % next, jumps need to be considered, so we'll concatenate those moves...
