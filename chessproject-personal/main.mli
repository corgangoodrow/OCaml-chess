(** 
   Representation of static chess data.

   This module represents the data stored in main files, including
   the types, pieces, moves, and board.  
*)


(** the type [color] represents the color of a piece or turn of game. Grey
    represents an empty space on the board. 
*)
type color = Black | White | Grey

(**the type [name] represents the type of each chess piece and the string
   that identifies the piece to the user *)
type name = King of string | Queen of string 
          | Bishop of string | Rook of string | Knight of string | Pawn of string

(**the type [activity] represents wheather or not a piece is in play (active) 
   or has been taken (nonactive) is no longer in play *)
type activity = Active | Nonactive

(**the type [piece] uses a record to represent the three attributes of a piece:
   color, activity, and name *)
type piece = {
  col: color;
  act: activity;
  nam: name;
}

(** the type [piece_type] represents a place in the board that is either 
    contains a Piece of type piece or is an empty space on the baord (Empty)*)
type piece_type = Piece of piece | Empty

(** the type [board] uses an 2D array to represent a board where each space
    in array is of type piece_type, therefore designating where the piece lay *)
type board = piece_type array array

(** the type [game] represents the state of a game by including the following
    attributes of a game: board is the current board, turn is the current turn's
    color, cas_white is whether or not White can castle, cas_black is whether or
    not Black can castle, king_white is the current location of the White king,
    king_black is the current location of the Black king, en_passant is whether 
    or not can make the en_passant move*)
type game = {
  board: board;  
  turn: color; 
  cas_white: activity;
  cas_black: activity; 
  king_white: (int * int);
  king_black: (int * int);
  en_passant: activity;
}

(** the type [location] represents the coordinates {x,y} of a piece on the 
    board *)
type location = {x : int;
                 y: int;
                }

(** the type [result] represents a Legal or Illegal location *)
type result = Legal of location * location | Illegal


(** Functions: *)

(** [start] is the initial game. Creates a starting chess board, and initializes
    the first turn to be white's.*)
val start : game 


(** [make_move game ft sd] is the new game created by a player moving a piece
    to a new location on the board after checking if it is a valid move or is 
    in check. 
    Requires: [game] is a valid game.
    [ft], [sd] is a valid location *)
val make_move : game -> location -> location -> game 

(** [on_board x y] is a bool representing whether the coordinates ([x], [y])
    represent a valid position on a chess board.
    Requires: 
    [x] [y] must be ints. *)
val on_board : int -> int -> bool

(** [movable_piece game x y] is a bool representing whether or not a white
    piece is being moved on a white turn or a black piece is being moved on 
    a black turn.
    Requires: 
    [game] must be a valid record representing a playable chess game.
    [x] and [y] must represent a valid position in [game].board in the form
    ([x], [y]) *)

val movable_piece : game -> int -> int -> bool

(** [on_piece board x y] is the color of the piece on board [board] at
    position ([x], [y]). If the space on the board is empty, 
    returns the color Grey.
    Requires: [board] must be an 8x8 matrix where each element of board is
    a constructor of type piece_type.
    [x] and [y] must be valid coordinates on a chessboard. *)

val on_piece : board -> int -> int -> color


(** [format_board board] is a method that prints a visual representation of
    the game's current board for the users.
    Requires: 
    [board] must be an 8x8 matrix where each element of board isa constructor 
    of type piece_type. *)
val format_board : piece_type array array -> unit


(** [pawn_val board ft sd] is the boolean value representing whether or not
    the pawn on board [board] at position [ft] can move to position [sd].
    Assumes that both starting and ending positions are on the board and that
    ft represents the location of a movable piece on the board with the current
    turn.
    Requires: [board] is a board
    [ft] is a location
    [sd] is a location
*)
val pawn_val : board -> location -> location -> bool


(** [rook_val game ft sd] is the boolean representation of if the rook at
    location [ft] makes a valid move by moving to location [sd] on [game].board.
    Requires:
      [game] is a valid game
      [ft] and [sd] are not equal, are both locations on [game].board, 
    and [ft] is the location of a piece whose color is equal to [game].turn. *)
val rook_val : game -> location -> location -> bool 


(** [bishop_val game ft sd] is the boolean value representing whether the bishop
    on board [game].board can move from location [ft] to location [sd].
    Requires:
    [game] is a valid game.
    [ft] and [sd] are not equal and are both locations on [game].board, 
    and [ft] is the location of a piece whose color is equal to [game].turn.
*)
val bishop_val : game -> location -> location -> bool


(** [king_val game ft sd] is the a boolean value that represents if it is 
    valid move for a king piece. 
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [sd] is a value location.
*)
val king_val : game -> location -> location -> bool


(** [queen_val game ft sd] is the a boolean value that represents if it is 
    valid move for a queen piece. 
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [sd] is a value location.
*)
val queen_val : game -> location -> location -> bool


(** [knight_val game ft sd] is the a boolean value that represents if it is 
    valid move for a knight piece. 
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [sd] is a value location.
*)
val knight_val : game -> location -> location -> bool 


(** [string_of_color game] is the string representation of the color of the
    player whose turn it is.
    Requires: [game] is a valid game. 
*)
val string_of_color: game -> string


(** [castling_king game] is the new game created by the White or Black king
    side castle. 
    Requires: [game] is a valid game. 
*)
val castling_king : game -> game


(** [castling_queen game] is the new game created by the White or Black queen
    side castle. 
    Requires: [game] is a valid game. 
*)
val castling_queen : game -> game


(** [check game ft color] is the boolean representation if the game is in the 
    state of check.
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [color] is a valid color (White or Black or Grey)
*)
val check : game -> location -> bool 

val new_check : game -> location -> location list
