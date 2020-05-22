(** Authors: Cameron Haarmann (cmh332), Corgan Goodrow (cjg269), and Lily Kafka
    (lak254)*)

open Printf

type color = Black | White | Grey

type name = King of string | Queen of string 
          | Bishop of string | Rook of string | Knight of string | 
          Pawn of string

type activity = Active | Nonactive


type piece = {
  col: color;
  act: activity;
  nam: name;
}

type piece_type = Piece of piece | Empty

type board = piece_type array array

type game = {
  board: board;  
  turn: color; 
  cas_white: activity;
  cas_black: activity; 
  king_white: (int * int);
  king_black: (int * int);
  en_passant: activity;
}

type location = {x : int;
                 y: int;
                }

type result = Legal of location * location | Illegal



let start = 
  let init_board = Array.make_matrix 8 8 Empty in
  for x = 0 to 7 do
    init_board.(x).(1) <- Piece{col= Black; act = Active; nam = Pawn "P"} ;
    init_board.(x).(6) <- Piece{col= White; act = Active; nam = Pawn "P"} ;
  done;
  init_board.(0).(0) <- Piece{col= Black; act = Active; nam = Rook "R"} ;
  init_board.(7).(0) <- Piece{col= Black; act = Active; nam = Rook "R"} ;
  init_board.(0).(7) <- Piece{col= White; act = Active; nam = Rook "R"} ;
  init_board.(7).(7) <- Piece{col= White; act = Active; nam = Rook "R"} ;
  init_board.(1).(0) <- Piece{col= Black; act = Active; nam = Knight "k"} ;
  init_board.(6).(0) <- Piece{col= Black; act = Active; nam = Knight "k"} ;
  init_board.(1).(7) <- Piece{col= White; act = Active; nam = Knight "k"} ;
  init_board.(6).(7) <- Piece{col= White; act = Active; nam = Knight "k"} ;
  init_board.(5).(0) <- Piece{col= Black; act = Active; nam = Bishop "B"} ;
  init_board.(2).(0) <- Piece{col= Black; act = Active; nam = Bishop "B"} ;
  init_board.(5).(7) <- Piece{col= White; act = Active; nam = Bishop "B"} ;
  init_board.(2).(7) <- Piece{col= White; act = Active; nam = Bishop "B"} ;
  init_board.(3).(0) <- Piece{col= Black; act = Active; nam = Queen "Q"} ;
  init_board.(3).(7) <- Piece{col= White; act = Active; nam = Queen "Q"} ;
  init_board.(4).(0) <- Piece{col= Black; act = Active; nam = King "K"} ;
  init_board.(4).(7) <- Piece{col= White; act = Active; nam = King "K"} ;
  {
    board = init_board;
    turn = White;
    cas_white = Active;
    cas_black = Active;
    king_white = (4,7);
    king_black = (4,0);
    en_passant = Active;
  }


(** [format_helper nm] is the string representation of [nm] which is the
    name of a piece being currently matched in format_board.
    Requires: 
    [nm] must be a valid constructor of the variant type name. *)
let format_helper nm = 
  match nm with
  | Pawn p -> p
  | Rook r -> r
  | Knight kn -> kn
  | Bishop b -> b
  | King k -> k
  | Queen q -> q


let format_board board = 
  let separator = "\n   --------------------------------\n" in
  print_string "Current board:";
  for y = 0 to 7 do
    print_string separator;
    print_int (8 - y); print_string " | ";
    for x = 0 to 7 do
      match board.(x).(y) with
      | Empty -> printf " |  "
      | Piece p -> if p.col = White 
        then ANSITerminal.(print_string [white] 
                             ((format_helper p.nam) ^ " | ")) 
        else ANSITerminal.(print_string [on_black]
                             ((format_helper p.nam) ^ " | "))
    done;
  done;
  print_string "\n   --------------------------------";
  print_string "\n    A   B   C   D   E   F   G   H  \n"


let on_board (x : int) (y : int) : bool = 
  (x >= 0 && x <= 7) && (y >= 0 && y <= 7)


let on_piece (board : board) (x : int) (y : int) : color = 
  match board.(x).(y) with
  | Empty -> Grey
  | Piece c -> c.col


let movable_piece (game : game) (x : int) (y : int) : bool =
  (game.turn = White && on_piece game.board x y = White) ||
  (game.turn = Black && on_piece game.board x y = Black) 


(* A pawn move is a valid move if and only if:
    [ft] and [st] are on the board 
    AND 
    [ft] represents the location of a movable pawn
    THEN

   - IF [ft] represents the location of a white pawn
      THEN

   - IF the distance between [sd].y and [ft].y is -1
        THEN
            [sd].x = [ft].x 
            AND 
            [sd] represents the location of an Empty space
          OR
            The absolute value of [sd].x and [ft].x is 1
            AND
            [sd] represents the location of a black pawn

   - OR IF the distance between [sd].y and [sd].y is -2 AND [sd].x = [ft].x
        THEN

          [ft].y = 6 AND ([ft].x, [ft].y - 1) is Empty AND ([sd].x, [sd].y) 
          is Empty

   - OR IF [ft] represents the location of a black pawn 
      THEN

   - IF the distance between [sd].y and [ft].y is 1
        THEN
            [sd].x = [ft].x 
            AND 
            [sd] represents the location of an Empty space
          OR
            The absolute value of [sd].x and [ft].x is 1
            AND
            [sd] represents the location of a white pawn

   - OR IF the distance between [sd].y and [sd].y is 2 AND [sd].x = [ft].x
        THEN
        [ft].y = 1 AND ([ft].x, [ft].y + 1) is Empty AND ([sd].x, [sd].y) 
        is Empty
*)  


(** [pawn_val_white board ft sd] is the boolean value reprsenting whether
    or not the white pawn on board [board] at position [ft] can move to position
    [sd]. 
    Requires: [board] is a board
    [ft] is a location
    [sd] is a location*)
let pawn_val_white (board : board) (ft : location) (sd : location) : bool =
  if (sd.y - ft.y = (-1)) 
  then (sd.x = ft.x && on_piece board sd.x sd.y = Grey) ||
       (Int.abs(sd.x - ft.x) = 1 && on_piece board sd.x sd.y = Black)
  else if (sd.y - ft.y = (-2))
  then ft.x = sd.x && ft.y = 6 && on_piece board ft.x (ft.y - 1) = Grey &&
       on_piece board ft.x sd.y = Grey
  else false


(** [pawn_val_black board ft sd] is the boolean value reprsenting whether
    or not the black pawn on board [board] at position [ft] can move to position
    [sd]. 
    Requires: [board] is a board
    [ft] is a location
    [sd] is a location *)
let pawn_val_black (board : board) (ft : location) (sd : location) : bool =
  if (sd.y - ft.y = 1) 
  then (sd.x = ft.x && on_piece board sd.x sd.y = Grey) ||
       (Int.abs(sd.x - ft.x) = 1 && on_piece board sd.x sd.y = White)
  else if (sd.y - ft.y = 2)
  then ft.x = sd.x && ft.y = 1 && on_piece board ft.x (ft.y + 1) = Grey &&
       on_piece board ft.x sd.y = Grey
  else false


let pawn_val (board : board) (ft : location) (sd : location) : bool = 
  if on_piece board ft.x ft.y = White 
  then pawn_val_white board ft sd
  else pawn_val_black board ft sd


(* Assuming that [ft] and [sd] are on the board,that [ft] is not equal 
   to [sd] and that [ft] is the location of a movable rook in relation to the 
   turn of the game, a rook move from [ft] to [sd] is valid if and only if:

   The turn of the game does not equal the color of the piece at [sd]
   THEN
   - IF [sd].x = [ft].x 
     THEN
   - IF [sd].y - [ft].y > 0
      THEN
        ([ft].x, [ft].y + 1), ... ([ft].x, [sd].y - 1) contains no pieces
   - ELSE IF [sd].y - [ft].y < 0
        ([ft].x, [ft].y - 1), ... ([ft].x, [sd].y + 1) contains no pieces  
   - ELSE IF [sd].y = [ft].y
     THEN
   - IF [sd].x - [ft].x > 0
      THEN
        ([ft].x + 1, [ft].y), ... ([sd].x - 1, [ft].y) contains no pieces
   - ELSE IF [sd].x - [ft].x < 0
      THEN
        ([ft].x - 1, [ft].y), ... ([sd].x + 1, [ft].y) contains no pieces
*)



(** [vertical_recurse game x a b] is a recursive helper function to
    rook_val that returns the boolean value representing if all the vertical
    spaces from ([x], [a]) to ([x], [b]) on [game].board contain no pieces.
    Requires:
      [game] is a valid game
      The path from ([x], [a]) to ([x], [b]) on [game].board is a valid 
      rook maneuver. *)
let rec vertical_recurse (board : board) (x : int) (a : int) (b : int) : bool =
  if b = a
  then on_piece board x a = Grey 
  else if a < b
  then
    if on_piece board x a <> Grey 
    then false else vertical_recurse board x (a + 1) b
  else
  if on_piece board x a <> Grey
  then false else vertical_recurse board x (a - 1) b


(** [horizontal_recurse game y a b] is a recursive helper function to
    rook_val that returns the boolean value representing if all the horizontal
    spaces from ([a], [y]) to ([b], [y]) on [game].board contain no pieces.
    Requires:
      [game] is a valid game
      The path from ([a], [y]) to ([b], [y]) on [game].board is a valid 
      rook maneuver. *)
let rec horizontal_recurse 
    (board : board) (y : int) (a : int) (b : int) : bool =
  if b = a
  then on_piece board a y = Grey 
  else if a < b
  then
    if on_piece board a y <> Grey
    then false else horizontal_recurse board y (a + 1) b
  else 
  if on_piece board a y <> Grey
  then false else horizontal_recurse board y (a - 1) b


(** [rook_x_constant game ft sd] is a helper function of rook_val that is the
    boolean representation of if the rook at location [ft] makes a valid move by 
    moving vertically to position [sd] on board [game].board
    Requires: [game] is a valid game [ft] and [sd] are not equal, are both 
    locations on [game].board, have a path to each other that is vertical 
    and [ft] is the location of a piece whose color is equal to [game].turn. *)
let rook_x_constant (game : game) (ft : location) (sd : location) : bool =
  Int.abs (sd.y - ft.y) = 1 ||
  if ft.y < sd.y
  then
    vertical_recurse game.board ft.x (ft.y + 1) (sd.y - 1)
  else
    vertical_recurse game.board ft.x (ft.y - 1) (sd.y + 1)


(** [rook_y_constant game ft sd] is a helper function of rook_val that is the
    boolean representation of if the rook at location [ft] makes a valid move by 
    moving horizontally to position [sd] on board [game].board
    Requires: [game] is a valid game [ft] and [sd] are not equal, are both 
    locations on [game].board, have a path to each other that is horizontal 
    and [ft] is the location of a piece whose color is equal to [game].turn. *)
let rook_y_constant (game : game) (ft : location) (sd : location) : bool =
  Int.abs (sd.x - ft.x) = 1 ||
  if ft.x < sd.x
  then
    horizontal_recurse game.board ft.y (ft.x + 1) (sd.x - 1)
  else
    horizontal_recurse game.board ft.y (ft.x - 1) (sd.x + 1)



let rook_val (game : game) (ft : location) (sd : location) : bool =
  if game.turn <> on_piece game.board sd.x sd.y 
  then 
    if sd.x = ft.x
    then
      rook_x_constant game ft sd
    else if sd.y = ft.y
    then
      rook_y_constant game ft sd
    else false
  else false


(* Assuming that [ft] and [sd] are on the board, that [ft] is not equal 
   to [sd] and that [ft] is the location of a movable rook in relation to the 
   turn of the game, a bishop move from [ft] to [sd] is valid if and only if:

   The turn of the game is not the same as the color of the piece at [sd]
   THEN
   IF the absolute value of the difference of [sd].x and [ft].x and the 
   difference of [sd].y and [ft].y is the same
   THEN
    The absolute value of [sd].y and [ft].y is 1
    OR
    IF the difference of [sd].x and [ft].x and the difference of [sd].y and 
    [ft].y are both positive
    THEN
      recurse positions (([ft].x + 1), ([ft].y + 1)), 
      ... (([sd].x - 1), ([sd].y - 1)) are Empty
    ELSE IF the difference of [sd].x and [ft].x is positive and the difference
    of [sd].y and [ft].y is negative
    THEN
      recurse positions (([ft].x + 1), ([ft].y - 1)), 
      ... (([sd].x - 1), ([sd].y + 1)) are Empty
    ELSE IF the difference of [sd].x and [ft].x is negative and the difference
    of [sd].y and [ft].y is positive
    THEN
      recurse positions (([ft].x - 1), ([ft].y + 1)),
      ... (([sd].x + 1), ([sd].y - 1)) are Empty
    ELSE
      recurse positions (([ft].x - 1), ([ft].y - 1)),
      ... (([sd].x + 1), ([sd].y + 1)) are Empty
*)


(** [bishop_recurse game x1 y1 x2 y2] is a recursive helper function to
    bishop_val that returns the boolean value representing if all the diagonal
    spaces from ([x1], [y1]) to ([x2], [y2]) on [game].board contain no pieces.
    Requires:
      [game] is a valid game
      The path from ([x1], [y1]) to ([x2], [y2]) on [game].board is a valid 
      bishop maneuver.  *)
let rec bishop_recurse 
    (board : board) (x1 : int) (y1 : int) (x2 : int) (y2 : int) : bool =
  if (x1 = x2) && (y1 = y2) 
  then on_piece board x2 y2 = Grey
  else if (x2 - x1 > 0) && (y2 - y1 > 0)
  then if on_piece board x1 y1 <> Grey then false else
      bishop_recurse board (x1 + 1) (y1 + 1) x2 y2
  else if (x2 - x1 < 0) && (y2 - y1 > 0)
  then if on_piece board x1 y1 <> Grey then false else
      bishop_recurse board (x1 - 1) (y1 + 1) x2 y2
  else if (x2 - x1 > 0) && (y2 - y1 < 0)
  then if on_piece board x1 y1 <> Grey then false else
      bishop_recurse board (x1 + 1) (y1 - 1) x2 y2
  else
    bishop_recurse board (x1 - 1) (y1 - 1) x2 y2




let bishop_val (game : game) (ft : location) (sd : location) : bool =
  if (game.turn <> on_piece game.board sd.x sd.y) &&
     (Int.abs (sd.x - ft.x) = Int.abs (sd.y - ft.y))
  then
    Int.abs (sd.x - ft.x) = 1 ||
    if (sd.x - ft.x > 0) && (sd.y - ft.y) > 0
    then
      bishop_recurse game.board (ft.x + 1) (ft.y + 1) (sd.x - 1) (sd.y - 1)
    else if (sd.x - ft.x < 0) && (sd.y - ft.y > 0)
    then
      bishop_recurse game.board (ft.x - 1) (ft.y + 1) (sd.x + 1) (sd.y - 1)
    else if (sd.x - ft.x > 0) && (sd.y - ft.y < 0)
    then
      bishop_recurse game.board (ft.x + 1) (ft.y - 1) (sd.x - 1) (sd.y + 1)
    else
      bishop_recurse game.board (ft.x - 1) (ft.y - 1) (sd.x + 1) (sd.y + 1)
  else false 




let queen_val game ft sd = 
  bishop_val game ft sd || rook_val game ft sd


(** [knight_val_move ft sd] is the a boolean value that checks if a knight
    piece only moves 3 spaces total, but 3 spaces cannot be only vertically or
    horizontally. 
    Requires: 
    [ft] is a valid location.
    [sd] is a value location.
*)
let knight_val_move ft sd = 
  Int.abs(sd.x - ft.x) + Int.abs(sd.y - ft.y) = 3 && 
  Int.abs(sd.x - ft.x) <> 3 && Int.abs(sd.y - ft.y) <> 3



let knight_val game ft sd = 
  if on_board sd.x sd.y && knight_val_move ft sd then
    game.turn = Black && on_piece game.board sd.x sd.y = White ||
    game.turn = White && on_piece game.board sd.x sd.y = Black || 
    on_piece game.board sd.x sd.y = Grey  
  else false 



let string_of_color game = 
  if game.turn = White then "White" else if game.turn = Black then "Black" else 
    failwith "invalid color"


(** [overall_castle_check game] is the boolean representation if the game is
    in a state where White or Black players can make a castle move. 
    Requires: [game] is a valid game. 
*)
let overall_castle_check game =
  (game.turn = White && game.cas_white = Nonactive) 
  || (game.turn = Black && game.cas_black = Nonactive) 


(** [king_castle_white_helper game] is the boolean representation if a White
    player has their king piece and rook piece in the right position, with no
    pieces in between to make a valid king side castle move. 
    Requires: [game] is a valid game. 
*)
let king_castle_white_helper game =
  game.turn = White && 
  (match game.king_white with
   | (x,y) when x = 4 && y = 7 -> true 
   | _ -> false) && 
  (match game.board.(7).(7) with 
   | Piece p when p.nam = Rook "R" -> true             
   | _ -> failwith "cant castle") &&
  (match game.board.(5).(7), game.board.(7).(7) with
   | Empty, Empty -> true
   | _ -> false)


(** [king_castle_black_helper game] is the boolean representation if a Black
    player has their king piece and rook piece in the right position, with no
    pieces in between to make a valid king side castle move. 
    Requires: [game] is a valid game. 
*)
let king_castle_black_helper game = 
  game.turn = Black && 
  (match game.king_black with
   | (x,y) when x = 4 && y = 0 -> true 
   | _ -> false) && 
  (match game.board.(7).(0) with 
   | Piece p when p.nam = Rook "R" -> true             
   | _ -> failwith "cant castle")&&
  (match game.board.(5).(0), game.board.(7).(0) with
   | Empty, Empty -> true
   | _ -> false)



let castling_king game =
  if overall_castle_check game
  then game 
  else if king_castle_white_helper game
  then (game.board.(5).(7) <- game.board.(7).(7);
        game.board.(6).(7) <- game.board.(4).(7);
        game.board.(7).(7) <- Empty;
        game.board.(4).(7) <- Empty;
        {game with turn = Black; cas_white = Nonactive; king_white = (7,6)};)
  else if king_castle_black_helper game
  then (game.board.(5).(0) <- game.board.(7).(0);
        game.board.(6).(0) <- game.board.(4).(0);
        game.board.(7).(0) <- Empty;
        game.board.(4).(0) <- Empty;
        {game with turn = White; cas_black = Nonactive; king_black = (6,0)};)
  else game


(** [queen_castle_white_helper game] is the boolean representation if a White
    player has their king piece and rook piece in the right position, with no
    pieces in between to make a valid queen side castle move. 
    Requires: [game] is a valid game. 
*)
let queen_castle_white_helper game = 
  game.turn = White && 
  (match game.king_white with
   | (x,y) when x = 4 && y = 7 -> true 
   | _ -> false) && 
  (match game.board.(0).(7) with 
   | Piece p when p.nam = Rook "R" -> true             
   | _ -> failwith "cant castle") &&
  (match game.board.(1).(7), game.board.(2).(7), game.board.(3).(7) with
   | Empty, Empty, Empty -> true
   | _ -> false)


(** [queen_castle_black_helper game] is the boolean representation if a Black
    player has their king piece and rook piece in the right position, with no
    pieces in between to make a valid queen side castle move. 
    Requires: [game] is a valid game. 
*)
let queen_castle_black_helper game =
  game.turn = Black && 
  (match game.king_black with
   | (x,y) when x = 4 && y = 0 -> true 
   | _ -> false) && 
  (match game.board.(0).(0) with 
   | Piece p when p.nam = Rook "R" -> true             
   | _ -> failwith "cant castle")&&
  (match game.board.(1).(0), game.board.(2).(0), game.board.(3).(0) with
   | Empty, Empty, Empty -> true
   | _ -> false)



let castling_queen game = 
  if overall_castle_check game
  then game 
  else if queen_castle_white_helper game
  then (game.board.(2).(7) <- game.board.(4).(7);
        game.board.(3).(7) <- game.board.(0).(7);
        game.board.(0).(7) <- Empty;
        game.board.(4).(7) <- Empty;
        {game with turn = Black; cas_white = Nonactive; king_white = (2,7)};)
  else if queen_castle_black_helper game
  then (game.board.(2).(0) <- game.board.(4).(0);
        game.board.(3).(0) <- game.board.(0).(0);
        game.board.(0).(0) <- Empty;
        game.board.(4).(0) <- Empty;
        {game with turn = White; cas_black = Nonactive; king_black = (2,0)};)
  else game



let king_val game ft sd = 
  if on_board sd.x sd.y && 
     ((Int.abs(sd.x - ft.x) = 1 && Int.abs(sd.y - ft.y) = 0) || 
      (Int.abs(sd.y - ft.y) = 1 && Int.abs(sd.x - ft.x) = 0) || 
      (Int.abs(sd.y - ft.y) = 1 && Int.abs(sd.x - ft.x) = 1))
  then
    if game.turn = Black && on_piece game.board sd.x sd.y = White ||
       game.turn = White && on_piece game.board sd.x sd.y = Black || 
       on_piece game.board sd.x sd.y = Grey then true else false 
  else false


(** [mave_move_helper game ft sd] is the boolean representation that checks if
    the piece on the first given location can make a valid move to the second 
    location.
    Requires: [game] is a valid game. 
    [ft] is a valid location.
    [sd] is a valid location. 
*)
let make_move_helper game ft sd =
  match game.board.(ft.x).(ft.y) with
  | Piece s -> (match s.nam  with
      | Knight "k" -> knight_val game ft sd
      | Queen "Q" ->  queen_val game ft sd
      | King "K" -> king_val game ft sd
      | Bishop "B" -> bishop_val game ft sd
      | Rook "R" -> rook_val game ft sd
      | Pawn "P" -> pawn_val game.board ft sd
      | _ -> failwith "The string component of s is incorrect." 
    )
  | Empty -> failwith 
               "The postcondition of movable_piece has been violated"



(* Assuming that [ft] is on the board, a piece is in check if and only if

   If the arbitrary square is not the base square
   AND
   if the piece on the arbitrary square is the opposite color
   AND
   if the helper function returns true


   a piece 
*)

(* [check_square game ft x y] is a helper function to check and is the boolean
   representation of if the piece at position ([ft].x, [ft].y) of 
   [game].board is able to be captured by the whatever is on position (x, y).
   Requires:
    [game] is a valid game
    [ft] is the location of some piece on [game].board
    ([x], [y]) is a location on [game].board. *)
let check_square (game : game) (ft : location) (x : int) (y : int) : bool =
  (ft.x <> x || ft.y <> y) && 
  (game.turn <> on_piece game.board x y && on_piece game.board x y <> Grey) &&
  (if game.turn = White 
   then make_move_helper {game with turn = Black} {x = x; y = y} ft
   else make_move_helper {game with turn = White} {x = x; y = y} ft)


let check (game : game) (ft : location) : bool =
  let rec check_helper 
      (game : game) (ft : location) (x : int) (y : int) : bool =
    if x = 0 && y = 0 
    then 
      check_square game ft x y
    else if x = 0 then 
      if check_square game ft x y  
      then true
      else check_helper game ft 7 (y - 1)
    else if check_square game ft x y
    then true
    else check_helper game ft (x - 1) y
  in check_helper game ft 7 7 


(*let check game ft color = 
  let acc : bool list = [] in 
  for a = 0 to 7 do
    for b = 0 to 7 do 
      match game.board.(a).(b) with 
      | Empty -> acc
      | Piece p -> if make_move_helper game {x = a ; y = b} ft && color <> p.col 
        then acc else true::acc 
    done ; 
  done ;
  List.length acc <> 0 *)


(**[king_val_check game sd] is boolean representation if king is moving into
   check or not
   Requires: [game] is valid game.
   [sd] is a valid location*)
let king_val_check (game : game) (sd : location) : bool = 
  check game sd 


(** [checkmate_corner game ft] is a helper function of checkmate that is
    the boolean representation of if the piece at location [ft], which is a
    location in a corner of the board, is in "checkmate" on [game].board. 
    See the specification for function checkmate to learn the definition of 
    a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is a location on the board. *)
let checkmate_corner (game : game) (ft : location) : bool =
  if (ft.x = 0 && ft.y = 0) then
    check game {x = 1; y = 0} &&
    check game {x = 0; y = 1} &&
    check game {x = 1; y = 1}
  else if (ft.x = 7 && ft.y = 0) then
    check game {x = 6; y = 0} &&
    check game {x = 7; y = 1} &&
    check game {x = 6; y = 1} 
  else if (ft.x = 0 && ft.y = 7) then
    check game {x = 0; y = 6} &&
    check game {x = 1; y = 7} &&
    check game {x = 1; y = 6}
  else 
    check game {x = 6; y = 7} &&
    check game {x = 7; y = 6} &&
    check game {x = 6; y = 6}


(** [checkmate_edge_two game ft] is a helper function of checkmate_edge that
    is the boolean representation of if the piece at location [ft], which is a
    location on y = 7 edge of the board but not in a corner, is in 
    "checkmate" on [game].board. See the specification for function 
    checkmate to learn thedefinition of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is location that represents the edge y = 7 on the board but is not a 
    corner. *)
let checkmate_edge_four game ft =
  check game {x = (ft.x - 1); y = ft.y} &&
  check game {x = (ft.x - 1); y = (ft.y - 1)} &&
  check game {x = ft.x; y = (ft.y - 1)} &&
  check game {x = (ft.x + 1); y = (ft.y - 1)} &&
  check game {x = (ft.x + 1); y = ft.y}


(** [checkmate_edge_two game ft] is a helper function of checkmate_edge that
    is the boolean representation of if the piece at location [ft], which is a
    location on x = 7 edge of the board but not in a corner, is in 
    "checkmate" on [game].board. See the specification for function 
    checkmate to learn thedefinition of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is location that represents the edge x = 7 on the board but is not a 
    corner. *)
let checkmate_edge_three game ft =
  check game {x = ft.x; y = (ft.y - 1)} &&
  check game {x = (ft.x - 1); y = (ft.y - 1)} &&
  check game {x = (ft.x - 1); y = ft.y} &&
  check game {x = (ft.x - 1); y = (ft.y + 1)} &&
  check game {x = ft.x; y = (ft.y + 1)}


(** [checkmate_edge_two game ft] is a helper function of checkmate_edge that
    is the boolean representation of if the piece at location [ft], which is a
    location on y = 0 edge of the board but not in a corner, is in 
    "checkmate" on [game].board. See the specification for function 
    checkmate to learn thedefinition of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is location that represents the edge y = 0 on the board but is not a 
    corner. *)
let checkmate_edge_two game ft =
  check game {x = (ft.x - 1); y = ft.y} &&
  check game {x = (ft.x - 1); y = (ft.y + 1)} &&
  check game {x = ft.x; y = (ft.y + 1)} &&
  check game {x = (ft.x + 1); y = (ft.y + 1)} &&
  check game {x = (ft.x + 1); y = ft.y}


(** [checkmate_edge_two game ft] is a helper function of checkmate_edge that
    is the boolean representation of if the piece at location [ft], which is a
    location on x = 0 edge of the board but not in a corner, is in 
    "checkmate" on [game].board. See the specification for function 
    checkmate to learn thedefinition of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is location that represents the edge x = 0 on the board but is not a 
    corner. *)
let checkmate_edge_one game ft =
  check game {x = ft.x; y = (ft.y - 1)} &&
  check game {x = (ft.x + 1); y = (ft.y - 1)} &&
  check game {x = (ft.x + 1); y = ft.y} &&
  check game {x = (ft.x + 1); y = (ft.y + 1)} &&
  check game {x = ft.x; y = (ft.y + 1)}


(** [checkmate_edge game ft] is a helper function of checkmate that is
    the boolean representation of if the piece at location [ft], which is a
    location on the edge of the board but not in a corner, is in "checkmate" on 
    [game].board. See the specification for function checkmate to learn the
    definition of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is location that represents and edge on the board but is not a 
    corner. *)
let checkmate_edge (game : game) (ft : location) : bool =
  if ft.x = 0 then
    checkmate_edge_one game ft
  else if ft.y = 0 then
    checkmate_edge_two game ft
  else if ft.x = 7 then
    checkmate_edge_three game ft
  else
    checkmate_edge_four game ft


(** [checkmate_not_edge game ft] is a helper function of checkmate that is
    the boolean representation of if the piece at location [ft], which is not
    an edge or a corner on the board, is in "checkmate" on [game].board.
    See the specification for function checkmate to learn the definition
    of a piece being in checkmate.
    Requires:
    [game] is a valid game
    [ft] is a location on the board that is not a corner or edge. *)
let checkmate_not_edge (game : game) (ft : location) : bool =
  check game {x = ft.x + 1; y = ft.y} && 
  check game {x = ft.x + 1; y = ft.y + 1} && 
  check game {x = ft.x ; y = ft.y + 1} && 
  check game {x = ft.x - 1; y = ft.y + 1} && 
  check game {x = ft.x - 1; y = ft.y} && 
  check game {x = ft.x - 1; y = ft.y - 1} && 
  check game {x = ft.x ; y = ft.y - 1} && 
  check game {x = ft.x + 1; y = ft.y - 1} 


let checkmate game ft =
  check game ft &&
  if (ft.x = 0 || ft.x = 7) && (ft.y = 0 || ft.y = 7)
  then
    checkmate_corner game ft
  else if ft.x = 0 || ft.x = 7 || ft.y = 0 || ft.y = 7
  then
    checkmate_edge game ft
  else
    checkmate_not_edge game ft



(** [apply_board_change game ft sd] is the new game returned when a player
    makes a move from the first location to the second location. The game
    switches to the next players turn.
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [sd] is a valid location.
*)
let apply_board_change game ft sd = 
  (game.board.(sd.x).(sd.y) <- game.board.(ft.x).(ft.y);
   game.board.(ft.x).(ft.y) <- Empty;
   if game.turn = White then {game with turn = Black}
   else 
     {game with turn = White})


(** [change_board game ft sd] is the new game returned when a player
    makes a move from the first location to the second location using a King
    piece. 
    Requires: [game] is a valid game.
    [ft] is a valid location.
    [sd] is a valid location.
*)
let change_board game ft sd =
  (match game.board.(ft.x).(ft.y) with
   | Piece p when p.nam = King "K" && p.col = White -> 
     { (apply_board_change game ft sd)  with king_white = (sd.x,sd.y)} 
   | Piece p when p.nam = King "K" && p.col = Black -> 
     { (apply_board_change game ft sd)  with king_black = (sd.x,sd.y)} 
   | _ -> apply_board_change game ft sd
  )


(** [check_color game] is the location of the White or Black king based off
    which color player's turn it is.
    Requires: [game] is a valid game. *)
let check_color game =
  if game.turn = White then (match game.king_white with
      | (s,v) -> {x = s; y = v}
    ) else (match game.king_black with
      | (s,v) -> {x = s; y = v}
    )


(** [not_stagnant ft sd] is boolean represenation that checks if a piece is
    moving to a different square. 
    Requires: [ft], [sd] is a valid location *)
let not_stagnant ft sd =
  (ft.x <> sd.x) || (ft.y <> sd.y)



let check_move_basics game ft sd =
  on_board ft.x ft.y && on_board sd.x sd.y 
  && movable_piece game ft.x ft.y && not_stagnant ft sd


let move_king game ft =
  match game.board.(ft.x).(ft.y) with
  | Piece p when p.nam = King "K" -> true 
  | _ -> false 


let make_move game ft sd = 
  if check_move_basics game ft sd && move_king game ft && check game sd 
  then if make_move_helper game ft sd then 
      (ANSITerminal.(print_string [red] ("Cannot move into check")); game)
    else change_board game ft sd
  else if checkmate game ft 
  then (print_string ("" ^ (string_of_color game) ^ 
                      "is in checkmate. Game Over"); 
        {game with board = Array.make_matrix 8 8 Empty}) 
  else if check_move_basics game ft sd && make_move_helper game ft sd 
  then change_board game ft sd  
  else failwith "Invalid input. Please enter something else."


(* if check_move_basics game ft sd 
   then (if make_move_helper game ft sd  
        && (check (change_board game ft sd) (check_color game))
        then (print_endline "You are in check"; game)  
        else change_board game ft sd )
   else *)

