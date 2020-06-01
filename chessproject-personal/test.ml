(* open OUnit *)
open OUnit
open Main
(*open Game
  open Command *)


(** Test Plan:

    Our appraoch to testing was to use OUnit modules for each function in 
    main.ml. Then for each function we created specific tests including: 
    each valid move function for each chess piece, on_board functions, 
    on_piece functions, functions to make specific moves, 
    functions to check color and pieces, and functions to put all these smaller 
    functions together. The tests cases were developed using glass box testing 
    because we used the implementation of the source code. We created
    tests for each execution paths for moving each white and black chess piece
    on our chess board, more specically we tested each white or black piece
    with other white and black pieces to see if they could mave a valid move
    (moving forward, bakcward, sideways, and diagnol), could take a piece, 
    or could not make a move. While we created test cases for all these 
    situations, we believe we omitted specific cases when pieces interacted
    on the chess board, as there is an endless amount of combinations where
    different pieces can interact in chess. Therefore, our tests cover the 
    amount of common moves in chess that we could think of. Overall, we 
    believe our test suite demonstrates the correctness of our system 
    because we created tests for each function in our main.ml. In addition, we
    created over 100 specific edge cases to test these functions and all 
    cases pass 'make test'. 
*)


(** [] constructs an OUnit test named [name] that asserts the 
    quality of [expected_output] with []. *)


let rec string_of_loc_list = function
  | [] -> ""
  | h::t -> "(" ^ string_of_int(h.x) ^ ", " ^ string_of_int (h.y) ^ "), " ^
            string_of_loc_list t


(** [make_on_board_test name input_x input_y expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [on_board input_x input_y].*)
let make_on_board_test 
    (name : string)
    (input_x : int)
    (input_y : int)
    (expected_output : bool) : test =
  name >:: (fun _ ->
      assert_equal (on_board input_x input_y) expected_output
        ~printer:string_of_bool)


(** [make_on_piece_test name input_board input_x input_y] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with [on_piece input_board input_x input_y]. *)
let make_on_piece_test
    (name : string)
    (input_board : piece_type array array)
    (input_x : int)
    (input_y : int)
    (expected_output : color) : test =
  name >:: (fun _ ->
      assert_equal (on_piece input_board input_x input_y) expected_output)


(** [make_movable_piece_test name input_game input_x input_y] 
    constructs an OUnit test named [name] that asserts the quality of 
    [expected_output] with []. *)
let make_movable_piece_test
    (name : string)
    (input_game : game)
    (input_x : int)
    (input_y : int) 
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (movable_piece input_game input_x input_y)
               expected_output~printer:string_of_bool)


(** [make_pawn_val_test name input_board input_x input_y] constructs 
    an OUnit test named [name] that asserts the quality of 
    [expected_output] with [pawn_val input_board input_x input_y]. *)
let make_pawn_val_test
    (name : string)
    (input_board : piece_type array array)
    (input_ft : location)
    (input_sd : location)
    (expected_output : bool) : test =
  name >:: (fun _ -> assert_equal (pawn_val input_board input_ft input_sd)
               expected_output ~printer:string_of_bool)


(** [make_rook_val_test name input_game input_ft input_sd] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [rook_val input_game input_ft input_sd]. *)
let make_rook_val_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (input_sd : location) 
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (rook_val input_game input_ft input_sd)
               expected_output ~printer:string_of_bool)



(** Board used for on_board_tests and on_piece_tests.  *)
let board_one = Array.make_matrix 8 8 Main.Empty
let _ = 
  board_one.(0).(0) <- Piece{col= Black; act = Active; nam = Pawn "P"};
  board_one.(0).(7) <- Piece{col= White; act = Active; nam = Queen "Q"}; ()

(** Board and games used for movable_piece_tests. *)
let board_two = Array.make_matrix 8 8 Main.Empty
let _ =
  board_two.(4).(0) <- Piece{col = Black; act = Active; nam = Bishop "B"};
  board_two.(0).(4) <- Piece{col = White; act = Active; nam = Rook "R"}; ()

let game_one = {board = board_two; turn = White; cas_white = Active;
                cas_black = Nonactive; king_white = (4, 2);
                king_black = (5, 6); en_passant = Nonactive}
let game_two = {game_one with turn = Black}

(** Boards used for pawn_val_tests.*)
let pawn_val_board_white = Array.make_matrix 8 8 Main.Empty
let pawn_val_board_black = Array.make_matrix 8 8 Main.Empty
let _ =
  pawn_val_board_white.(0).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};
  pawn_val_board_white.(7).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};
  pawn_val_board_white.(7).(5) <- Piece{col = White; act = Active; nam = Pawn "P"};
  pawn_val_board_white.(2).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};
  pawn_val_board_white.(2).(4) <- Piece{col = White; act = Active; nam = Pawn "P"};
  pawn_val_board_white.(1).(5) <- Piece{col = Black; act = Active; nam = Queen "Q"};
  pawn_val_board_white.(6).(5) <- Piece{col = White; act = Active; nam = Pawn "P"};

  ()

(** White and black boards and games used for rook_val_tests. *)

let rook_val_board_w = Array.make_matrix 8 8 Main.Empty 
let _ =
  rook_val_board_w.(1).(6) <- Piece{col = White; act = Active; nam = Rook "R"};
  rook_val_board_w.(6).(6) <- Piece{col = White; act = Active; nam = Rook "R"};
  rook_val_board_w.(1).(7) <- Piece{col = White; act = Active; nam = Pawn "P"};
  rook_val_board_w.(0).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};
  rook_val_board_w.(2).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};
  rook_val_board_w.(1).(5) <- Piece{col = White; act = Active; nam = Pawn "P"};
  rook_val_board_w.(3).(6) <- Piece{col = Black; act = Active; nam = Pawn "P"};

  ()

let rook_val_board_b = Array.make_matrix 8 8 Main.Empty 
let _ =
  rook_val_board_b.(1).(6) <- Piece{col = Black; act = Active; nam = Rook "R"};
  rook_val_board_b.(6).(6) <- Piece{col = Black; act = Active; nam = Rook "R"};
  rook_val_board_b.(1).(7) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  rook_val_board_b.(0).(6) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  rook_val_board_b.(2).(6) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  rook_val_board_b.(1).(5) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  rook_val_board_b.(3).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};

  ()

let rook_game_w = 
  {
    board = rook_val_board_w;
    turn = White;
    cas_white = Active;
    cas_black = Active;
    king_white = (4,7);
    king_black = (4,0);
    en_passant = Active;
  }

let rook_game_b = 
  {
    board = rook_val_board_b;
    turn = Black;
    cas_white = Active;
    cas_black = Active;
    king_white = (4,7);
    king_black = (4,0);
    en_passant = Active;
  }


(* let rook_val_board_b = Array.make_matrix 8 8 Main.Empty

   rook_val_board_b.(0).(0) <- Piece{col = Black; act = Active; nam = Rook "R"};
   rook_val_board_b.(7).(0) <- Piece{col = Black; act = Active; nam = Rook "R"}; *)

(* Unit tests for function on_board in module Main. *)
let on_board_tests = [
  make_on_board_test "top left" 0 0 true;
  make_on_board_test "top right" 7 0 true;
  make_on_board_test "bottom left" 0 7 true;
  make_on_board_test "bottom right" 7 7 true;
  make_on_board_test "middle" 3 4 true;
  make_on_board_test "half right half wrong" 4 8 false;
  make_on_board_test "both wrong" 8 8 false;
]

(* Unit tests for function on_piece in module Main. *)
let on_piece_tests = [
  make_on_piece_test "black piece" board_one 0 0 Black;
  make_on_piece_test "white piece" board_one 0 7 White;
  make_on_piece_test "no piece" board_one 4 3 Grey;
]

(* Unit tests for function movable_piece in module Main. *)
let movable_piece_tests = [
  make_movable_piece_test "White turn moving a white piece" game_one 0 4 true;
  make_movable_piece_test "White turn moving a black piece" game_one 4 0 false;
  make_movable_piece_test "Black turn moving a black piece" game_two 4 0 true;
  make_movable_piece_test "Black turn moving a white piece" game_two 0 4 false;
  make_movable_piece_test "White turn moving an empty space" game_one 6 6 false;
  make_movable_piece_test "Black turn moving an empty space" game_two 6 6 false;
]

(* Unit tests for function pawn_val in module Main *)
let pawn_val_tests = [
  make_pawn_val_test "White pawn moving forward 1 with nothing blocking it."
    pawn_val_board_white {x=0; y=6} {x=0; y=5} true;
  make_pawn_val_test "White pawn moving forward 1 with something blocking it."
    pawn_val_board_white {x=7; y=6} {x=7; y=5} false;
  make_pawn_val_test "White pawn moving forward 2 with nothing blocking it."
    pawn_val_board_white {x=0; y=6} {x=0; y=4} true;
  make_pawn_val_test "White pawn moving forward 2 with something blocking it."
    pawn_val_board_white {x=2; y=6} {x=2; y=4} false;
  make_pawn_val_test "White pawn legally taking a piece."
    pawn_val_board_white {x=0; y=6} {x=1; y=5} true;
  make_pawn_val_test "White pawn illegally trying to take a piece"
    pawn_val_board_white {x=7; y=6} {x=6; y=5} false;
  make_pawn_val_test "White pawn illegally moving diagonally."
    pawn_val_board_white {x=2; y=6} {x=3; y=5} false;
]

(* Unit tests for function rook_val in module Main *)
let rook_val_tests = [
  (** White rook move test. *)
  make_rook_val_test "White rook moving forward with nothin blocking."
    rook_game_w {x=6;y=6} {x = 6; y= 3} true;
  make_rook_val_test "White rook moving backward with nothin blocking."
    rook_game_w {x=6;y=6} {x = 6; y= 7} true;
  make_rook_val_test "White rook moving right with nothin blocking."
    rook_game_w {x=6;y=6} {x = 7; y= 6} true;
  make_rook_val_test "White prook moving left with nothin blocking."
    rook_game_w {x=6;y=6} {x = 4; y= 6} true;
  make_rook_val_test "White rook moving backward with nothin blocking."
    rook_game_w {x=6;y=6} {x = 6; y= 7} true;
  make_rook_val_test "White rook moving forward with something blocking."
    rook_game_w {x=1;y=6} {x = 1; y= 4} false;
  make_rook_val_test "White rook moving backward with something blocking."
    rook_game_w {x=1;y=6} {x = 1; y= 7} false;
  make_rook_val_test "White rook moving left with something blocking."
    rook_game_w {x=1;y=6} {x = 0; y= 6} false;
  make_rook_val_test "White rook moving right with something blocking."
    rook_game_w {x=1;y=6} {x = 3; y= 6} false;
  make_rook_val_test "White rook moving left taking a piece."
    rook_game_w {x=6;y=6} {x = 3; y= 6} true;
  (** Black rook move test*)
  make_rook_val_test "Black rook moving forward with nothin blocking."
    rook_game_b {x=6;y=6} {x = 6; y= 3} true;
  make_rook_val_test "Black rook moving backward with nothin blocking."
    rook_game_b {x=6;y=6} {x = 6; y= 7} true;
  make_rook_val_test "Black rook moving right with nothin blocking."
    rook_game_b {x=6;y=6} {x = 7; y= 6} true;
  make_rook_val_test "Black prook moving left with nothin blocking."
    rook_game_b {x=6;y=6} {x = 4; y= 6} true;
  make_rook_val_test "Black rook moving backward with nothin blocking."
    rook_game_b {x=6;y=6} {x = 6; y= 7} true;
  make_rook_val_test "Black rook moving forward with something blocking."
    rook_game_b {x=1;y=6} {x = 1; y= 4} false;
  make_rook_val_test "Black rook moving backward with something blocking."
    rook_game_b {x=1;y=6} {x = 1; y= 7} false;
  make_rook_val_test "Black rook moving left with something blocking."
    rook_game_b {x=1;y=6} {x = 0; y= 6} false;
  make_rook_val_test "Black rook moving right with something blocking."
    rook_game_b {x=1;y=6} {x = 3; y= 6} false;
  make_rook_val_test "Black rook moving left taking a piece."
    rook_game_b {x=6;y=6} {x = 3; y= 6} true;
]



(** [make_bishop_val_test name input_game input_ft input_sd] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [bishop_val input_game input_ft input_sd]. *)
let make_bishop_val_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (input_sd : location) 
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (bishop_val input_game input_ft input_sd)
               expected_output ~printer:string_of_bool)


(** Black and White boards used for bishop_val_tests.*)
let bishop_val_board_white = Array.make_matrix 8 8 Main.Empty
let bishop_val_board_black = Array.make_matrix 8 8 Main.Empty
let bishop_val_board_third = Array.make_matrix 8 8 Main.Empty
let _ =
  bishop_val_board_white.(2).(7) <- 
    Piece{col = White; act = Active; nam = Bishop "B"};
  bishop_val_board_white.(5).(7) <- 
    Piece{col = White; act = Active; nam = Bishop "B"};
  bishop_val_board_white.(6).(6) <- 
    Piece{col = White; act = Active; nam = Pawn "P"};
  bishop_val_board_white.(2).(4) <- 
    Piece{col = Black; act = Active; nam = Pawn "P"};

  bishop_val_board_black.(2).(0) <- 
    Piece{col = Black; act = Active; nam = Bishop "B"};
  bishop_val_board_black.(5).(0) <- 
    Piece{col = Black; act = Active; nam = Bishop "B"};
  bishop_val_board_black.(6).(1) <- 
    Piece{col = Black; act = Active; nam = Pawn "P"};
  bishop_val_board_black.(2).(3) <- 
    Piece{col = White; act = Active; nam = Pawn "P"};

  bishop_val_board_third.(0).(6) <-
    Piece{col = Black; act = Active; nam = Bishop "B"};
  bishop_val_board_third.(3).(3) <-
    Piece{col = White; act = Active; nam = Queen "Q"};
  ()

(** Black game used for bishop_val_tests.*)
let bishop_game_black = {board = bishop_val_board_black; 
                         turn= Black; 
                         cas_white= Nonactive;
                         cas_black=  Nonactive; 
                         king_white= (4,7);
                         king_black = (4,0);
                         en_passant= Nonactive;

                        }

(** White game used for bishop_val_tests.*)
let bishop_game_white = {board = bishop_val_board_white; 
                         turn= White; 
                         cas_white= Nonactive;
                         cas_black=  Nonactive; 
                         king_white= (4,7);
                         king_black = (4,0);
                         en_passant= Nonactive;

                        }

let bishop_game_third = {board = bishop_val_board_third; 
                         turn= Black; 
                         cas_white= Nonactive;
                         cas_black=  Nonactive; 
                         king_white= (4,7);
                         king_black = (4,0);
                         en_passant= Nonactive;

                        }

(* Unit tests for function bishop_val in module Main *)
let bishop_val_tests = [
  (** bishop_game_white tests *)
  make_bishop_val_test 
    "White left bishop moving diagnol 1 with nothing blocking it." 
    bishop_game_white {x=2; y=7} {x=1; y=6} true;
  make_bishop_val_test 
    "White right bishop moving diagnol 1 with nothing blocking it." 
    bishop_game_white {x=5; y=7} {x=4; y=6} true;
  make_bishop_val_test "White bishop moving diagnol 1 with pawn blocking it." 
    bishop_game_white {x=5; y=7} {x=6; y=6} false;
  make_bishop_val_test "White bishop moving diagnol 2 with nothing blocking it." 
    bishop_game_white {x=5; y=7} {x=3; y=5} true;
  make_bishop_val_test "White bishop moving across board with nothing blocking it." 
    bishop_game_white {x=2; y=7} {x=7; y=2} true;
  make_bishop_val_test "White bishop moving not in a diagnol." 
    bishop_game_white {x=2; y=7} {x=6; y=2} false;
  make_bishop_val_test 
    "White bishop moving backwards across board with nothing blocking it." 
    bishop_game_white {x=7; y=2} {x=3; y=6} true;
  make_bishop_val_test 
    "White bishop moving backwards across board trying to take White piece." 
    bishop_game_white {x=7; y=2} {x=2; y=7} false;
  make_bishop_val_test "White bishop moving taking a Black pawn." 
    bishop_game_white {x=5; y=7} {x=2; y=4} true;
  make_bishop_val_test "White bishop jumping over White pawn." 
    bishop_game_white {x=5; y=7} {x=7; y=5} false;

  (** bishop_game_black tests *)
  make_bishop_val_test 
    "Black left bishop moving diagnol 1 with nothing blocking it." 
    bishop_game_black {x=2; y=0} {x=1; y=1} true;
  make_bishop_val_test 
    "Black right bishop moving diagnol 1 with nothing blocking it." 
    bishop_game_black {x=5; y=0} {x=1; y=4} false;
  make_bishop_val_test "Black bishop moving diagnol 1 with pawn blocking it." 
    bishop_game_black {x=5; y=0} {x=1; y=6} false;
  make_bishop_val_test "Black bishop moving diagnol 2 with nothing blocking it." 
    bishop_game_black {x=5; y=0} {x=3; y=2} true;
  make_bishop_val_test 
    "Black bishop moving across board with nothing blocking it." 
    bishop_game_black {x=2; y=0} {x=7; y=5} true;
  make_bishop_val_test "Black bishop moving straight, not in a diagnol." 
    bishop_game_black {x=2; y=0} {x=2; y=1} false;
  make_bishop_val_test "Black bishop moving sideways, not in a diagnol." 
    bishop_game_black {x=2; y=0} {x=3; y=0} false;
  make_bishop_val_test 
    "Black bishop moving backwards across board with nothing blocking it." 
    bishop_game_black {x=7; y=5} {x=3; y=1} true;
  make_bishop_val_test "Black bishop moving taking a White pawn." 
    bishop_game_black {x=5; y=0} {x=2; y=3} true;

  make_bishop_val_test "White bishop jumping over White pawn." 
    bishop_game_third {x=0; y=6} {x=3; y=3} true;
]

(** [make_king_val_test name input_game input_ft input_sd] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [king_val input_game input_ft input_sd]. *)
let make_king_val_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (input_sd : location) 
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (king_val input_game input_ft input_sd)
               expected_output ~printer:string_of_bool)


(** Black and White boards used for king_val_tests.*)
let king_val_board_white = Array.make_matrix 8 8 Main.Empty
let king_val_board_black = Array.make_matrix 8 8 Main.Empty
let _ =
  king_val_board_white.(4).(7) <- 
    Piece{col = White; act = Active; nam = King "K"};
  king_val_board_white.(4).(6) <- 
    Piece{col = Black; act = Active; nam = Pawn "P"};
  king_val_board_white.(5).(6) <- 
    Piece{col = Black; act = Active; nam = Pawn "P"};
  king_val_board_white.(5).(7) <- 
    Piece{col = White; act = Active; nam = Pawn "P"};

  king_val_board_black.(4).(0) <- 
    Piece{col = Black; act = Active; nam = King "K"};
  king_val_board_black.(4).(1) <- 
    Piece{col = White; act = Active; nam = Pawn "P"};
  king_val_board_black.(3).(1) <- 
    Piece{col = White; act = Active; nam = Pawn "P"};
  king_val_board_black.(3).(0) <- 
    Piece{col = Black; act = Active; nam = Pawn "P"};

  ()

(** Black game used for bishop_val_tests.*)
let king_game_black = {board = king_val_board_black; 
                       turn= Black; 
                       cas_white= Nonactive;
                       cas_black=  Nonactive; 
                       king_white= (4,7);
                       king_black = (4,0);
                       en_passant= Nonactive;

                      }

(** White game used for bishop_val_tests.*)
let king_game_white = {board = king_val_board_white; 
                       turn= White; 
                       cas_white= Nonactive;
                       cas_black=  Nonactive; 
                       king_white= (4,7);
                       king_black = (4,0);
                       en_passant= Nonactive;

                      }


(* Unit tests for function king_val in module Main *)
let king_val_tests = [
  (** king_game_white tests *)
  make_king_val_test 
    "White king moving diagnol 1 with nothing blocking it." 
    king_game_white {x=4; y=7} {x=3; y=6} true;
  make_king_val_test 
    "White king moving sideways 1 with nothing blocking it." 
    king_game_white {x=4; y=7} {x=3; y=7} true;
  make_king_val_test 
    "White king moving 2 spaces." 
    king_game_white {x=4; y=7} {x=2; y=7} false;
  make_king_val_test 
    "White king moving forward 2 and piece in between it." 
    king_game_white {x=4; y=7} {x=2; y=7} false;
  make_king_val_test 
    "White king capturing Black piece moving forward 1 space." 
    king_game_white {x=4; y=7} {x=4; y=6} true;
  make_king_val_test 
    "White king capturing Black piece moving diagnol 1 space." 
    king_game_white {x=4; y=7} {x=5; y=6} true;
  make_king_val_test 
    "White king moving sideways 1 space with White piece there." 
    king_game_white {x=4; y=7} {x=5; y=7} false;

  (** king_game_black tests *)
  make_king_val_test 
    "Black king moving diagnol 1 with nothing blocking it." 
    king_game_black {x=4; y=0} {x=5; y=1} true;
  make_king_val_test 
    "Black king moving sideways 1 with nothing blocking it." 
    king_game_black {x=4; y=0} {x=5; y=0} true;
  make_king_val_test 
    "Black king moving 2 spaces." 
    king_game_black {x=4; y=0} {x=6; y=0} false;
  make_king_val_test 
    "Black king moving forward 2 and piece in between it." 
    king_game_black {x=4; y=0} {x=4; y=2} false;
  make_king_val_test 
    "Black king capturing White piece moving forward 1 space." 
    king_game_black {x=4; y=0} {x=4; y=1} true;
  make_king_val_test 
    "Black king capturing White piece moving diagnol 1 space." 
    king_game_black {x=4; y=0} {x=3; y=1} true;
  make_king_val_test 
    "Black king moving sideways 1 space with Black piece there." 
    king_game_black {x=4; y=0} {x=3; y=0} false;
]

(** [make_castling_king_test name input_game] constructs an OUnit 
    test named [name] that takes in a game and outputs a new game with 
    [castling_king input_game]. *)
let make_castling_king_test
    (name : string)
    (input_game : game)
    (expected_output : game): test =
  name >:: (fun _ -> assert_equal (castling_king input_game)
               expected_output)




(** Black and White boards used for castling king_tests.*)
let castling_king_board_white = Array.make_matrix 8 8 Main.Empty
let post_castling_king_board_white = Array.make_matrix 8 8 Main.Empty
let castling_king_board_black = Array.make_matrix 8 8 Main.Empty
let post_castling_king_board_black = Array.make_matrix 8 8 Main.Empty
let _ =
  castling_king_board_white.(4).(7) <- 
    Piece{col = White; act = Active; nam = King "K"};
  castling_king_board_white.(7).(7) <- 
    Piece{col = White; act = Active; nam = Rook "R"};

  post_castling_king_board_white.(6).(7) <- 
    Piece{col = White; act = Active; nam = King "K"};
  post_castling_king_board_white.(5).(7) <- 
    Piece{col = White; act = Active; nam = Rook "R"};

  castling_king_board_black.(4).(0) <- 
    Piece{col = Black; act = Active; nam = King "K"};
  castling_king_board_black.(7).(0) <- 
    Piece{col = Black; act = Active; nam = Rook "R"};

  post_castling_king_board_black.(6).(0) <- 
    Piece{col = Black; act = Active; nam = King "K"};
  post_castling_king_board_black.(5).(0) <- 
    Piece{col = Black; act = Active; nam = Rook "R"};

  ()

(** Black game used for castling_king_tests.*)
let castling_king_black = {board = castling_king_board_black; 
                           turn= Black; 
                           cas_white= Active;
                           cas_black=  Active; 
                           king_white= (4,7);
                           king_black = (4,0);
                           en_passant= Nonactive;
                          }

(** Black game used for post_castling_king_tests.*)
let post_castling_king_black = {board = post_castling_king_board_black; 
                                turn= Black; 
                                cas_white= Active;
                                cas_black=  Active; 
                                king_white= (4,7);
                                king_black = (4,0);
                                en_passant= Nonactive;
                               }

(** White game used for castling_king_tests.*)
let castling_king_white = {board = castling_king_board_white; 
                           turn= White; 
                           cas_white= Active;
                           cas_black=  Active; 
                           king_white= (4,7);
                           king_black = (4,0);
                           en_passant= Nonactive;

                          }

(** White game used for post_castling_king_tests.*)
let post_castling_king_white = {board = post_castling_king_board_white; 
                                turn= White; 
                                cas_white= Active;
                                cas_black=  Active; 
                                king_white= (4,7);
                                king_black = (4,0);
                                en_passant= Nonactive;

                               }

(** Castling king OUnit tests*)                      
let castling_king_tests = [
  (** Black castling king tests*)
  make_castling_king_test 
    "(Valid) Black King and Black Rook castling with no pieces in between" 
    castling_king_black post_castling_king_black ;

  (** White castling king tests*)
  make_castling_king_test 
    "(Valid) White King and White Rook castling with no pieces in between" 
    castling_king_white post_castling_king_white ;

]



(** [make_queen_val_test name input_game input_ft input_sd] constructs an OUnit 
    test named [name] that asserts the quality of [expected_output] with 
    [queen_val input_game input_ft input_sd]. *)
let make_queen_val_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (input_sd : location) 
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (queen_val input_game input_ft input_sd)
               expected_output ~printer:string_of_bool)



(** White and black boards used for queen_val_tests.*)
let queen_board_w = Array.make_matrix 8 8 Main.Empty
let _ = 
  queen_board_w.(2).(2) <- Piece{col = White; act = Active; nam = Queen "Q"};
  queen_board_w.(6).(6) <- Piece{col = White; act = Active; nam = Queen "Q"};
  queen_board_w.(1).(2) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(3).(2) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(2).(3) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(2).(1) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(1).(1) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(3).(3) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(3).(1) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(1).(3) <- Piece{col = White; act = Active; nam = Pawn "P"};
  queen_board_w.(4).(6) <- Piece{col = Black; act = Active; nam = Pawn "P"};

  ()

let queen_board_b = Array.make_matrix 8 8 Main.Empty
let _ = 
  queen_board_b.(2).(2) <- Piece{col = Black; act = Active; nam = Queen "Q"};
  queen_board_b.(6).(6) <- Piece{col = Black; act = Active; nam = Queen "Q"};
  queen_board_b.(1).(2) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(3).(2) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(2).(3) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(2).(1) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(1).(1) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(3).(3) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(3).(1) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(1).(3) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  queen_board_b.(4).(6) <- Piece{col = White; act = Active; nam = Pawn "P"};

  ()

(** White game used for queen_val_tests.*)
let queen_game_white = {board = queen_board_w; 
                        turn= White; 
                        cas_white= Nonactive;
                        cas_black=  Nonactive; 
                        king_white= (4,7);
                        king_black = (4,0);
                        en_passant= Nonactive;

                       }

(** Black game used for queen_val_tests.*)
let queen_game_black = {board = queen_board_b; 
                        turn=  Black; 
                        cas_white= Nonactive;
                        cas_black=  Nonactive; 
                        king_white= (4,7);
                        king_black = (4,0);
                        en_passant= Nonactive;

                       }

(* Unit tests for function king_val in module Main *)
let queen_val_tests = [
  (** White queen tests*)
  make_queen_val_test 
    "White queen moving diagonal (down-right) with nothing blocking." 
    queen_game_white {x=6; y=6} {x=7; y=7} true;
  make_queen_val_test 
    "White queen moving diagonal (up-left) with nothing blocking." 
    queen_game_white {x=6; y=6} {x=5; y=5} true;
  make_queen_val_test 
    "White queen moving diagonal (up-right) with nothing blocking." 
    queen_game_white {x=6; y=6} {x=7; y=5} true;
  make_queen_val_test 
    "White queen moving diagonal (down-left) with nothing blocking." 
    queen_game_white {x=6; y=6} {x=5; y=7} true;
  make_queen_val_test 
    "White queen moving left with nothing blocking." 
    queen_game_white {x=6; y=6} {x=5; y=6} true;
  make_queen_val_test 
    "White queen moving right with nothing blocking." 
    queen_game_white {x=6; y=6} {x=7; y=6} true;
  make_queen_val_test 
    "White queen moving up with nothing blocking." 
    queen_game_white {x=6; y=6} {x=6; y=5} true;
  make_queen_val_test 
    "White queen moving down with nothing blocking." 
    queen_game_white {x=6; y=6} {x=6; y=7} true;
  make_queen_val_test 
    "White queen moving down with something blocking." 
    queen_game_white {x=2; y=2} {x=2; y=4} false;
  make_queen_val_test 
    "White queen moving up with something blocking." 
    queen_game_white {x=2; y=2} {x=2; y=0} false;
  make_queen_val_test 
    "White queen moving left with something blocking." 
    queen_game_white {x=2; y=2} {x=0; y=2} false;
  make_queen_val_test 
    "White queen moving right with something blocking." 
    queen_game_white {x=2; y=2} {x=4; y=2} false;
  make_queen_val_test 
    "White queen moving diagonal (up-left) with something blocking." 
    queen_game_white {x=2; y=2} {x=0; y=0} false;
  make_queen_val_test 
    "White queen moving diagonal (up-right) with something blocking." 
    queen_game_white {x=2; y=2} {x=0; y=4} false;
  make_queen_val_test 
    "White queen taking a black piece." 
    queen_game_white {x=6; y=6} {x=4; y=6} true;
  (** Black queen tests*)
  make_queen_val_test 
    "Black queen moving diagonal (down-right) with nothing blocking." 
    queen_game_black {x=6; y=6} {x=7; y=7} true;
  make_queen_val_test 
    "Black queen moving diagonal (up-left) with nothing blocking." 
    queen_game_black {x=6; y=6} {x=5; y=5} true;
  make_queen_val_test 
    "Black queen moving diagonal (up-right) with nothing blocking." 
    queen_game_black {x=6; y=6} {x=7; y=5} true;
  make_queen_val_test 
    "White queen moving diagonal (down-left) with nothing blocking." 
    queen_game_black {x=6; y=6} {x=5; y=7} true;
  make_queen_val_test 
    "Black queen moving left with nothing blocking." 
    queen_game_black {x=6; y=6} {x=5; y=6} true;
  make_queen_val_test 
    "Black queen moving right with nothing blocking." 
    queen_game_black {x=6; y=6} {x=7; y=6} true;
  make_queen_val_test 
    "Black queen moving up with nothing blocking." 
    queen_game_black {x=6; y=6} {x=6; y=5} true;
  make_queen_val_test 
    "White queen moving down with nothing blocking." 
    queen_game_black {x=6; y=6} {x=6; y=7} true;
  make_queen_val_test 
    "Black queen moving down with something blocking." 
    queen_game_black {x=2; y=2} {x=2; y=4} false;
  make_queen_val_test 
    "Black queen moving up with something blocking." 
    queen_game_black {x=2; y=2} {x=2; y=0} false;
  make_queen_val_test 
    "Black queen moving left with something blocking." 
    queen_game_black {x=2; y=2} {x=0; y=2} false;
  make_queen_val_test 
    "Black queen moving right with something blocking." 
    queen_game_black {x=2; y=2} {x=4; y=2} false;
  make_queen_val_test 
    "Black queen moving diagonal (up-left) with something blocking." 
    queen_game_black {x=2; y=2} {x=0; y=0} false;
  make_queen_val_test 
    "Black queen moving diagonal (up-right) with something blocking." 
    queen_game_black {x=2; y=2} {x=0; y=4} false;
  make_queen_val_test 
    "Black queen taking a white piece." 
    queen_game_black {x=6; y=6} {x=4; y=6} true;
]



(* Creating boards for check
   let check_board_w = Array.make_matrix 8 8 Main.Empty
   let non_check_board_w = Array.make_matrix 8 8 Main.Empty
   let _ = 

   check_board_w.(4).(4) <- Piece{col = White; act = Active; nam = King "K"};
   check_board_w.(6).(6) <- Piece{col = Black; act = Active; nam = Queen "Q"};

   non_check_board_w.(4).(4) <- Piece{col = White; act = Active; nam = King "K"};
   non_check_board_w.(5).(6) <-Piece{col = Black; act = Active; nam = Queen "Q"};


   () *)


(* * White game used for check_tests.
   let check_game_white = {board = check_board_w; 
                        turn= White; 
                        cas_white= Nonactive;
                        cas_black=  Nonactive; 
                        king_white= (4,4);
                        king_black = (4,0);
                        en_passant= Nonactive;

                       }

   (** White game used for non_check_tests.*)
   let non_check_game_white = {board = non_check_board_w; 
                            turn= White; 
                            cas_white= Nonactive;
                            cas_black=  Nonactive; 
                            king_white= (4,4);
                            king_black = (4,0);
                            en_passant= Nonactive;

                           } *)

(* 
let check_tests = [
  (* White check tests*)
  make_check_test "White king in check." 
    check_game_white {x=4; y=4} White true;

  (* White non-check tests*)
  make_check_test "White king not in check." 
    non_check_game_white {x=4; y=4} White false;
  *)

(** [make_check_test name input_game input_ft expected_output] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output] with 
    [check input_game input_ft]. *)
let make_check_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (expected_output : bool): test =
  name >:: (fun _ -> assert_equal (check input_game input_ft) 
               expected_output ~printer:string_of_bool)


(* Board for check_tests. *)
let check_board = Array.make_matrix 8 8 Main.Empty
let check_board2 = Array.make_matrix 8 8 Main.Empty
let _ =
  check_board.(3).(3) <- Piece{col = White; act = Active; nam = Queen "Q"};
  check_board.(0).(6) <- Piece{col = Black; act = Active; nam = Bishop "B"};
  check_board.(3).(4) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  check_board.(5).(4) <- Piece{col = White; act = Active; nam = Knight "k"};
  check_board.(5).(0) <- Piece{col = White; act = Active; nam = Rook "R"};
  check_board.(6).(6) <- Piece{col = White; act = Active; nam = Knight "k"};
  check_board.(5).(5) <- Piece{col = Black; act = Active; nam = Bishop "B"};
  check_board.(4).(4) <- Piece{col = Black; act = Active; nam = Pawn "P"};
  check_board.(2).(7) <- Piece{col = Black; act = Active; nam = King "K"};
  check_board.(0).(0) <- Piece{col = White; act = Active; nam = King "K"}; 

  check_board2.(4).(0) <- Piece{col = Black; act = Active; nam = King "K"}; 
  check_board2.(6).(3) <- Piece{col = White; act = Active; nam = Bishop "B"}; 

  ()


(* Game for check_tests. *)
let check_game = {
  board = check_board;
  turn = White;
  cas_white = Nonactive;
  cas_black = Nonactive;
  king_white = (0,0);
  king_black = (2,7);
  en_passant = Active;
}

let check_game2 = {
  board = check_board2;
  turn = Black;
  cas_white = Nonactive;
  cas_black = Nonactive;
  king_white = (0,0);
  king_black = (4,0);
  en_passant = Active;
}

(* Tests for the function check *)
let check_tests = [
  make_check_test {|White queen that is in "check"|} check_game {x = 3; y = 3}
    true;
  make_check_test {|White knight that is not in "check"|} check_game 
    {x = 5; y = 4} false; 

  make_check_test {|Black king that is in "check"|} check_game2 
    {x = 4; y = 1} true; 
]


(** [make_new_check_test name input_game input_ft expected_output] constructs 
    an OUnit test named [name] that asserts the quality of [expected_output] 
    with [new_check input_game input_ft]. *)
let make_new_check_test
    (name : string)
    (input_game : game)
    (input_ft : location)
    (expected_output : location list) : test =
  name >:: (fun _ -> assert_equal ((new_check input_game input_ft))
               expected_output ~printer:string_of_loc_list)


(* Boards and games created for testing the function new_check *)
let new_check_board_one = Array.make_matrix 8 8 Main.Empty
let _ =
  new_check_board_one.(4).(3) <- 
    Piece {col = Black; act = Active; nam = Pawn "P"};
  new_check_board_one.(3).(3) <- 
    Piece {col = White; act = Active; nam = Rook "R"};
  new_check_board_one.(1).(6) <- 
    Piece {col = White; act = Active; nam = Queen "Q"};
  ()

let new_check_game_one = {
  board = new_check_board_one;
  turn = Black;
  cas_white = Nonactive;
  cas_black = Nonactive;
  king_white = (3,0);
  king_black = (3,8);
  en_passant = Active;
}

(* Tests for function new_check *)
let new_check_tests = [
  make_new_check_test "Black pawn in check in two places." new_check_game_one
    {x = 4; y = 3} [{x=1;y=6} ; {x=3;y=3}];
  make_new_check_test "Space in check from Queen in one place" new_check_game_one
    {x = 0; y = 7} [{x=1;y=6}];
  make_new_check_test "Corner space not in check" new_check_game_one
    {x = 0; y = 0} [];
]

(** Run all test suites *)
let tests = 
  "Test Suite for modules main, game, and command"  >::: List.flatten [
    on_board_tests;
    on_piece_tests;
    movable_piece_tests;
    pawn_val_tests;
    rook_val_tests;
    bishop_val_tests;
    king_val_tests;
    queen_val_tests;
    new_check_tests;

  ]

let _ = run_test_tt_main tests


