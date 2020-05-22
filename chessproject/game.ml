open Main
open Command 

(** [play game] is the new game every time the player inputs a new command 
    (quit,move, or castle). Outputs whose turn it is and if the move made is 
    valid or not and prompts for new move. 
    Requires: [game] is a valid game. 
*)
let rec play (game: game) : game  = 
  format_board game.board;
  print_endline ("It is " ^ (string_of_color game) ^"'s turn.");
  print_string  "> ";
  let input = read_line() in 
  try match parse input with
    | Quit -> (print_endline "Game Over" ; exit 0)
    | exception Empty -> ANSITerminal.(print_string [red] ("Empty ")); 
      play game
    | exception Malformed -> ANSITerminal.(print_string [red] ("Malformed ")); 
      play game
    | Move lst -> 
      (match make_location lst with 
       | Legal (s,v) -> play (make_move game s v)
       | Illegal  -> play game 
      )
    | Castle t -> (match List.hd t with
        | "queen" -> play (castling_queen game) 
        | "king" -> play (castling_king game)  
        | _ -> failwith "illegal move" ; 
      )
  with _ -> ANSITerminal.(print_string [red] ("Ilegal Input \n")); 
    play game

(* * [play_game f] starts the adventure in file [f].
   let play_game f : unit =
   try
    play start
   with Sys_error s -> ANSITerminal.(print_string [red] ("Invalid ]")) *)


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  move  ANSITerminal.(print_string [red]
                        "\n\nWelcome to our chess game!\n");
  print_endline "Please enter your move in these formats: \n
  move A2,A3 \n
  castle king \n
  castle queen \n
  quit \n
  Note: Castle king is a king side castle 
  and castle queen is a queen side castle. Valid input for move is an uppercase 
  letter followed by an integer number 1-8, uppercase letter followed by an 
  integer number 1-8.";  
  print_string  "> ";
  play start

(** [start_it] executes the game engine. *)
let start_it = main ()

