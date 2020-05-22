open Main 

type object_phrase = string list

type command = 
  | Move of object_phrase
  | Quit
  | Castle of object_phrase

exception Empty

exception Malformed 


(** [funct strng] gives a boolean expression
    Requires: strng is a string . *)
let funct strng = 
  strng <> " " && strng <> ""


(** [remove_space string_lst] gives a new string list of the filtered string 
    list. Requires: string_list is a string list. *)
let remove_space string_lst =
  List.filter funct string_lst


(** [parse_helpr str] is helper that validates if string is a command 
    Requires: str is a string.
    Raises: [Empty] if [str] is the empty string or contains only spaces. 
    Raises: [Malformed] if the command is malformed. *)
let rec parse_helper str = 

  let com = String.split_on_char ' ' str in
  let new_com = remove_space com in 
  match new_com with 
  | [] -> raise(Empty)
  | h :: t -> if h = "move" then Move t else if h = "castle" then Castle t else
      (if h = "quit" then Quit else raise (Malformed)) 


(** [parse_position str] gives the {x,y} location that is inputed by the user. 
    Requires: [str] is a string. 
*)
let parse_position str =
  let a = 
    match String.sub str 0 1 with 
    | "A" -> 0 
    | "B" -> 1 
    | "C" -> 2 
    | "D" -> 3
    | "E" -> 4
    | "F" -> 5
    | "G" -> 6
    | "H" -> 7 
    | _ -> raise Malformed 
  in  {x = a; y = (8 - int_of_string (String.sub str 1 1))}


(** [string_list_to_string str_l] is the concatanated string version of the 
    inputted string list. 
    Requires: [str_l] is a string list. 
*)
let string_list_to_string str_l: string = 
  String.concat "" str_l


let make_location str =  
  let temp = string_list_to_string str in
  let inputs = (String.split_on_char ',' temp) in
  Legal (parse_position (List.hd inputs), 
         parse_position (List.hd (List.rev inputs)))


let parse str = 
  parse_helper str