let hexToDec2 = (fun x -> Scanf.sscanf x "%x" (fun x -> x));;


let hexToDec = (fun x -> int_of_string ("0x"^x));;

hexToDec2 "A";;

(*
string_of_int 345;;
Printf.sprintf "%X" 252;;
*)
