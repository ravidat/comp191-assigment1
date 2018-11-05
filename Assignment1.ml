#use "pc.ml";;
#use "reader.ml";;
open PC;;

(*service function *)

let spaces = star (char ' ');;

let hashtagParser = char ('#');;
(* end service function *)

(*boolean function *)
let trueParser =
  let _t_ = char_ci 't' in
  let _hashT_ = caten hashtagParser _t_ in
  pack _hashT_ (fun x -> true);;

let falseParser =
  let _f_ = char_ci 'f' in
  let _hashF_ = caten hashtagParser _f_ in
  pack _hashF_ (fun x -> false);;

let booleanParser = disj falseParser trueParser;; 
(* end boolean function *)


(* char parser *)
let charPrefixParser =
  let backslashParser = char '\\' in
  let _charPrefixParser =   caten hashtagParser backslashParser in
  pack _charPrefixParser (fun x  -> '#');;

let visibleSimpleCharPrefix =
  let simpleCharParser = const (fun ch -> ch > ' ') in
  pack simpleCharParser (fun x -> x);;


let newLineParser =
  let _newLineParser = word_ci "newline" in
  pack _newLineParser (fun nch -> Char.chr(10));;

let returnParser =
  let _returnParser  = word_ci "return" in
  pack _returnParser (fun nch -> Char.chr(13));;
 
let tabParser =
  let _tabParser = word_ci "tab" in
  pack _tabParser (fun nch -> Char.chr(9));;

let pageParser =
  let _pageParser = word_ci "page" in
  pack _pageParser (fun nch -> Char.chr(12));;

let nulParser =
  let _nulParser = word_ci "nul" in
  pack _nulParser (fun nch -> Char.chr(0));;

let spaceParser =
  let _spaceParser = word_ci "space" in
  pack _spaceParser (fun nch -> Char.chr(32));;

let namedCharParser = disj_list [returnParser; newLineParser; tabParser; pageParser; nulParser; spaceParser];;

let charParser =
  let charAsList =  disj_list[caten charPrefixParser namedCharParser ;
                              caten charPrefixParser visibleSimpleCharPrefix ] in
  pack charAsList (fun l -> snd l);;
(* end char parser *)
let xHexChar =
  let _x_ = char_ci 'x' in
    pack _x_ (fun x -> Scan.sscanf x "%x%!" fun(x -> x));;



test_string xHexChar "xa";; 


