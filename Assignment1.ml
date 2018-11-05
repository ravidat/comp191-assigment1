#use "pc.ml";;
#use "reader.ml";;
open PC;;

let spaces = star (char ' ');;

let hashtagParser = char ('#');;

let trueParser =
  let _t_ = char_ci 't' in
  let _hashT_ = caten hashtagParser _t_ in
  pack _hashT_ (fun x -> true);;

let falseParser =
  let _f_ = char_ci 'f' in
  let _hashF_ = caten hashtagParser _f_ in
  pack _hashF_ (fun x -> false);;

let booleanParser = disj falseParser trueParser;; 

test_string booleanParser "#t";;

let charPrefixParser =
  let backslashParser = char '\\' in
  caten hashtagParser backslashParser;; 

let visibleSimpleCharPrefix = const (fun ch -> ch > ' ');;

(*
let charParser = disj (caten charPrefixParser visibleSimpleCharPrefix) namedCharParser;;

let namedCharParser = disj
  newLineParser
  returnParser
  tabParser
  pageParser
  nulParser
  spaceParser;;
*)

let newLineParser =
  let _newLineParser = word_ci "newline" in
  pack _newLineParser (fun nch -> '\n');;

let returnParser=
  let _returnParser  = word_ci "return" in
  pack _returnParser (fun nch -> '\r');;
  
let tabParser =
  let _tabParser = word_ci "tab" in
  pack _tabParser (fun nch -> '\t');;

let pageParser =
  let _pageParser = word_ci "page" in
  pack _pageParser (fun nch -> '\p');;

let nulParser =
  let _nulParser = word_ci "nul" in
  pack _nulParser (fun nch -> '\0');;

let spaceParser =
  let _spaceParser = word_ci "space" in
  pack _spaceParser (fun nch -> '\0');;


test_string newLineParser "newline";; 

