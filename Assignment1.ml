#use "pc.ml";;
#use "reader.ml";;
open PC;;

(* Auxiliary methods *)
(***********************************************************************************)
let spaces = star (char ' ');;

let hashtagParser = char '#';;

let char_to_int = (fun ch -> ((int_of_char ch) - (int_of_char '0')));;

(*
let valueOfHexList = (fun hexList -> List.fold_left (fun acc cur ->
  acc * 16 + cur) 0  hexList);; 
*)

let hexCharToDecVal = (fun x -> List.map (fun y -> (hexLetterValue y)) x);; 

let hexToDec = (fun x -> int_of_string ("0x"^x));;

(* end Auxiliary function *)

(*boolean *)
let trueParser =
  let _t_ = char_ci 't' in
  let _hashT_ = caten hashtagParser _t_ in
  pack _hashT_ (fun x -> Bool true);;

let falseParser =
  let _f_ = char_ci 'f' in
  let _hashF_ = caten hashtagParser _f_ in
  pack _hashF_ (fun x -> Bool false);;

let booleanParser = disj falseParser trueParser;; 
(* end boolean function *)

(* Boolean test () *)

(*
test_string booleanParser "#t";;
*)


(******************************************************************************************************)
(* char parser *)
let charPrefixParser =
  let backslashParser = char '\\' in
  let _charPrefixParser =   caten hashtagParser backslashParser in
  pack _charPrefixParser (fun x -> "");;

let visibleSimpleCharPrefix =
  let simpleCharParser = const (fun ch -> ch > ' ') in
  pack simpleCharParser (fun x -> x);;

let namedCharParser =
  let newLineParser =
    let _newLineParser = word_ci "newline" in
    pack _newLineParser (fun nch -> Char.chr(10)) in
  
  let returnParser =
    let _returnParser  = word_ci "return" in
    pack _returnParser (fun nch -> Char.chr(13)) in
 
  let tabParser =
    let _tabParser = word_ci "tab" in
    pack _tabParser (fun nch -> Char.chr(9)) in

  let pageParser =
    let _pageParser = word_ci "page" in
    pack _pageParser (fun nch -> Char.chr(12)) in

  let nulParser =
    let _nulParser = word_ci "nul" in
    pack _nulParser (fun nch -> Char.chr(0)) in

  let spaceParser =
    let _spaceParser = word_ci "space" in
    pack _spaceParser (fun nch -> Char.chr(32)) in

   disj_list [returnParser; newLineParser; tabParser; pageParser; nulParser; spaceParser];;

let charParser =
  let charAsList =  disj
    (caten charPrefixParser namedCharParser)
    (caten charPrefixParser visibleSimpleCharPrefix) in
  pack charAsList (fun l -> Char (snd l));;
(* end char parser *)

(*
test_string charParser "#\\a"
*)

(* we need to flatten the list and then use list to string and evaluate easily *)


(******************************************** Hex TODO again *************************************************)
(*
let hexCharParser =  
  let _xPrefix = char_ci 'x' in
  let _hexCalc = (fun str -> value_of_hexList (string_to_list str))
  pack _hexChar (fun x -> x);;

test_string hexCharParser "x3";;



let hexParser =  
  let _zeroPrefix = char '0' in
  let _xPrefix = char_ci 'x' in
  let _hexNumbers = range '0' '9' in
  let _hexSmallLetters = range 'a' 'f' in
  let _hexCapsLetters = range 'A' 'F' in
  let _hexDigitsWrapper = disj_list [_hexNumbers; _hexSmallLetters; _hexCapsLetters] in 

  (* TODO add star *)
  let _hexDigitsWrapperStar = star (_hexDigitsWrapper) in 

  let _wrapper = caten_list [_zeroPrefix; _xPrefix; _hexDigitsWrapper] in
  pack _wrapper (fun x ->  x);;
*)

(*
test_string hexParser "0xB";;
*)





(*

let toDecList = (fun x -> List.map (fun cur -> if cur >= '0' && cur <= '9' then cur else hexLetterValue cur) x);;

toDecList ['F'; 'E'];;
*)

(************************************************ end of hex ***************************************************)


(***********************************************************************************************)
(*hex parsers *)
let hexPrefix =
  let _hexPrefix = caten hashtagParser (char_ci 'x') in
  pack _hexPrefix (fun x -> snd x);;

let hexDigit =
  let _hexSmallLetters = range 'a' 'f' in
  let _hexCapsLetters = range 'A' 'F' in
  disj_list [digit; _hexSmallLetters; _hexCapsLetters];;

let hexDigitStar = star hexDigit;;

let hexNatural =
  let _hexNatural = plus hexDigit in
  pack _hexNatural (fun x -> (int_of_string ("0x"^(list_to_string x))));;

test_string hexNatural "a";;


let hexCharParser =
  let _hexChar = plus hexDigit in
  let _hexParser = caten hexPrefix _hexChar in
  pack _hexParser (fun x -> Char (char_of_int (int_of_string ("0x"^(list_to_string (snd x))))));;


test_string hexCharParser "#x31";;

(* numbers *)
let digit = range '0' '9';;

let natural = 
  let _natural = plus digit in
  pack _natural (fun n -> (int_of_string (list_to_string n)));;


let signedIntegerParser =
  let _plusSign = char '+' in
  let _minusSign = char '-' in
  let _signs = disj _plusSign _minusSign in
  let _signedInteger = caten _signs natural in
  pack _signedInteger (fun x -> if (fst x) = '-' then (-1 * (snd x)) else snd x);;

let integerParser = disj signedIntegerParser natural;;

test_string integerParser "4";
