#use "pc.ml";;
#use "reader.ml";;
open PC;;

(* Auxiliary methods *)
let starWhiteSpaces =
  let starSpaces = star (const (fun ch -> (int_of_char ch) < 33)) in
  pack starSpaces (fun spaces -> Nil);;

let plusWhiteSpaces =
  let starSpaces = plus (const (fun ch -> (int_of_char ch) < 33)) in
  pack starSpaces (fun spaces -> Nil);;

let whiteSpaces =
  let _whiteSpaces = star whiteSpace in
  pack _whiteSpaces (fun x -> list_to_string x);;
test_string whiteSpaces "    ";;

let leftParenParser =
  let _leftParenParser = char '(' in
  pack _leftParenParser (fun ch -> Char.escaped ch);;

let rightParenParser =
  let _rightParenParser = char ')' in
  pack _rightParenParser (fun ch -> Char.escaped ch);;

let listParser =
  let sexprs = star sexprParser in
  caten_list [leftParenParser;	  
	      sexprs;
	      rightParenParser];;

let whiteSpace = (const (fun ch -> (int_of_char ch) < 33));;

let lineComment =
  let _semicolon = char ';' in
  let _comment = star (const (fun ch -> int_of_char (ch) != 10)) in
  let _newLine =pack (char '\n') (fun ch -> [ch]) in
  let _line_comment = caten _semicolon _comment in
  let endOfComment =  disj _newLine  nt_end_of_input in
  let _full_comment = caten _line_comment endOfComment in
  pack _full_comment (fun comment -> Nil);;                
                                

let hashtagParser = char '#';;

let char_to_int = (fun ch -> ((int_of_char ch) - (int_of_char '0')));;

let digit = range '0' '9';;

let reduce = (fun f base list -> List.fold_left (fun acc cur ->
  f acc cur) base list);;

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

let hexDigit =
  let _hexSmallLetters = range 'a' 'f' in
  let _hexCapsLetters = range 'A' 'F' in
  disj_list [digit; _hexSmallLetters; _hexCapsLetters];;

let hexChar = plus hexDigit;;

let hexCharParser =
  let _hexCharParser = caten (char_ci 'x') hexChar in
  pack _hexCharParser (fun x -> (char_of_int (int_of_string ("0x"^(list_to_string (snd x))))));;

let charParser =
  let charAsList =  disj_list
    [(caten charPrefixParser hexCharParser); 
    (caten charPrefixParser namedCharParser);
    (caten charPrefixParser visibleSimpleCharPrefix)] in
  pack charAsList (fun l -> Char (snd l));;
(* end char parser *)


(*Symbol*)
let symbolParser =
  let digit = range '0' '9' in
  let lowerCase = range 'a' 'z' in
  let upperCase = range 'A' 'Z' in
  let _upperCase = pack upperCase (fun x -> Char.lowercase_ascii x) in
  let punctuation = one_of "!$^*-_=+<>/?:" in 
  let _symbol_ = plus (disj_list [digit ; lowerCase ; _upperCase ; punctuation]) in
  pack _symbol_ (fun x -> Symbol (list_to_string  x));;

(*End Symbol*)


(*String*)
let quoute = char '\"';;

let stringLiteralCharParser = const (fun ch -> (int_of_char ch) != 92 && (int_of_char ch) != 34);;

let stringHexCharParser =
  pack (caten (char '\\') hexCharParser) (fun l -> snd l) ;;

let stringMetaCharParser =
   disj_list[
      pack (word "\\r") (fun ch -> char_of_int 13);
      pack (word "\\n") (fun ch -> char_of_int 10);
      pack (word "\\\"") (fun ch -> char_of_int 34);
      pack (word "\\f") (fun ch -> char_of_int 12);
      pack (word "\\t") (fun ch -> char_of_int 9);
      pack (word "\\\\") (fun ch -> char_of_int 92)
     ];;

let stringText = disj_list [stringLiteralCharParser;
                            stringMetaCharParser;
                           stringHexCharParser
                   ];;

let stringParser =
  let _quoute = quoute in
  let _stringText = pack (star stringText) (fun (str) -> list_to_string (str))  in
  pack (caten _quoute (caten _stringText  _quoute)) (fun l -> String (fst(snd(l))));;
(*End String*)

 


(* NUMBERS *)
let natural = 
  let _natural = plus digit in
  pack _natural (fun n -> (int_of_string (list_to_string n)));;

let signs =
  let _plusSign = char '+' in
  let _minusSign = char '-' in
  disj _plusSign _minusSign;;

let naturalString =
  let _natural = plus digit in
  pack _natural (fun x -> list_to_string x);;

let dotChar =
  let _dotChar = (char '.') in
  pack _dotChar (fun x -> Char.escaped x);;


(* Integer *)
let signedIntegerParser =
  let _signedInteger = caten signs natural in
  pack _signedInteger (fun x -> if (fst x) = '-' then (-1 * (snd x)) else snd x);;

let integerParser =
  let _integerParser = disj signedIntegerParser natural in
  pack _integerParser (fun x -> Number (Int x));;


(*Hex Integer *)
let hexPrefix =
  let _hexPrefix = caten hashtagParser (char_ci 'x') in
  pack _hexPrefix (fun x -> "");;

let hexDigitStar = star hexDigit;;

let hexNatural =
  let _hexNatural = plus hexDigit in
  pack _hexNatural (fun x -> (int_of_string ("0x"^(list_to_string x))));;

let signedHexNaturalParser =
  let _signedHexNaturalParser = caten signs hexNatural in
  pack _signedHexNaturalParser (fun x -> if (fst x) = '-' then (-1 * (snd x)) else snd x);;

let hexIntegerParser =
  let _hexIntegerParser =  caten hexPrefix (disj signedHexNaturalParser hexNatural) in
  pack _hexIntegerParser (fun x -> Number (Int (snd x)));;


(* Float *)
let unSignedFloatParser =
  let _floatParser = caten_list [naturalString; dotChar; naturalString] in
  pack _floatParser (fun list -> float_of_string (reduce (fun x y -> x^y) "" list));;

let signsToString =
  let _plusSign = char '+' in
  let _minusSign = char '-' in
  let _signsToString = disj _plusSign _minusSign in
  pack _signsToString (fun ch -> Char.escaped ch);;

let signedFloatParser =
  let _signedFloatParser = caten_list [signsToString; naturalString; dotChar; naturalString] in
  pack _signedFloatParser (fun list -> float_of_string (reduce (fun x y -> x^y) "" list));;

let floatParser =
  let _floatParser = disj signedFloatParser unSignedFloatParser in
  pack _floatParser (fun f -> Number (Float f));;


(* Hex Float *)
let hexDigitStarToString =
  let _hexDigitStarToString = star hexDigit in
  pack _hexDigitStarToString (fun list -> list_to_string list);;

let hexNaturalToString =
  let _hexNatural = plus hexDigit in
  pack _hexNatural (fun x -> ("0x"^(list_to_string x)));;


let unSignedHexFloatParser =
  let _floatParser = caten_list [hexPrefix; hexNaturalToString; dotChar; hexDigitStarToString] in
  pack _floatParser (fun list -> float_of_string (reduce (fun x y -> x^y) "" list));;

let signedHexFloatParser =
  let _signedHexFloatParser = caten_list [hexPrefix; signsToString; hexNaturalToString; dotChar; hexDigitStarToString] in
  pack _signedHexFloatParser (fun list -> float_of_string (reduce (fun x y -> x^y) "" list));;

let hexFloatParser =
  let _hexFloatParser = disj signedFloatParser unSignedHexFloatParser in
  pack _hexFloatParser (fun f -> Number (Float f));;


(* numbers *)

let natural = 
  let _natural = plus digit in
  pack _natural (fun n -> (int_of_string (list_to_string n)));;


let signs =
  let _plusSign = char '+' in
  let _minusSign = char '-' in
  disj _plusSign _minusSign;;


let numberParser =
  let _numberParser = disj_list [hexFloatParser; floatParser; integerParser; hexIntegerParser] in
  pack _numberParser (fun x -> x);; 
(*end number*)


(*TODO : chnage lineComment to comment for sexpr comment addition *)
let spacesAndComments =
  let toSkip = caten plusWhiteSpaces (star lineComment) in
  pack toSkip (fun skip -> Nil);;

let commentAndSpaces =
  let _comment = plus lineComment in
  let toSkip = caten _comment starWhiteSpaces in
  pack toSkip (fun skip -> Nil);;
                     

let skipSpacesAndComments =
  let typeOfSkip = disj spacesAndComments commentAndSpaces in
  pack typeOfSkip (fun skip -> Nil);;

let skip =
  let toSkip = star skipSpacesAndComments in
  pack toSkip (fun skil -> Nil);;

let sexprParser =
  disj_list [booleanParser;
             charParser;
             numberParser;
             stringParser;
             symbolParser];;


let sexprWithSpacesAndCommentsParser =
  let commentsAndSpaces = caten skip (caten sexprParser skip) in
  pack commentsAndSpaces (fun exp -> fst (snd exp));;

