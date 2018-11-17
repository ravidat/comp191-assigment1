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

let leftParenParser =
  let _leftParenParser = char '(' in
  pack _leftParenParser (fun ch -> Char.escaped ch);;

let rightParenParser =
  let _rightParenParser = char ')' in
  pack _rightParenParser (fun ch -> Char.escaped ch);;

let whiteSpace =
  let _whiteSpacesParser = const (fun ch -> (int_of_char ch) < 33) in
  pack _whiteSpacesParser (fun l -> Nil);;

let leftParen = word "(";;

let rightParen = word ")";;

let squareLeftParen = word "[";;

let squareRightParen = word "]";;

let dot = word "." ;;

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
  let _upperCase = pack upperCase (fun x -> Char.lowercase x) in
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



(************************************************************************************************)
(* scientific notation *)

let evaluateScientificUnSigned list =
  let num1 = float_of_string (fst list) in
  let _e = fst (snd list) in
  let num2 = float_of_string  (snd (snd list)) in
  (num1 *. (10. ** num2));;

let scientificUnSigneNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString (caten _eStringPack  naturalString)) in
  pack _scientificNumberParser (fun x -> evaluateScientificUnSigned x);;


let evaluateScientificPlus list =
  let num1 = float_of_string (fst list) in
  let _e = fst (snd list) in
  let _sign = fst (snd (snd list)) in
  let num2 = float_of_string (snd (snd (snd list))) in
  (num1 *. (10. ** num2));;

let scientificPlusNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _plusSign = char '+' in
  let _plusSignPack = pack _plusSign (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString (caten _eStringPack (caten _plusSignPack naturalString))) in
  pack _scientificNumberParser (fun x -> evaluateScientificPlus x);;


let evaluateScientificMinus list =
  let num1 = float_of_string (fst list) in
  let _e = fst (snd list) in
  let _sign = fst (snd (snd list)) in
  let num2 = float_of_string (snd (snd (snd list))) in
  (num1 /. (10. ** num2));;


let scientificMinusNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _minusSign = char '-' in
  let _minusSignPack = pack _minusSign (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString (caten _eStringPack (caten _minusSignPack naturalString))) in
  pack _scientificNumberParser (fun x -> evaluateScientificMinus x);;

let scientificIntegerParser = disj scientificMinusNumberParser
  (disj scientificUnSigneNumberParser scientificPlusNumberParser);;


(* float scientific *)

let evaluateScientificFloatUnSigned list =
  let num1 = float_of_string (fst list) in
  let _dot = fst (snd list) in
  let num1_frac = (fst (snd (snd list))) in
  let _e = fst (snd (snd (snd list))) in
  let num2 = float_of_string (snd (snd (snd (snd list)))) in
  (num1  *. (10. ** num2)) +. (float_of_string ("0."^(num1_frac))  *. (10. ** num2)) ;;

let scientificFloatUnSignedNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _dot = char '.' in
  let _dotPack = pack _dot (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString
				   (caten _dotPack
				      (caten naturalString
					 (caten _eStringPack naturalString)))) in
  pack _scientificNumberParser (fun x -> evaluateScientificFloatUnSigned x);;

let evaluateScientificFloatPlus list =
  let num1 = float_of_string (fst list) in
  let _dot = fst (snd list) in
  let num1_frac = (fst (snd (snd list))) in
  let _e = fst (snd (snd (snd list))) in
  let _sign = fst (snd (snd (snd (snd list)))) in
  let num2 = float_of_string (snd (snd (snd (snd (snd list))))) in
  (num1  *. (10. ** num2)) +. (float_of_string ("0."^(num1_frac))  *. (10. ** num2)) ;;

let scientificFloatPlusNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _plusSign = char '+' in
  let _plusSignPack = pack _plusSign (fun ch -> Char.escaped ch) in
  let _dot = char '.' in
  let _dotPack = pack _dot (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString
				   (caten _dotPack
				      (caten naturalString
					 (caten _eStringPack
					    (caten _plusSignPack naturalString))))) in
  pack _scientificNumberParser (fun x -> evaluateScientificFloatPlus x);;


let evaluateScientificFloatMinus list =
  let num1 = float_of_string (fst list) in
  let _dot = fst (snd list) in
  let num1_frac = (fst (snd (snd list))) in
  let _e = fst (snd (snd (snd list))) in
  let _sign = fst (snd (snd (snd (snd list)))) in
  let num2 = float_of_string (snd (snd (snd (snd (snd list))))) in
  (num1  /. (10. ** num2)) +. (float_of_string ("0."^(num1_frac))  /. (10. ** num2)) ;;

let scientificFloatMinusNumberParser =
  let _eString = char_ci 'e' in
  let _eStringPack = pack _eString (fun ch -> Char.escaped ch) in
  let _plusSign = char '-' in
  let _plusSignPack = pack _plusSign (fun ch -> Char.escaped ch) in
  let _dot = char '.' in
  let _dotPack = pack _dot (fun ch -> Char.escaped ch) in
  let _scientificNumberParser = (caten naturalString
				   (caten _dotPack
				      (caten naturalString
					 (caten _eStringPack
					    (caten _plusSignPack naturalString))))) in
  pack _scientificNumberParser (fun x -> evaluateScientificFloatMinus x);;



let scientificFloatParser = disj scientificFloatUnSignedNumberParser
  (disj scientificFloatPlusNumberParser scientificFloatMinusNumberParser);;


let scientificNumberParser =
  let _scientificNumberParser = disj scientificFloatParser scientificIntegerParser in
  pack _scientificNumberParser (fun n -> Number (Float n));;


let numberParser =
  let _numberParser = disj_list [scientificNumberParser; hexFloatParser; floatParser; integerParser; hexIntegerParser] in
  pack _numberParser (fun x -> x);; 


(*end number*)


(*Maybe need to add sexprComment
Issue with return Nil with no input*)


(*Maybe need to add sexprComment
Issue with return Nil with no input*)

let rec _sexprParser_ string =
  pack(caten skip (caten (disj_list [booleanParser;
             charParser;
             numberParser;
             stringParser;
             symbolParser;
             roundListParser;
             squareListParser;
             roundDottedListParser;
             squareDottedListParser;
             sexprComment;
             roundVectorParser;
             squareVectorParser;
             quotedParser;
             qQuotedParser;
             unQuotedSplicedParser;
             unQuotedParser]) skip))
(fun(exp) -> fst(snd (exp)))
string
and sexprComment str =
  let _comment_ = word ";#" in
  let _sexprCommentParser = caten _comment_ _sexprParser_ in
  pack _sexprCommentParser (fun l -> Nil)
    str
and skip str =
  let spacesAndComments = star(disj_list[whiteSpace ; sexprComment; lineComment]) in
  pack  spacesAndComments (fun l -> Nil)
    str
and roundListParser str =
  let sexprsInList = caten skip (caten (star _sexprParser_) skip) in
  let _list_ = caten leftParen (caten sexprsInList rightParen) in
  pack _list_ (function (l,((a, (x,y)),r))  -> match x with
             | [] -> Nil
             | _->(List.fold_right (fun a b -> Pair(a,b)) x Nil))
    str
and squareListParser str =
  let sexprsInList = caten skip (caten (star _sexprParser_) skip) in
  let _list_ = caten squareLeftParen (caten sexprsInList squareRightParen) in
  pack _list_ (function (l,((a, (x,y)),r))  -> match x with
             | [] -> Nil
             | _->(List.fold_right (fun a b -> Pair(a,b)) x Nil))
    str  
and roundDottedListParser str =
  let leftSexprs = plus _sexprParser_ in
  let _dottedList_ = caten leftParen (caten leftSexprs (caten dot (caten _sexprParser_ rightParen))) in
  pack _dottedList_((function (a,(b,(c,(d,e)))) -> match b with
             | _ -> (List.fold_right (fun a b -> Pair (a, b)) b d)))
    str
and squareDottedListParser str =
  let leftSexprs = plus _sexprParser_ in
  let _dottedList_ = caten squareLeftParen (caten leftSexprs (caten dot (caten _sexprParser_ squareRightParen))) in
  pack _dottedList_((function (a,(b,(c,(d,e)))) -> match b with
             | _ -> (List.fold_right (fun a b -> Pair (a, b)) b d)))
    str
and roundVectorParser str =
  let sexprs = star _sexprParser_ in
  let _vector = caten hashtagParser (caten leftParen (caten sexprs rightParen)) in
  pack _vector (fun (a,(b,(c,d))) -> match c with
                                       |_ -> Vector(c))
    str
and squareVectorParser str =
  let sexprs = star _sexprParser_ in
  let _vector = caten hashtagParser (caten squareLeftParen (caten sexprs squareRightParen)) in
  pack _vector (fun (a,(b,(c,d))) -> match c with
                                       |_ -> Vector(c))
    str
and quotedParser str = 
  let quote = word "'" in
  let _qouted_ = caten quote _sexprParser_ in
  pack _qouted_ (fun (q,exp) -> Pair(Symbol("quote") , Pair(exp,Nil)))
    str
and qQuotedParser str = 
  let quote = word "`" in
  let _qouted_ = caten quote _sexprParser_ in
  pack _qouted_ (fun (q,exp) -> Pair(Symbol("quasiquote") , Pair(exp,Nil)))
    str
and unQuotedSplicedParser str =
  let quote = word ",@" in
  let _qouted_ = caten quote _sexprParser_ in
  pack _qouted_ (fun (q,exp) -> Pair(Symbol("unquote-splicing") , Pair(exp,Nil)))
    str
and unQuotedParser str =
  let quote = word "," in
  let _qouted_ = caten quote _sexprParser_ in
  pack _qouted_ (fun (q,exp) -> Pair(Symbol("unquote") , Pair(exp,Nil)))
    str;; 
  
  


let a = string_to_list "[,4 `31.31]";;

_sexprParser_ a;;
sexprParser                              


(*******************************************************)


(* List *)

let whiteSpaces = star whiteSpace;;

let _sexprParser =
  let _wrapper = disj_list [numberParser; booleanParser; charParser; symbolParser; stringParser] in
  not_followed_by _wrapper symbolParser;;
  
let _sexprParserWithSpace =
  let _wrapper = caten _sexprParser whiteSpace in
  pack _wrapper (fun tuple -> fst tuple);;


let sexprParser = disj _sexprParserWithSpace _sexprParser;;


test_string sexprParser "#t1";;

let nilParser =
  let _nilParser = caten leftParenParser (caten whiteSpaces rightParenParser) in
  pack _nilParser (fun x -> Nil);;


(****************************************************************************)

let read_sexpr string = sexprParser (string_to_list string) ;;

let read_sexprs string =
  raise X_not_yet_implemented;;
