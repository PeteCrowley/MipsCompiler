type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0

val stringStartPos = ref 0
(* Note: Characters accumulate in the front *)
val charsReadByString = ref ([] : char list)
val numCharsReadByString = ref 0

structure CharOrd : ORD_KEY =
struct
    type ord_key = char
    val compare = Char.compare
end
structure CharMap : ORD_MAP = RedBlackMapFn(CharOrd)

val escapeMap =
    let
        val pairs = [
            (#"\\", #"\\"),
            (#"\"", #"\""),
            (#"b", #"\b"),
            (#"n", #"\n"),
            (#"t", #"\t")
        ]
        fun insert ((k, v), m) = CharMap.insert (m, k, v)
	val f = foldl insert CharMap.empty
    in
        f pairs
    end

fun eof() = 
    let 
        val pos = hd(!linePos) 
        val () = if !commentDepth > 0 then raise Fail "Unclosed Comment" else ()
    in 
        Tokens.EOF(pos,pos) 
    end

%% 
%s STRING COMMENT;
digit= [0-9];
printable = [ -~];
%%


<INITIAL> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> (" " | \t)   => (continue());

<INITIAL> ":="  => (Tokens.ASSIGN(yypos, yypos+2));
<INITIAL> "&"   => (Tokens.AND(yypos, yypos+1));
<INITIAL> "|"   => (Tokens.OR(yypos, yypos+1));
<INITIAL> ">="  => (Tokens.GE(yypos, yypos+2));
<INITIAL> ">"   => (Tokens.GT(yypos, yypos+1));
<INITIAL> "<="  => (Tokens.LE(yypos, yypos+2));
<INITIAL> "<"   => (Tokens.LT(yypos, yypos+1));
<INITIAL> "<>"  => (Tokens.NEQ(yypos, yypos+2));
<INITIAL> "="   => (Tokens.EQ(yypos, yypos+1));
<INITIAL> "/"   => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL> "*"   => (Tokens.TIMES(yypos, yypos+1));
<INITIAL> "-"   => (Tokens.MINUS(yypos, yypos+1));
<INITIAL> "+"   => (Tokens.PLUS(yypos, yypos+1));
<INITIAL> "."   => (Tokens.DOT(yypos, yypos+1));
<INITIAL> "{"   => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL> "}"   => (Tokens.RBRACE(yypos, yypos+1));
<INITIAL> "["   => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL> "]"   => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL> ")"   => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL> "("   => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL> ";"   => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL> ":"   => (Tokens.COLON(yypos, yypos+1));
<INITIAL> ","   => (Tokens.COMMA(yypos, yypos+1));

<INITIAL> {digit}+ =>(Tokens.INT(
    case Int.fromString yytext of
        SOME n => n
        | NONE => 0, yypos, yypos + String.size yytext));

<INITIAL> "/*"  => (YYBEGIN COMMENT; commentDepth := 1; continue());
<COMMENT> "/*"  => (commentDepth := !commentDepth + 1; continue());
<COMMENT> "*/"  => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> . => (continue());

<INITIAL> \"    => (
    YYBEGIN STRING;
    charsReadByString := [];
    stringStartPos := yypos;
    numCharsReadByString := 0;
    continue()
);
<STRING> \\{printable} => (
    let
        val escapeIdentifier = String.sub (yytext, 1)
        val escapeTrueChar = CharMap.find (escapeMap, escapeIdentifier)
	val () = numCharsReadByString := !numCharsReadByString + 2;
	val () = case escapeTrueChar of
              NONE => (ErrorMsg.error yypos ("illegal escape sequence " ^ yytext); ())
            | SOME chr => (charsReadByString := chr :: (!charsReadByString); ())
    in
        continue()
    end
);
<STRING> \" => (
    let
        val string = String.implode (List.rev (!charsReadByString))
        val strBegin = !stringStartPos
        val strEnd = strBegin + !numCharsReadByString
    in
        YYBEGIN INITIAL;
        Tokens.STRING(string, strBegin, strEnd)
    end
);
<STRING> {printable} => (
    numCharsReadByString := !numCharsReadByString + 1;
    charsReadByString := (String.sub (yytext, 0)) :: !charsReadByString;
    continue()
);
<STRING> \n   => (ErrorMsg.error yypos "illegal EOL in string literal"; continue());
<STRING> .    => (ErrorMsg.error yypos ("illegal character in string " ^ yytext); continue());

<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
