type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0

val stringStartPos = ref 0
(* Note: Characters accumulate in the front *)
val charsReadByString = []
val numCharsReadByString = ref 0

fun eof() = 
    let 
        val pos = hd(!linePos) 
        val () = if !commentDepth > 0 then raise Fail "Unclosed Comment" else ()
    in 
        Tokens.EOF(pos,pos) 
    end

%% 
%s COMMENT;
%s STRING;
digit= [0-9];
printable = [ -~];
%%


<INITIAL> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> [" " | \t]   => (continue());

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

<INITIAL> "\""  => (
    YYBEGIN STRING;
    charsReadByString := [];
    stringStartPos := yypos;
    numCharsRead = 0;
    continue()
);
<STRING> "\\"{printable} => (
    let
        val character = STRING.sub yytext 1
    in 
        numCharsRead := !numCharsRead + 1;
        charsReadByString := character :: !charsReadByString;
	continue()
    end
);
<STRING> {printable} => (
    numCharsRead := !numCharsRead + 1;
    charsReadByString = STRING.sub yytext 0 :: !charsReadByString;
    continue()
);
<STRING> "\"" => (
    let
        val string = String.implode (List.rev !charsReadByString)
        val strBegin = !stringStartPos
        val strEnd = strBegin + !numCharsReadByString
    in
        Tokens.STRING(string, strBegin, strEnd)
    end
YYBEGIN INITIAL; )
<STRING> \n   => (ErrorMsg.error yypos "illegal EOL in string literal"; continue());
<STRING> .    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
