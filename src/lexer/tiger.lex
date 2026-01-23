type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0

fun eof() = 
    let 
        val pos = hd(!linePos) 
        val () = if !commentDepth > 0 then raise Fail "Unclosed Comment" else ()
    in 
        Tokens.EOF(pos,pos) 
    end

%% 
%s COMMENT;
digit= [0-9];

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

<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<INITIAL> "/*"  => (YYBEGIN COMMENT; commentDepth := 1; continue());
<COMMENT> "/*"  => (commentDepth := !commentDepth + 1; continue());
<COMMENT> "*/"  => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> . => (continue());
