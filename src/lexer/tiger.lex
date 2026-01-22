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
%%
<INITIAL> "/*"  => (YYBEGIN COMMENT; commentDepth := 1; continue());
<INITIAL> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> ","   => (Tokens.COMMA(yypos,yypos+1));
<INITIAL> var   => (Tokens.VAR(yypos,yypos+3));
<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

<COMMENT> "/*"  => (commentDepth := !commentDepth + 1; continue());
<COMMENT> "*/"  => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<COMMENT> . => (continue());
