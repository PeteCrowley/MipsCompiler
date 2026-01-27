type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val commentDepth = ref 0

val stringStartPos = ref 0
(* Note: Characters accumulate in the front *)
val charsReadByString = ref ([] : char list)

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

structure StringOrd : ORD_KEY = 
struct
    type ord_key = string
    val compare = String.compare
end
structure StringMap : ORD_MAP = RedBlackMapFn(StringOrd)

val keywordMap = 
    let
        val pairs = [
            ("type", Tokens.TYPE),
            ("var", Tokens.VAR),
            ("function", Tokens.FUNCTION),
            ("break", Tokens.BREAK),
            ("of", Tokens.OF), 
            ("end", Tokens.END),
            ("in", Tokens.IN),
            ("nil", Tokens.NIL),
            ("let", Tokens.LET),
            ("do", Tokens.DO),
            ("to", Tokens.TO),
            ("for", Tokens.FOR),
            ("while", Tokens.WHILE),
            ("else", Tokens.ELSE),
            ("then", Tokens.THEN),
            ("if", Tokens.IF),
            ("array", Tokens.ARRAY)
        ]
        fun insert ((k, v), m) = StringMap.insert (m, k, v)
	    val f = foldl insert StringMap.empty
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
format = [ \r\n\t];
printable = [ -~];
identiferCharacter = [a-zA-Z0-9_];

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

<INITIAL> [a-zA-Z]{identiferCharacter}+   => (
    let 
        val keywordMatch = StringMap.find (keywordMap, yytext)
    in
        case keywordMatch of
            NONE => Tokens.ID(yytext, yypos, yypos + String.size yytext)
          | SOME keyword => keyword(yypos, String.size yytext)
    end
);

<INITIAL> "/*"  => (YYBEGIN COMMENT; commentDepth := 1; continue());
<COMMENT> "/*"  => (commentDepth := !commentDepth + 1; continue());
<COMMENT> "*/"  => (commentDepth := !commentDepth - 1; if !commentDepth = 0 then (YYBEGIN INITIAL; continue()) else continue());
<COMMENT> \n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT> . => (continue());

<INITIAL> \"    => (
    YYBEGIN STRING;
    charsReadByString := [];
    stringStartPos := yypos;
    continue()
);
<STRING> \\{format}+\\ => (
    continue()
);
<STRING> \\[0-9][0-9][0-9] => (
    let
        val codeString = String.substring (yytext, 1, 3)
        val asciiCodeOption = Int.fromString codeString
        val asciiCode = case asciiCodeOption of
          NONE => raise Fail("Compiler bug: matched \"" ^ yytext ^ "\" as \\ddd escape sequence")
        | SOME c => c
        val () =
            if asciiCode > Char.maxOrd then
                ErrorMsg.error yypos ("illegal ASCII code " ^ yytext)
            else
                charsReadByString := (Char.chr asciiCode) :: (!charsReadByString)
    in
        continue()
    end
);
<STRING> \\{printable} => (
    let
        val escapeIdentifier = String.sub (yytext, 1)
        val escapeTrueChar = CharMap.find (escapeMap, escapeIdentifier)
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
        val strEnd = yypos
    in
        YYBEGIN INITIAL;
        Tokens.STRING(string, strBegin, strEnd)
    end
);
<STRING> {printable} => (
    charsReadByString := (String.sub (yytext, 0)) :: !charsReadByString;
    continue()
);
<STRING> \n   => (ErrorMsg.error yypos "illegal EOL in string literal"; continue());
<STRING> .    => (ErrorMsg.error yypos ("illegal character in string " ^ yytext); continue());

<INITIAL> . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());