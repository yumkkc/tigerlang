structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun to_int v = List.foldl (fn (a, b) => ord(a) - ord(#"0") + b*10) 0 (explode v);


%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT;
alpha=[A-Za-z];
digit=[0-9];
alphanumeric=[A-Za-z0-9];
space=[\ \t\r];
%%

<INITIAL>"var" => (Tokens.VAR(yypos, yypos + String.size yytext));
<INITIAL>"type" => (Tokens.TYPE(yypos, yypos + String.size yytext));
<INITIAL>"function" => (Tokens.FUNCTION(yypos, yypos + String.size yytext));
<INITIAL>"break" => (Tokens.BREAK(yypos, yypos + String.size yytext));
<INITIAL>"of" => (Tokens.OF(yypos, yypos + String.size yytext));
<INITIAL>"end" => (Tokens.END(yypos, yypos + String.size yytext));
<INITIAL>"in" => (Tokens.IN(yypos, yypos + String.size yytext));
<INITIAL>"nil" => (Tokens.NIL(yypos, yypos + String.size yytext));
<INITIAL>"let" => (Tokens.LET(yypos, yypos + String.size yytext));
<INITIAL>"do" => (Tokens.DO(yypos, yypos + String.size yytext));
<INITIAL>"to" => (Tokens.TO(yypos, yypos + String.size yytext));
<INITIAL>"for" => (Tokens.FOR(yypos, yypos + String.size yytext));
<INITIAL>"while" => (Tokens.WHILE(yypos, yypos + String.size yytext));
<INITIAL>"else" => (Tokens.ELSE(yypos, yypos + String.size yytext));
<INITIAL>"then" => (Tokens.THEN(yypos, yypos + String.size yytext));
<INITIAL>"if" => (Tokens.IF(yypos, yypos + String.size yytext));
<INITIAL>"array" => (Tokens.ARRAY(yypos, yypos + String.size yytext));
<INITIAL>":=" => (Tokens.ASSIGN(yypos, yypos + String.size yytext));
<INITIAL>"|" => (Tokens.OR(yypos, yypos + String.size yytext));
<INITIAL>"&" => (Tokens.AND(yypos, yypos + String.size yytext));
<INITIAL>">=" => (Tokens.GE(yypos, yypos + String.size yytext));
<INITIAL>">" => (Tokens.GT(yypos, yypos + String.size yytext));
<INITIAL>"<=" => (Tokens.LE(yypos, yypos + String.size yytext));
<INITIAL>"<" => (Tokens.LT(yypos, yypos + String.size yytext));
<INITIAL>"<>" => (Tokens.NEQ(yypos, yypos + String.size yytext));
<INITIAL>"=" => (Tokens.EQ(yypos, yypos + String.size yytext));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos + String.size yytext));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos + String.size yytext));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos + String.size yytext));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos + String.size yytext));
<INITIAL>"." => (Tokens.DOT(yypos, yypos + String.size yytext));
<INITIAL>"}" => (Tokens.RBRACE(yypos, yypos + String.size yytext));
<INITIAL>"{" => (Tokens.LBRACE(yypos, yypos + String.size yytext));
<INITIAL>"]" => (Tokens.RBRACK(yypos, yypos + String.size yytext));
<INITIAL>"[" => (Tokens.LBRACK(yypos, yypos + String.size yytext));
<INITIAL>")" => (Tokens.RPAREN(yypos, yypos + String.size yytext));
<INITIAL>"(" => (Tokens.LPAREN(yypos, yypos + String.size yytext));
<INITIAL>";" => (Tokens.SEMICOLON(yypos, yypos + String.size yytext));
<INITIAL>":" => (Tokens.COLON(yypos, yypos + String.size yytext));
<INITIAL>"," => (Tokens.COMMA(yypos, yypos + String.size yytext));

<INITIAL>["][^"]*["] => (Tokens.STRING(yytext, yypos, yypos + size yytext));



<INITIAL>{digit}+ => (Tokens.INT(to_int(yytext), yypos, yypos+String.size yytext));
<INITIAL>{alpha}+({alphanumeric}|"_")* => (Tokens.ID(yytext, yypos, yypos+String.size yytext));

<INITIAL>"/*" => (YYBEGIN COMMENT; continue());
<COMMENT>"*/" => (YYBEGIN INITIAL; continue());
<COMMENT>. => (continue());

<INITIAL>{space}+ => (continue());
"\n" => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<INITIAL>["][^"]* => (ErrorMsg.error yypos ("Unterminated string " ^ yytext); continue());
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

