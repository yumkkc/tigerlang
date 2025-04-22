type lexresult = Tokens.token
fun eof() = Tokens.EOF(0, 0)
%%
%structure CalcLex
%s COMMENT;
alpha=[A-Za-z];
digit=[0-9];
%%

<INITIAL>"(" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>")" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"(*" => (YYBEGIN COMMENT; continue());
[ \t\n\r]+ => (continue());
<COMMENT> ")*" => (YYBEGIN INITIAL; continue());
<COMMENT>. => (continue());
<INITIAL>{digit}+ => (Tokens.NUM(yytext, yypos, yypos));
<INITIAL>. => (print("Error!!"); continue());
