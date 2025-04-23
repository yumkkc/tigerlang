type lexresult = Tokens.token
fun eof() = Tokens.EOF(0, 0)
%%
%structure CalcLex
%s COMMENT;
%s STRING;
alpha=[A-Za-z];
digit=[0-9];
alphanumericnp=[A-Za-z0-9] | [ \t\n];
%%

<INITIAL>"(" => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>")" => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"+" => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-" => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*" => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/" => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>{digit}+ => (Tokens.NUM(yytext, yypos, yypos));
<INITIAL>"\"" => (YYBEGIN STRING; continue());
<STRING>{alphanumericnp}+ => (Tokens.STRING(yytext, yypos, yypos));
<STRING>"\"" => (YYBEGIN INITIAL; continue());
<INITIAL>[ \t\n\r]+ => (continue());
<COMMENT>[ \t\n\r]+ => (continue());
<INITIAL>"(*" => (YYBEGIN COMMENT; continue());
<COMMENT> "*)" => (YYBEGIN INITIAL; continue());
<COMMENT>. => (continue());
<INITIAL>. => (print("Error!!"); continue());
