signature Calc_TOKENS =
sig
    type linenum
    type token
    val NUM: (string) * linenum * linenum -> token
    val STRING: (string) * linenum * linenum -> token
    val LBRACK: linenum * linenum -> token
    val RBRACK: linenum * linenum -> token
    val PLUS: linenum * linenum -> token
    val MINUS: linenum * linenum -> token
    val TIMES: linenum * linenum -> token
    val DIVIDE: linenum * linenum -> token
    val EOF:  linenum * linenum -> token
end
