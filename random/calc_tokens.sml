structure Tokens : Calc_TOKENS =
struct
type linenum = int
type token = string

fun NUM(c, i, j) = "NUM("^c^")   " ^ Int.toString(i)
fun STRING(c, i, j) = "STRING("^c^")   " ^ Int.toString(i) ^ " to " ^ Int.toString(j)
fun LBRACK(i, j) = "LBRACK   " ^ Int.toString(i)
fun RBRACK(i, j) = "RBRACK   " ^ Int.toString(i)
fun PLUS(i, j) = "PLUS   " ^ Int.toString(i)
fun MINUS(i, j) = "MINUS   " ^ Int.toString(i)
fun TIMES(i, j) = "TIMES   " ^ Int.toString(i)
fun DIVIDE(i, j) = "DIVIDE   " ^ Int.toString(i)
fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
