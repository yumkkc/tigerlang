use "sample.lex.sml";

fun parse () =
    let
        (* Open the CalcLex structure to bring its definitions into scope *)
        open CalcLex.UserDeclarations

        fun get _ = TextIO.input TextIO.stdIn
        val lexer = CalcLex.makeLexer get

        fun loop () =
            let
                val token = lexer ()
            in
                case token of
                    EOF => print "EOF\n"
                  | EOS => (print "EOS\n"; loop ())
                  | DIV => (print "DIV\n"; loop ())
                  | LPAREN => (print "LPAREN\n"; loop ())
                  | RPAREN => (print "RPAREN\n"; loop ())
                  | PLUS => (print "PLUS\n"; loop ())
                  | SUB => (print "SUB\n"; loop ())
                  | TIMES => (print "TIMES\n"; loop ())
                  | PRINT => (print "PRINT\n"; loop ())
                  | ID s => (print ("ID(" ^ s ^ ")\n"); loop ())
                  | NUM i => (print ("NUM(" ^ Int.toString i ^ ")\n"); loop())
            end
    in
        loop ()
    end
