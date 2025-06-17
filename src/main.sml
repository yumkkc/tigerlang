structure Main =
struct
fun main filename =
    let
        val ast = Parse.parse filename
        val _ = FindEscape.findEscape ast
    in
        Semant.transProg ast
    end
end
