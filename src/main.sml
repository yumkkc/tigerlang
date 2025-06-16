structure Main =
struct
fun main filename =
    let
        val ast = Parse.parse filename
    in
        FindEscape.findEscape ast; (* finding the escape *)
        Semant.transProg ast
    end
end
