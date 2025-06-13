structure Main =
struct
fun main filename =
    let
        val ast = Parse.parse filename
    in
        Semant.transProg ast
    end
end
