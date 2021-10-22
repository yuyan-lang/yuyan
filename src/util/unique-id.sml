structure UID =
struct
    val counter : int ref = ref 0

    fun next() : int = 
    let 
        val _ = counter := !counter + 1
    in 
        !counter
    end

end