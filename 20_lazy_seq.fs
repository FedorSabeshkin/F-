
let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

let fac_seq = Seq.initInfinite (
    fun i -> 
        let rec iter n acc =
            if n <= 1 then acc
            else iter (n - 1) (n * acc)
        iter i 1
)

let even_sign i = 2 * (i % 2) - 1

let seq_seq = Seq.initInfinite (fun i ->
    if i = 0 then 0
    else ((i + 1) / 2) * (even_sign (i + 1))
)
