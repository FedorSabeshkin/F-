let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

let fac_seq = Seq.initInfinite (
    fun i -> 
        let rec iter n acc =
            if n <= 1 then acc
            else iter (n - 1) (n * acc)
        iter i 1
)

let seq_seq = Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else (-1) * (n + 1)/2)
