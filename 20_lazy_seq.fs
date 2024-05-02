let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

let fac_seq = Seq.initInfinite (
    fun i -> 
        // define rec function
        let rec iter n acc =
            if n <= 1I then acc
            // use tail recursion for optimization
            else iter (n - 1I) (n * acc)
        // call rec function with default value for accumulator = 1.    
        iter (bigint i) 1I
)

let seq_seq = Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else (-1) * (n + 1)/2)
