let even_seq = Seq.initInfinite (fun i -> (i+1)*2)

let factorial n =
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    f n 1
let fac_seq = Seq.initInfinite factorial

let seq_seq = Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else (-1) * (n + 1)/2)
