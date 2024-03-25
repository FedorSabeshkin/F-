
let rec fibo1 n n1 n2 =
    if n = 0 then n2
    else fibo1 (n - 1) (n1 + n2) n1

let rec fibo2 n c =
    if n = 0 then c 0
    elif n = 1 then c 1
    else fibo2 (n - 1) (fun f -> c(f + (fibo2 (n - 2) id)))

let rec bigList n k =
    let rec iter n acc =
        if n = 0 then acc
        else iter (n - 1) (1 :: acc)
    iter n (k [])
