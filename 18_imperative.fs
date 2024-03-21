
let f n =
    let mutable x = 1
    let mutable i = 2
    while i <= n do
        x <- x * i
        i <- i + 1
    x


let fibo n =
    let mutable a = 1
    let mutable b = 0
    let mutable i = 0
    while i < n do
        let c = a + b
        a <- b
        b <- c
        i <- i + 1
    b
