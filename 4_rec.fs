let rec fibo = function 
 | 0  -> 0
 | 1  -> 1
 | n  -> fibo(n-2) + fibo(n-1)

let rec sum = function 
 | 0  -> 0
 | 1  -> 1
 | n  -> n + sum(n-1)

let rec sum2 = function 
 | (m,0) -> m 
 | (m,n) -> m + n + sum2(m, n - 1)
