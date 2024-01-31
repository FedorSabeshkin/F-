let notDivisible (n, m) = m % n = 0


let rec check_rec = function
 | (n, m) when n < 1 -> false
 | (n, 1) -> true
 | (n, m) when n = m -> true && (check_rec (n, m-1))
 | (n, m) -> ((n % m) <> 0) && (check_rec (n, m-1))
 
let prime = function
 | n -> check_rec (n, n)
