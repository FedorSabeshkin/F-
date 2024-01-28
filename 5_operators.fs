let notDivisible (n, m) = m % n = 0


let prime n =
    let rec notDiv = function
     | (n, 0) | (n, 1) -> true
     | (n, m) -> (n % m <> 0) && notDiv(n, m - 1)
    let maxDiv = int(System.Math.Sqrt(float(n)))
    notDiv(n, maxDiv) 
