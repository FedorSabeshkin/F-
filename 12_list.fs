let rec upto_with_accum = function
  | (1, accummulator) -> 1 :: accummulator
  | (n, accummulator) -> 
    let updatedAccummulator = n :: accummulator
    upto_with_accum(n-1, updatedAccummulator)

// upto n = [1; 2; ...; n]
let rec upto = function
  | n -> upto_with_accum (n, [])


let rec dnto_with_accum = function
  | (1, accummulator) -> 1 :: accummulator
  | (n, accummulator) -> n :: accummulator


// downto n = [n; n-1; n-2; ...; 1]
let rec dnto = function
  | 1 -> dnto_with_accum (1, []) 
  | n -> dnto_with_accum (n, (dnto (n-1)))



let rec calculate_n_even_with_accum = function
  | (currentValue, currentRound, maxRound) when currentRound < maxRound  -> 
    calculate_n_even_with_accum(currentValue+2, currentRound+1, maxRound)
  | (currentValue, currentRound, maxRound) -> currentValue


let rec calculate_n_even = function
  | n -> calculate_n_even_with_accum(0, 1, n)
    
let rec evenn_with_accum = function
  | (0, accummulator) -> 0 :: accummulator
  | (n, accummulator) -> 
    let updatedAccummulator = n :: accummulator
    evenn_with_accum(n-2, updatedAccummulator)

// evenn n = [0; 2; ...; n-2; n]
let rec evenn = function
  | n -> 
    let maxValue = calculate_n_even n
    evenn_with_accum (maxValue, [])
