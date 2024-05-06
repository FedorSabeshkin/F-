// 0, -1, 1, -2, 2, -3, 3, ...
let rec fun_seq_pos_neq i = seq {  
  yield (0 - i)
  yield i  
  yield! fun_seq_pos_neq (i+1)
}
let seq_seq = seq {  
  yield 0
  yield! fun_seq_pos_neq 1
}

let rec fac_seq_fun current_factorial i = seq { 
  let nextMultiplier  = i + 1
  let next_factorial = current_factorial * nextMultiplier  
  yield next_factorial 
  yield! fac_seq_fun next_factorial nextMultiplier  
}
// factorial sequance
let fac_seq = seq {   
  yield 1 
  yield 1 
  yield! fac_seq_fun 1 1
}
