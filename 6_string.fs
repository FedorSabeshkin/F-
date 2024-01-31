// reprint string n 
let rec pow = function
 | (n, 0) -> ""
 | (n, m) -> n + pow(n, m-1)

// predicate
let rec isIthChar = function
 | ((s:string),(n:int),(c:char)) -> char(s.[n]) = c
