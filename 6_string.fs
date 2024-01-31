// reprint string n 
let rec pow = function
 | (n, 0) -> ""
 | (n, m) -> n + pow(n, m-1)

// predicate
let rec isIthChar = function
 | ((s:string),(n:int),(c:char)) -> char(s.[n]) = c

// returns the number of occurrences of the character c in the string s, starting from position n 
let rec occFromIthAccumulator = function
 | ((s:string), (n:int), (c:char), (amount:int)) when n < ((String.length s)) -> if (char(s.[n]) = c) 
                                                                                 then
                                                                                    let nextAmount = amount + 1 
                                                                                    occFromIthAccumulator(s,n+1,c, nextAmount);
                                                                                 else
                                                                                    occFromIthAccumulator(s,n+1,c, amount);
 | ((s:string), (n:int), (c:char), (amount:int)) -> amount
 
let rec occFromIth = function
 | ((s:string),(n:int),(c:char)) -> occFromIthAccumulator (s,n,c,0)
