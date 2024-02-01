// reprint string n 
let rec pow = function
 | (n, 0) -> ""
 | (n, m) -> n + pow(n, m-1)

 // predicate
let rec isIthChar = function
 | (s, i, c) when String.length(s) <= i -> false
 | (s ,n, c) -> s.[n] = c

// returns the number of occurrences of the character c in the string s, starting from position n 
let rec occFromIthAccumulator = function
 | ((s), (n), (c), (amount)) when n < ((String.length s)) -> if (s.[n] = c) 
                                                                                 then
                                                                                    let nextAmount = amount + 1 
                                                                                    occFromIthAccumulator(s,n+1,c, nextAmount);
                                                                                 else
                                                                                    occFromIthAccumulator(s,n+1,c, amount);
 | ((s), (n), (c), (amount)) -> amount
 
let rec occFromIth = function
 | ((s),(n),(c)) -> occFromIthAccumulator (s,n,c,0)
