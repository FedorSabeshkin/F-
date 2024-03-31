
let rec get_odd_with_accum = function
  | (head :: tail, currentRound, maxRound, accumulator) when currentRound < maxRound -> 
    if(currentRound % 2 = 1)  
    then get_odd_with_accum(tail, currentRound+1, maxRound, head :: accumulator)
    else get_odd_with_accum(tail, currentRound+1, maxRound, accumulator)
  | ([head], currentRound, maxRound, accumulator) -> 
    if(currentRound % 2 = 1)  
    then head :: accumulator
    else accumulator
  | ([ ], currentRound, maxRound, accumulator) -> 
    accumulator
    
// Оставить только значения на нечетных позициях
let rec rmodd = function
  | [] -> []
  | list -> 
    let odd_item_list = get_odd_with_accum (list, 0, (List.length list), [])
	List.rev odd_item_list

let rec del_even_with_accum = function
  | (head :: tail, accumulator) -> 
    if(head % 2 = 1)  
    then del_even_with_accum(tail, head :: accumulator)
    else del_even_with_accum(tail, accumulator)
  | ([head], accumulator) -> 
    if(head % 2 = 1)  
    then head :: accumulator
    else accumulator
  | ([ ], accumulator) -> 
    accumulator
    
// Вернуть только нечетные значения 
let rec del_even =  function
  | [] -> []
  | list -> 
    let odd_item_list = del_even_with_accum (list, [])
    List.rev odd_item_list

let rec multiplicity_with_accum = function
  | (search_value, head :: tail, amount) -> 
    if(head = search_value)  
    then multiplicity_with_accum(search_value, tail, amount+1)
    else multiplicity_with_accum(search_value, tail, amount)
  | (search_value, [head], amount) -> 
    if(head = search_value)  
    then amount + 1
    else amount
  | (search_value, [ ], amount) -> 
    amount
    
// Вычисляет число повторений элемента "x" в списке "xs"
let rec multiplicity x xs = match xs with
  | [] -> 0
  | _  -> 
    multiplicity_with_accum (x, xs, 0)

let rec split_with_accum = function
  | (head :: tail, currentRound, maxRound, first_list, second_list) when currentRound < maxRound -> 
    if(currentRound % 2 = 1)  
    then split_with_accum(tail, currentRound+1, maxRound, first_list, head :: second_list)
    else split_with_accum(tail, currentRound+1, maxRound, head :: first_list, second_list)
  | ([head], currentRound, maxRound, first_list, second_list) -> 
    if(currentRound % 2 = 1)  
    then (List.rev first_list, List.rev (head :: second_list))
    else (List.rev (head :: first_list), List.rev second_list)
  | ([], currentRound, maxRound, first_list, second_list) -> 
    ( List.rev first_list, List.rev second_list)

let rec split = function
  | [] -> ([], [])
  | [x] -> ([x], [])
  | list -> 
    split_with_accum (list, 0, (List.length list), [], [])


exception ListNotEqualsSize

let rec zip_with_accum = function
  | (head_1 :: tail_1 , head_2 :: tail_2, accumulator) ->
      zip_with_accum(tail_1, tail_2, (head_1, head_2) :: accumulator)
  | (_, _, accumulator) -> 
    List.rev accumulator
    
let rec zip = function
  | ([], []) -> []
  | (xs1,xs2) when (List.length xs1 = List.length xs2) -> 
    zip_with_accum (xs1,xs2, [])
  | (xs1,xs2) -> raise ListNotEqualsSize
