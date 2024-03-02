
let rec rmodd = function
  | [] -> []
  | [x] -> []
  | first :: (second :: tail) -> second :: (rmodd tail)


let rec del_even = function
  | [] -> []
  | head :: tail when (head % 2 = 0) -> (del_even tail)
  | head :: tail -> head :: (del_even tail)


let rec multiplicity x xs = match xs with
  | [] -> 0
  | head :: tail when (head = x) -> 1 + (multiplicity x tail)
  | _ :: tail -> multiplicity x tail


let rec split = function
  | [] -> ([], [])
  | [x] -> ([x], [])
  | first :: (second :: tail) ->
        let (a, b) = split tail
        (first :: a, second :: b)


let rec zip = function
  | ([], []) -> []
  | (h1 :: t1, h2 :: t2) -> (h1, h2) :: zip(t1, t2)
  | _ -> failwith "different lengths"
