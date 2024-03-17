let try_find key m =
    let rec find = function
        | [] -> None
        | (k, v) :: tail -> if k = key then Some(v) else find tail
    find (Map.toList m)
