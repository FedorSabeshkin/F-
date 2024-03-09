let list_filter f xs = List.foldBack (fun head tail -> if (f head) then head :: tail else tail) xs []


let sum (p, xs) = List.fold (fun acc item -> if (p item) then item + acc else acc) 0 xs


let revrev = List.fold (fun acc item -> (List.rev item) :: acc) []
