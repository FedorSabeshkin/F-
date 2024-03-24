type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }


let to_24_format = function
  | {hours = h; minutes = m; f = PM} -> {hours = h + 12; minutes = m; f = AM}
  | {hours = h; minutes = m; f = AM} -> {hours = h; minutes = m; f = AM}

let (.>.) x y = 
    (to_24_format x) > (to_24_format y)
