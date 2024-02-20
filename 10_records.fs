type TimeOfDay = { hours: int; minutes: int; f: string }

let time24 = function
 | {hours = h; minutes = m; f = "PM"} -> {hours = h + 12; minutes = m; f = ""}
 | {hours = h; minutes = m; f = _} -> {hours = h; minutes = m; f = ""}

let (.>.) x y = time24(x) > time24(y)
