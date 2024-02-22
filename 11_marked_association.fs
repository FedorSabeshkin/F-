type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let shift = function
 | AM -> 0
 | PM -> 12

let total_minutes (x: TimeOfDay) = (x.hours + shift(x.f)) * 60 + x.minutes

let (.>.) x y = total_minutes(x) > total_minutes(y)
