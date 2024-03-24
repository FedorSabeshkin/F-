type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = 
    let timeOfDay_first = x
    let timeOfDay_second = y
    if (String.Equals(timeOfDay_first.f, timeOfDay_second.f) && timeOfDay_first.hours > timeOfDay_second.hours)
    then true
    else
      if (String.Equals(timeOfDay_first.f, timeOfDay_second.f) 
        && timeOfDay_first.hours = timeOfDay_second.hours
        && timeOfDay_first.minutes > timeOfDay_second.minutes)
      then true
      else
        if (String.Equals(timeOfDay_first.f,"PM") && String.Equals(timeOfDay_second.f, "AM"))
        then true
        else false
