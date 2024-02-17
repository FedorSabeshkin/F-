let vat n x = x * (1.0 + float(n) / 100.0)


let unvat n x = x / ((float n)/100.0 + 1.0)


let rec min f = 
   if (f 0) = 0 then
      0 
   else
      let composition = f << (+)1
      1 + min composition
