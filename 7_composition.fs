let vat n x = x * (1.0 + float(n) / 100.0)


let unvat n x = x / (1.0 + float(n) / 100.0)


let rec min f =
    if f(0) = 0 then
        0
    else
        1 + min(f << (+)1)
