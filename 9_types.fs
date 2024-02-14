let copper_per_silver = 12
let silver_per_gold = 20

let divmod (x, y) =
    let (q, r) = (x / y, x % y)
    if r >= 0 then
        (q, r)
    else
        (q - 1, r + y)

let norm_money x =
    let (g, s, c) = x
    let (sa, c1) = divmod(c, copper_per_silver)
    let (ga, s1) = divmod(s + sa, silver_per_gold)
    (g + ga, s1, c1)


let (.+.) x y =
    let (xg, xs, xc) = x
    let (yg, ys, yc) = y
    norm_money(xg + yg, xs + ys, xc + yc)

let (.-.) x y =
    let (xg, xs, xc) = x
    let (yg, ys, yc) = y
    norm_money(xg - yg, xs - ys, xc - yc)

let (.+) x y =
    let (xr, xi) = x
    let (yr, yi) = y
    (xr + yr, xi + yi)

let (.-) x y =
    let (yr, yi) = y
    x .+ (-yr, -yi)

let (.*) x y =
    let (xr, xi) = x
    let (yr, yi) = y
    (xr * yr - xi * yi, xr * yi + xi * yr)

let (./) x y =
    let (yr, yi) = y
    let y2 = yr * yr + yi * yi
    x .* (yr / y2, -yi / y2)
