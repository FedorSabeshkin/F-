let copper_per_silver = 12
let silver_per_gold = 20
let to_copper = function
  | (a, b, c) -> (a*silver_per_gold + b) * copper_per_silver + c

let copper_to_full_format_money = function
  | copper -> let copperAmount = copper % copper_per_silver
              let silverFromcopper = copper / copper_per_silver
              let silverAmount = silverFromcopper % silver_per_gold
              let goldAmount = silverAmount / silver_per_gold
              (goldAmount, silverAmount, copperAmount)

let (.-.) x y =
    let (a, b, c) = x
    let (d, e, g) = y
    let copperAmount_first = to_copper (a, b, c)
    let copperAmount_second = to_copper (d, e, g)
    let copperResult = copperAmount_first - copperAmount_second
    copper_to_full_format_money copperResult

let (.+.) x y =
    // nice pattern
    let (a, b, c) = x
    let (d, e, g) = y
    
    let cooperSum = c + g
    let copperAmount = cooperSum % copper_per_silver
    let silverSum = b + e + cooperSum/copper_per_silver
    let silverAmount = silverSum % silver_per_gold
    let goldAmount = a + d + silverSum/silver_per_gold
    (goldAmount, silverAmount, copperAmount)

let (.+) x y = 
    let (a, b) = x
    let (c, d) = y
	(a + c, b + d)
	
	
let (.-) x y = 
    let (a, b) = x
    let (c, d) = y
	(a - c, b - d)
	
let (.*) x y = 
    let (a, b) = x
    let (c, d) = y
	(a*c - b*d, b*c + a*d)

let (./) x y =
    let (yr, yi) = y
    let y2 = (yr*yr + yi * yi)
    x .* (yr/y2, - yi/y2)
