let allSubsets n k =
    let rec subsets = function
        | (m, 0) -> set [Set.empty]
        | (m, k) when k = n - m + 1 -> set [set [m .. n]]
        | (m, k) -> Set.union (subsets(m + 1, k)) (Set.map (fun s -> Set.add m s) (subsets(m + 1, k - 1)))
    subsets(1, k)
