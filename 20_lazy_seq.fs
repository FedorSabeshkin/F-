let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

let fac_seq_raw = Seq.initInfinite (
    fun i -> 
        // определяем рекурсивную функцию
        let rec iter n acc =
            if n <= 1 then acc
            // используем хвостовую рекурсию сразу вычисляя следующее значение аккамулятора перед 
            // передачей его в функцию
            else iter (n - 1) (n * acc)
        // вызываем рекурсивную функцию с дефолтным значением аккамулятора  = 1.    
        iter i 1
)

let fac_seq = Seq.cache fac_seq_raw

let seq_seq = Seq.initInfinite (fun n -> if n % 2 = 0 then n/2 else (-1) * (n + 1)/2)
