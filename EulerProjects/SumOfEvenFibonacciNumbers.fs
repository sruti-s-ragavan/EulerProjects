module SumOfEvenFibonacciNumbers

//finds sum of even fibonacci terms until term is <= 4 million

    let sumOfFibonacciEvenTermsUpto4Million = 
        Seq.unfold (fun state -> 
            if (snd state > 4000000) then None
            else Some(fst state + snd state, (snd state, fst state + snd state))) (1,2)
            |> Seq.filter(fun term -> term % 2 = 0)
            |> Seq.sum