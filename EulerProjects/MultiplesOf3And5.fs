// Problem 1 : Sum of multiples of 3 and 5 < 1000

module MultiplesOf3And5

    let sumMultiplesOf3And5 = 
        [|1 .. 999|]
            |> Seq.filter (fun num -> num % 3 = 0 || num % 5 = 0 )
            |> Seq.sum