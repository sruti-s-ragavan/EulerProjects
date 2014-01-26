// Problem 1

// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.


module MultiplesOf3And5

    let sumMultiplesOf3And5 = 
        [|1 .. 999|]
            |> Seq.filter (fun num -> num % 3 = 0 || num % 5 = 0 )
            |> Seq.sum