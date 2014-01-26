//Problem 3

//The prime factors of 13195 are 5, 7, 13 and 29.
//What is the largest prime factor of the number 600851475143 ?

module LargestPrimeFactor

//TODO : handle large numbers

let isPrime n = 
    let sqrtn = (int)(System.Math.Floor(System.Math.Sqrt((float)n)))
    [|2..sqrtn|]
        |> Seq.exists(fun x-> n % x = 0)
        = false

let largestPrimeFactor n = 
    let highestPossibleFactor = (int)(System.Math.Floor((float)n/2.0));
    [|2..highestPossibleFactor|]
        |> Seq.sortBy(~-)
        |> Seq.find(fun x-> n%x = 0 && isPrime(x))