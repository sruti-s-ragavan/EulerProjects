// Problem 5
// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

module SmallestMultiple

let rec gcd a b = 
    if b>a then gcd b a
    elif a % b = 0 then b
    else gcd b (a%b)

let LCMOfNaturalNumbersUpto x = 
    let multiple = 1
    [|1..x|]
        |> Seq.sortBy(~-)
        |> Seq.fold(fun acc elem -> acc * (elem/gcd acc elem)) 1
