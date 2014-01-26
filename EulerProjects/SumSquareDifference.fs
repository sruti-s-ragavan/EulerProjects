module SumSquareDifference

let SumOfSquaresUpto x = 
    [|1..x|]
        |> Seq.sumBy(fun i -> i*i)

let SquareOfSumUpto x = 
    [|1..x|]
        |> Seq.sum
        |> fun i -> i * i


let SumOfSquaresMinusSquareOfSum x = 
    System.Math.Abs((SumOfSquaresUpto x) - (SquareOfSumUpto x))

