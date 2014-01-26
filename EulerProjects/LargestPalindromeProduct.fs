//Problem 4

//A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
//Find the largest palindrome made from the product of two 3-digit numbers.

module LargestPalindromeProduct

let endsPruned x =
    let powerOf10 = (int)(System.Math.Floor(System.Math.Log10((float)x)))
    let firstDigit = x / (int)(System.Math.Pow(10.0, (float)powerOf10));
    (x - (int)(firstDigit * (int)(System.Math.Pow(10.0, (float)powerOf10))))/10

let rec IsPalindrome x = 
    let powerOf10 = (int)(System.Math.Floor(System.Math.Log10((float)x)))
    let firstDigit = x / 10;
    if x < 10 then true
    elif(x / (int)(System.Math.Pow(10.0, (float)powerOf10)) <> x % 10) then false
    else IsPalindrome(endsPruned(x))

let largestNumberWithDigits x = 
    (int)(System.Math.Pow(10.0, (float)x) - 1.0)

let smallestNumberWithDigits x = 
    (int)(System.Math.Pow(10.0, (float)(x-1)))

let largestPalindromeWithProductOfDigits x = 
    let largestXDigitNumber = largestNumberWithDigits(x)
    let smallestXDigitNumber = smallestNumberWithDigits(x)
    seq{
        for row in [|smallestXDigitNumber..largestXDigitNumber|]|>Seq.sortBy(~-) do
            for col in [|row..largestXDigitNumber|] do
                yield row*col
        }
        |> Seq.filter(fun num -> IsPalindrome(num))
        |> Seq.max