module Day01Tests

open Xunit

[<Fact>]
let ``Day 1-1 sample`` () =
    let x = @"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
    let res = Day01.calibrate x
    Assert.Equal(142, res)

[<Fact>]
let ``Day 1-2 sample`` () =
    let x = @"two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"
    let res = Day01.calibrate x
    Assert.Equal(281, res)
    
[<Fact>]
let ``Day 1-2 overlapping numbers`` () =
    let x = @"nineight
eighthree"
    let res = Day01.calibrate x
    Assert.Equal(181, res)
