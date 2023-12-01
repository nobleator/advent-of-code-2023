module Day01Tests

open Xunit

[<Fact>]
let ``Day 1 sample`` () =
    let x = @"1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"
    let res = Day01.calibrate x
    Assert.Equal(142, res)
