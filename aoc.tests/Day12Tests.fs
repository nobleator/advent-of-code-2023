module Day12Tests

open Xunit

[<Literal>]
let sample = @"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

[<Fact>]
let ``Day 12-1 sample`` () =
    let res = Day12.part1 sample
    Assert.Equal(21, res)

[<Fact>]
let ``Day 12-1 sub-sample 1`` () =
    let res = Day12.part1 @"???.### 1,1,3"
    Assert.Equal(1, res)

[<Fact>]
let ``Day 12-1 sub-sample 2`` () =
    let res = Day12.part1 @".??..??...?##. 1,1,3"
    Assert.Equal(4, res)

[<Fact>]
let ``Day 12-1 sub-sample 3`` () =
    let res = Day12.part1 @"?#?#?#?#?#?#?#? 1,3,1,6"
    Assert.Equal(1, res)

[<Fact>]
let ``Day 12-1 sub-sample 4`` () =
    let res = Day12.part1 @"????.#...#... 4,1,1"
    Assert.Equal(1, res)

[<Fact>]
let ``Day 12-1 sub-sample 5`` () =
    let res = Day12.part1 @"????.######..#####. 1,6,5"
    Assert.Equal(4, res)

[<Fact>]
let ``Day 12-1 sub-sample 6`` () =
    let res = Day12.part1 @"?###???????? 3,2,1"
    Assert.Equal(10, res)

[<Fact>]
let ``Day 12-1 minimal`` () =
    let res = Day12.part1 @"#?? 1,1"
    Assert.Equal(1, res)
