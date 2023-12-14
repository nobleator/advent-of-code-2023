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
    Assert.Equal(21L, res)

[<Fact>]
let ``Day 12-1 sub-sample 1`` () =
    let res = Day12.part1 @"???.### 1,1,3"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-1 sub-sample 2`` () =
    let res = Day12.part1 @".??..??...?##. 1,1,3"
    Assert.Equal(4L, res)

[<Fact>]
let ``Day 12-1 sub-sample 3`` () =
    let res = Day12.part1 @"?#?#?#?#?#?#?#? 1,3,1,6"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-1 sub-sample 4`` () =
    let res = Day12.part1 @"????.#...#... 4,1,1"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-1 sub-sample 5`` () =
    let res = Day12.part1 @"????.######..#####. 1,6,5"
    Assert.Equal(4L, res)

[<Fact>]
let ``Day 12-1 sub-sample 6`` () =
    let res = Day12.part1 @"?###???????? 3,2,1"
    Assert.Equal(10L, res)

[<Fact>]
let ``Day 12-1 minimal`` () =
    let res = Day12.part1 @"#?? 1,1"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-2 sample`` () =
    let res = Day12.part2 sample
    Assert.Equal(525152L, res)

[<Fact>]
let ``Day 12-2 sub-sample 1`` () =
    let res = Day12.part2 @"???.### 1,1,3"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-2 sub-sample 2`` () =
    let res = Day12.part2 @".??..??...?##. 1,1,3"
    Assert.Equal(16384L, res)

[<Fact>]
let ``Day 12-2 sub-sample 3`` () =
    let res = Day12.part2 @"?#?#?#?#?#?#?#? 1,3,1,6"
    Assert.Equal(1L, res)

[<Fact>]
let ``Day 12-2 sub-sample 4`` () =
    let res = Day12.part2 @"????.#...#... 4,1,1"
    Assert.Equal(16L, res)

[<Fact>]
let ``Day 12-2 sub-sample 5`` () =
    let res = Day12.part2 @"????.######..#####. 1,6,5"
    Assert.Equal(2500L, res)

[<Fact>]
let ``Day 12-2 sub-sample 6`` () =
    let res = Day12.part2 @"?###???????? 3,2,1"
    Assert.Equal(506250L, res)

[<Fact>]
let ``Day 12-2 minimal`` () =
    let res = Day12.part2 @"#?? 1,1"
    Assert.Equal(1L, res)