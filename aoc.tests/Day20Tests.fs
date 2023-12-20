module Day20Tests

open Xunit

[<Fact>]
let ``Day 20-1 sample 1`` () =
    let res = Day20.part1 @"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"
    Assert.Equal(32000000, res)

[<Fact>]
let ``Day 20-1 sample 2`` () =
    let res = Day20.part1 @"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"
    Assert.Equal(11687500, res)

[<Fact>]
let ``Day 20-1 sequence of conjunctions`` () =
    let res = Day20.part1 @"broadcaster -> a, b
%a -> b
&b -> c
&c -> d
&d -> output"
    Assert.Equal(24750000, res)

[<Fact>]
let ``Day 20-1 sequence of conjunctions 2`` () =
    let res = Day20.part1 @"broadcaster -> a, b
%a -> b, c
&b -> c, d
&c -> d
&d -> output"
    Assert.Equal(71997996, res)

[<Fact>]
let ``Day 20-1 sequence of flip flops`` () =
    let res = Day20.part1 @"broadcaster -> a, b
%a -> con
%b -> con
&con -> output"
    Assert.Equal(11250000, res)

[<Fact>]
let ``Day 20-2 sample`` () =
    let res = Day20.part2 @""
    Assert.Equal(0, res)
