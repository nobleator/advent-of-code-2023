module Day24Tests

open Xunit

[<Literal>]
let sample = @"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"

// let a = intersect { position=(5,5,4); velocity=(5,5,2) } { position=(5,5,5); velocity=(5,5,-2) }
// assert (a=(6.25,6.25,4.5))
// let b = intersect { position=(6,8,4); velocity=(6,7,0) } { position=(6,8,2); velocity=(6,7,4) }
// assert (b=(9,11.5,4))
// let c = intersect { position=(19,13,0); velocity=(-2,1,0) } { position=(18,19,0); velocity=(-1,-1,0) }
// assert (c=(14.333,15.333,0))
// let d = futureIntersect intersectXY { position=(19,13,30); velocity=(-2,1,-2) } { position=(18,19,22); velocity=(-1,-1,-2) }
// assert (d=(14.333,15.333,0))
// let e = futureIntersect intersectXY { position=(19,13,30); velocity=(-2,1,-2) } { position=(20,19,15); velocity=(1,-5,-3) }
// assert (e=(14.333,15.333,0))

[<Fact>]
let ``Day 24-1 sample`` () =
    let res = Day24.part1 sample (7.0, 27.0)
    Assert.Equal(2, res)

// [<Fact>]
// let ``Day 24-2 sample`` () =
//     let res = Day24.part2 sample
//     Assert.Equal(0, res)
