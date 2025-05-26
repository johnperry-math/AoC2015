# Advent of Code 2015 in Ada

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 7 years (out-of-order) of pain and suffering aren't enough. :grin:
Santa's snow machine isn't working, and we're going to help him do it.

* Problems in order of appearance
  * 🛗 [Day 1](#-day-1-not-quite-lisp): Not Quite Lisp
  * 🎁 [Day 2](#-day-2-i-was-told-there-would-be-no-math): I Was Told There Would Be No Math
  * 🏠 [Day 3](#-day-3-perfectly-spherical-houses-in-a-vacuum): Perfectly Spherical Houses in a Vacuum

## Problems in order of appearance

### 🛗 Day 1: Not Quite Lisp

Santa's struggling to deliver presents in an apartment building,
but the directions are confusing.

In part 1, we figure out the instructions' ultimate destination.

In part 2, we figure out which instruction first takes us to the basement.

#### Unusual tools

Ada: None in particular.

SPARK: This is my first time trying to solve this using SPARK, so:

* SPARK itself
* SPARK's formal containers
* Modularizing into packages so that I can log errors.

#### Experience

Quite easy, aside from figuring out the SPARK stuff, which probably took me
longer than it did to solve Day 1 and Day 2 together...

### 🎁 Day 2: I Was Told There Would Be No Math

The elves are wrapping presents and need to know how much paper and ribbon to order.

In part 1, we figure the paper.

In part 2, we figure the ribbon.

#### Unusual tools

None in particular.

#### Experience

Quite easy.

### 🏠 Day 3: Perfectly Spherical Houses in a Vacuum

Santa is delivering presents to houses.

In part 1, we figure out how many houses receive at least one present.

In part 2, we figure out how many houses receive at least one present
when a robot helps him, so that Santa and the robot alternate instructions.

(I also have no idea what the title has to do with the puzzle.)

#### Unusual tools

Nonein particular.

#### Experience

Quite easy, even though I tried to anticipate Part 2
by mapping each house to the number of presents it received.
(I figured the next question would require some arithmetic on those numbers.)
It was a little annoying to find that no, that was _not_ the goal,
so I had to refactor the program a bit.