# Advent of Code 2015 in Ada

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 7 years (out-of-order) of pain and suffering aren't enough. :grin:
Santa's snow machine isn't working, and we're going to help him do it.

* Problems in order of appearance
  * 🛗 [Day 1](#-day-1-not-quite-lisp): Not Quite Lisp
  * 🎁 [Day 2](#-day-2-i-was-told-there-would-be-no-math): I Was Told There Would Be No Math
  * 🏠 [Day 3](#-day-3-perfectly-spherical-houses-in-a-vacuum): Perfectly Spherical Houses in a Vacuum
  * 🪙 [Day 4](#-day-4-the-ideal-stocking-stuffer): The Ideal Stocking Stuffer

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

### 🪙 Day 4: The Ideal Stocking Stuffer

Santa is mining AdventCoin
"for all the economically forward-thinking little girls and boys."

In part 1, we figure out the smallest number that, when appended
to our 6-character prefix, produces a hash that starts with five 0's.

In part 2, we do the same, but for six 0's.

#### Unusual tools

* Implementing the MD5 algorithm...
* ...so I put that into a separate package.
* I'm not sure I've ever used the `Interfaces` package before;
  it's necessary for rotating and shifting.

#### Experience

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

### 🪙 Day 4: The Ideal Stocking Stuffer

Santa is mining AdventCoin
"for all the economically forward-thinking little girls and boys."

In part 1, we figure out the smallest number that, when appended
to our 6-character prefix, produces a hash that starts with five 0's.

In part 2, we do the same, but for six 0's.

#### Unusual tools

* Implementing the MD5 algorithm...
* ...so I put that into a separate package.
* I'm not sure I've ever used the `Interfaces` package before;
  it's necessary for rotating and shifting.

#### Experience

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

### 🪙 Day 4: The Ideal Stocking Stuffer

Santa is mining AdventCoin
"for all the economically forward-thinking little girls and boys."

In part 1, we figure out the smallest number that, when appended
to our 6-character prefix, produces a hash that starts with five 0's.

In part 2, we do the same, but for six 0's.

#### Unusual tools

* Implementing the MD5 algorithm...
* ...so I put that into a separate package.
* I'm not sure I've ever used the `Interfaces` package before;
  it's necessary for rotating and shifting.

#### Experience

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

### 🪙 Day 4: The Ideal Stocking Stuffer

Santa is mining AdventCoin
"for all the economically forward-thinking little girls and boys."

In part 1, we figure out the smallest number that, when appended
to our 6-character prefix, produces a hash that starts with five 0's.

In part 2, we do the same, but for six 0's.

#### Unusual tools

* Implementing the MD5 algorithm...
* ...so I put that into a separate package.
* I'm not sure I've ever used the `Interfaces` package before;
  it's necessary for rotating and shifting.

#### Experience

# 😲

umadbro?

I started off wondering how other people solved this problem;
a glance at Reddit made it clear that most seem to have used
some freely-available library's implementation of MD5.
If something like that exists for Ada, I didn't find it:
* NaCL doesn't have it;
* I couldn't get WolfSSL to build;
* Simple Components has a binding to the GnuTLS (?) implementation,
  but after my issues with WolfSSL I didn't want to try that.

So, I decided to build it myself.

I first tried implementing
[the pseudocode on Wikipedia's MD5 page](https://en.wikipedia.org/wiki/MD5#Pseudocode),
but that wasn't working, and I couldn't find any detailed work-throughs
of that implementation.
In fact, I'm fairly well convinced it's wrong.

I found [the RFC online](https://www.rfc-editor.org/rfc/rfc1321), but
it looked much more complicated than the pseudocode, so I ignored that.
(In retrospect, that may have been a msitake.)

I found
[a Python implementation at StackOverflow](https://stackoverflow.com/a/34230330/4526030)
and made a lot of headway through translating it to Ada
when it occurred to me to test it, and it turned out to be incorrect.

Fortunately, it linked to a correct JavaScript implementation,
which I tested _first_ this time, verifying correctness.
Translating that took a bit of effort, but wasn't too hard.

Once that was out of the way and tested, 
I wrote a solution for the puzzle that worked on the first try.
-- More than 12 hours later!!!
-- But not 12 hours of work; I spent several hours with family.