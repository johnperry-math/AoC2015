# Advent of Code 2015 in Ada

[<img src="ada_logo.svg" width="200">](ada_logo.svg)

Because 7 years (out-of-order) of pain and suffering aren't enough. :grin:
Santa's snow machine isn't working, and we're going to help him do it.

* Problems in order of appearance
  * 🛗 [Day 1](#-day-1-not-quite-lisp): Not Quite Lisp
  * 🎁 [Day 2](#-day-2-i-was-told-there-would-be-no-math): I Was Told There Would Be No Math
  * 🏠 [Day 3](#-day-3-perfectly-spherical-houses-in-a-vacuum): Perfectly Spherical Houses in a Vacuum
  * 🪙 [Day 4](#-day-4-the-ideal-stocking-stuffer): The Ideal Stocking Stuffer
  * 🧵 [Day 5](#-day-5-doesnt-he-have-intern-elves-for-this): Doesn't He Have Intern-Elves For This?
  * 💡 [Day 6](#-day-6-probably-a-fire-hazard): Probably a Fire Hazard
  * 🎛️ [Day 7](#️-day-7-some-assembly-required): Some Assembly Required
  * 🪗 [Day 8](#-day-8-matchsticks): Matchsticks
  * 🕸️ [Day 9](#️-day-9-all-in-a-single-night): All in a Single Night
  * #️[Day 10](#️day-10-elves-look-elves-say): Elves Look, Elves Say

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

None in particular.

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

### 😲

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

### 🧵 Day 5: Doesn't He Have Intern-Elves For This?

Santa has to decide which... _strings?!?_ ... are naughty and nice.

In part 1, you apply one rule.

In part 2, you apply some revised rules, because the the old rules
"are all clearly ridiculous." (As if these are any better...)

#### Unusual tools
After solving it once, I rewrote it so that:
* Every subprogram is a function.
* Functions used reduction expressions (`'Reduce`) when appropriate
  (an Ada 2022 feature I haven't used nearly often enough).

#### Experience
Fun and easy, especially after Day 4...


### 💡 Day 6: Probably a Fire Hazard

Santa gives you instructions for how to set up Christmas decorations.

In part 1, follow the instructions and determine how many lights are lit.

In part 2, you realize the instructions weren't written in English but in
"Ancient Nordic Elvish". Follow the instructions again and report
the total brightness.

#### Unusual tools

Nothing unusual.

#### Experience

Fun and easy. So much so that I went to Day 7, which...
well, you can read about it there.

### 🎛️ Day 7: Some Assembly Required

A kid needs help with the electrical set Santa gave him.

In part 1, you help him figure out the value of wire `a`.

In part 2, you set wire `b` to that value, then reset the other wires
and help him figure out the new value of wire `a`.

#### Unusual tools

* Used `Ada.Characters.Handling`'s `Is_Digit` function.
* Thought of using a hash map to map wires to values and/or to gates,
  but after the compiler annoyed me with a complaint that it didn't know the hash,
  I decided to hack an array.
* To keep the wire names relatively simple:
  * I always used a 2-character string.
  * For one-character wire naems, instead of using a space I used a backtick,
    which is the character right before `a`.
    This way, the array size doesn't grow too large with useless wire names.

#### Experience

Once I worked out reading the input, this was relatively easy.
In retrospect, I probably could have implemented it a little more easily,
but it works fine, so I'll leave it.

### 🪗 Day 8: Matchsticks
Santa's list apparently contains strings with control characters
to indicate formatting.

In part 1, compute the amount of memory needed for the formatted string.

In part 2, compute the amount of memory for a string with control characters
that will generate the input string.

#### Unusual tools
None in particular.

#### Experience
Fun and easy.

### 🕸️ Day 9: All in a Single Night

Santa has a new list of locations.
He needs to visit each location exactly once.

In part 1, determine the shortest route.

In part 2, determine the longest route.

#### Unusual tools
* Used the same code to solve Part 1 and Part 2,
  distinguishing the approach by a parameter, `Shortest`,
  which when true would minimize, and when false would maximize.
* After writing that up, I decided it would be interesting to make it generic.
  That gave me some trouble, since the generic
  was not capturing the global `Map`,
  so I made that a parameter.

#### Experience
Fun and easy enough that I decided to make the function generic,
as indicated above.
I'm kind of surprised the map was so relatively small: only 8 locations,
so at most 20,160 routes to check.

### #️Day 10: Elves Look, Elves Say

The elves are playing a game of [look-and-say](https://en.wikipedia.org/wiki/Look-and-say_sequence).

In part 1, figure out how long the sequence of numbers will be after 40 turns.

In part 2, figure out how long the sequence of numbers will be after 50 turns.
Also watch a video of John Conway discussing the sequence, if it interests you.

#### Unusual tools
Like Day 09, one function solves both parts, but I didn't make it generic.

#### Experience
Fun and easy.
I was expecting that I might need some trick to handle 50,
especially since the video gives introduces us to
the "atoms" of a look-and-say sequence, but that turned out to be unnecessary.

~~Still, I remain sort-of-tempted to implement the
[cosmological decay](https://en.wikipedia.org/wiki/Look-and-say_sequence#Cosmological_decay).~~

I implemented that approach later. It's _much_ faster.

**Qualifier** I accidentally typed `5` instead of `50` when I did Part 2,
so I got the wrong answer.