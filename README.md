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
  * 🔐 [Day 11](#-day-11-corporate-policy): Corporate Policy
  * 🟥 [Day 12](#-day-11-jsabacusframeworkio): JSAbacusFramework.io
  * 🍽️ [Day 13](#️-day-13-knights-of-the-dinner-table): Knights of the Dinner Table
  * 🦌 [Day 14](#-day-14-reindeer-olympics): Reindeer Olympics
  * 🍪 [Day 15](#-day-15-science-for-hungry-people): Science for Hungry People
  * 🔬 [Day 16](#-day-16-aunt-sue): Aunt Sue
  * 🥛 [Day 17](#-day-17-no-such-thing-as-too-much): No Such Thing as Too Much
  * 💡 [Day 18](#-day-18-like-a-gif-for-your-yard): Like a GIF For Your Yard
  * 💉 [Day 19](#-day-19-medicine-for-rudolph): Medicine for Rudolph
  * ♾️ [Day 20](#️-day-20-infinite-elves-and-infinite-houses): Infinite Elves and Infinite Houses
  * 🎮 [Day 21](#-day-21-rpg-simulator-20xx): RPG Simulator 20XX
  * 🧙 [Day 22](#-day-22-wizard-simulator-20xx): Wizard Simulator 20XX
  * 🔒 [Day 23](#-day-23-opening-the-turing-lock): Opening the Turing Lock
  * ⚖️ [Day 24](#️-day-24-it-hangs-in-the-balance): It Hangs in the Balance
  * ❄️ [Day 25](#️-day-25-let-it-snow): Let It Snow

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
* Like Day 09, one function solves both parts, but I didn't make it generic.
* I implemented the
  [cosmological decay](https://en.wikipedia.org/wiki/Look-and-say_sequence#Cosmological_decay)
  optimization.
* I later revised the cosmological decay optimization to be far, far more efficient:
  instead of retaining a list of atoms, it now retains a count of atoms.

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

### 🔐 Day 11: Corporate Policy

Santa need a new password. Current policy is already insecure,
and a new Security-Elf is determine to make it even more insecure.

(Admittedly, the Puzzle Master doesn't put it quite that way.)

In part 1, you find the next password that meets all the insecurity requirements.

In part 2, you find the one after that.

#### Unusual tools

None in particular.

#### Experience

Fun and easy. Wow, I'm about halfway through the puzzles,
and only Day 4 has been a problem.

### 🟥 Day 12: JSAbacusFramework.io

The elves need help balancing their books.
Of course these books are in JSON format of course.

In part 1, you sum all the numbers that appear.

In part 2, you sum all the numbers that appear in object that **do not**
have a "red" property.

#### Unusual tools
I was tempted to use the JSON alire crate, but I resisted.
Instead, I just read through the file to obtain the numbers for part 1,
then parsed it recursively for part 2, jettisoning intermediate computations
whenever a red object was detected.
That's pretty easy since it always looks like the string `":red"`:
the input is one line, with no spaces.

#### Experience
Fun, not quite easy.
As noted, I briefly considered using the JSON crate, then decided against it.
So I had to write a recursive descent parser for braces, which wasn't too hard,
but wasn't trivial.
In fact, my first implementation returned the wrong value from `Balance`
(it balances braces): since it always increments,
and the function was supposed to return the position of `}`,
the final position value was one _past_ the `}`,
and I wasn't taking that into account.

### 🍽️ Day 13: Knights of the Dinner Table

You need to seat your family at the dinner table for the feast,
but each member likes some members more than others.
You want to maximize their happiness.

In part 1, you seat everyone except you.

In part 2, you seat yourself, as well.

#### Unusual tools

* Recursion
* All functions work for both parts,
  with different solution activated by parameter.

#### Experience

Fun, not quite easy. I kept getting part 1 wrong, even when I was doing part 2!
* I forgot to consider both directions of happiness:
  if A sits next to B, you can't just consider whether A is happier or sadder,
  but also B.
* I forgot to consider that the table is a circle:
  you have to consider the first and last elements, as well.
* `Read_Input` wasn't properly checking for negatives.
* When adapting for part 2, I didn't take into account
  that the test for the base of recursion is now different in part 1
  than it was before.

Despite that, I solved both puzzles in 30 minutes.

### 🦌 Day 14: Reindeer Olympics

The reindeer are having a race. Sometimes they fly; sometimes they rest.

In part 1, you help Santa determine which reindeer flies furthest after 2,503 seconds.

In part 2, you help Santa determine which reindeer has held the lead the longest.

#### Unusual tools

* All functions work for both parts, with different solution activated by parameter.

#### Experience

Fun, should have been easy, but:

> Premature optimization is the root of all evil. (Donald Knuth)

I decided to implement a "clever" approach that, instead of stepping 1 second at a time,
would step to the time when the next reindeer status changed.
It was great, but for the fact that it kept giving me the wrong answer,
because when going from the penultimate step to the final step,
I added the wrong time delta.
I missed it with the example because both reindeer are resting in the example.

After finally getting that worked out, I moved on to part 2 and discovered that
you have to step by 1 second, anyway. 🤬🤬🤬 Bye-bye, not-so-elegant solution...

### 🍪 Day 15: Science for Hungry People

Perfect your milk-dunking cookie recipe.

In part 1, you find the recipe with the highest score, rather curiously defined.

In part 2, you find the recipe with the highest score and exactly 500 calories.

#### Unusual tools

* A "double recursion" of sores. I'm not sure that's the right word, but I'm running with it.
* Brute force where I'm pretty sure an integer linear program would do the trick,
  but I don't feel like looking up my old thin binding for GLPK.

#### Experience

Once again,

> Premature optimization is the root of all evil. (Donald Knuth)

Maybe there's a clever way to do this,
but after spending more than an hour trying to come up with one that would solve Part 1,
I settled for a brute force approach that took me less than 45 minutes for both parts.

I'm not proud of it, but it works.
Judging by the solutions posted at Reddit, apparently I shouldn't be ashamed, either,
although I am curious why mine seems so much slower than others'.

### 🔬 Day 16: Aunt Sue

Aunt Sue sent you a nice gift, and you'd like to send her a thank-you note.
Trouble is, you have 500 Aunts Sue.
The gift is a machine that just so happens to tell you something about her.

In part 1, you find the aunt who matches the output.

In part 2, you find the aunt who matches the output,
now that you've actually read the manual. 🙄

#### Unuusal tools

Used `Enumeration_IO` and it was surprisingly easy, given the mixed casing.
I have to make more use of that!

#### Experience

Fun and easy.

### 🥛 Day 17: No Such Thing as Too Much

The elves ordered way too much eggnog.

In part 1, you figure out how many ways you can completely fill
a set of containers with the eggnog.

In part 2, you figure out the minimum number of containers
you can fill completely, then count the ways you can fill them.

And yes, I do indeed endorse the name of this puzzle.
If only I could drink more eggnog!

#### Unusual tools

Nothing unusual.

#### Experience

Fun fun fun! Not easy. -- Well, it _should_ have been easy,
but  I made some small mistake in part 1,
so that I gave the wrong answer twice,
while in part 2 I botched my first approach that was supposed to track
only the _number of minimal_ combinations, and in the attempt to convert
to record the successful combinations I broke my solution to part 1.

I finally got it to record the successful combinations,
so I should be able to improve it to track only the number of minimal ones,
but it's gotten late, so I'll wait until another time, if at all.

### 💡 Day 18: Like a GIF For Your Yard

It's another light show, only with fewer lights and animation.

In part 1, you play Conway's game of Life.

In part 2, you repeat, but keep the corners lit.

#### Unusual tools

Nothing unusual at this point. (Same code solves both parts.)

#### Experience

Fun and easy, but a little confusing.
The example has the corners already lit for part 2, whereas my input does not.
How does one proceed in this case: wait until the corners light up,
then keep them lit? Or manually light them?
I chose the latter and it worked out.

### 💉 Day 19: Medicine for Rudolph

We need to compound a medicine for Rudolph, who is sick.

In part 1, we calibrate a machine by determining how many distinct molecules
can be created when you perform a replacement on the medicine molecule.
(Essentially, how many new sequences can be performed in one step
from a given sequence.)

In part 2, we determine how many steps it will take
to create the medicine molecule when we start from an electron.

#### Unusual tools

Nothing unusual.

#### Experience

Fun, not easy.
* Part 1 wasn't a problem; it just took a while.
* I took a pretty stupid approach to Part 2, and when I realized
  it would take an unreasonable amount of time
  I just looked up other solutions.
  In retrospect I feel pretty stupid: I probably would have needed help anyway,
  but I really should have thought to _reduce_ the target to the source,
  rather than try to _build_ the target from the source.
* My solution to Part 2 is not the optimal solution, by the way.
  Apparently you can just work out a nice mathematical formula for it!

### ♾️ Day 20: Infinite Elves and Infinite Houses

Now the _elves_ are delivering presents.

In part 1, determine the first house to receive a certain number of presents
when infinite elves deliver 10 presents to infinite houses,
with each elf starting at his corresponding number.

In part 2, repeat when infinite elves deliver **11** presents to **50** houses,
with each elf still starting at his corresponding number.

#### Unusual tools

The solution to parts 1 and 2 is identical. That isn't really unusual by now,
but I had to be a little clever with the terminating condition for part 2.

#### Experience

Easy but a little uninspiring for Day 20.
I was expecting the solution to require something not especially straightforward,
so I botched Part 1 at first by trying to be clever:
the solution was far too slow and experienced overflow.
Turns out a straightforward, brute force solution works today.

### 🎮 Day 21: RPG Simulator 20XX

A kid needs help beating the boss in a new video game.
What equipment should he buy?

In part 1, you determine the least expensive combination of equipment
where the kid wins.

In part 2, you determine the most expensive combination of equipment
where the kid loses. (Don't ask.)

#### Unusual tools

* Ada's enumerated types with their subranges helped out a lot here!

#### Experience

Fun, not quite as easy as some others.
I made a dumb mistake on each part:
* For part 1, I first implemented the recursion
  without actually reducing the number of items after choosing one.
* For part 2, I performed some copy-pasta on the `Adjust` function.

### 🧙 Day 22: Wizard Simulator 20XX

The same kid needs help with a different game.
What spells should he cast?

In part 1, you help the kid determine the least amount of "mana" required
to win the easy mode.

In part 2, you repeat with the hard mode.

#### Unusual tools

* `Ada.Containers.Unbounded_Priority_Queue` finally came through
* Pretty sure this is the first 2015 puzzle where I used Breadth-First Search.

#### Experience

Fun, but difficult. A lot of details here and there that kept tripping me up.
In fact, I even managed to get the correct answer for both parts 1 and 2
despite having a bug in the program.
I might not have solved it even as early as I did without
[this Python solution](https://www.reddit.com/r/adventofcode/comments/3xspyl/comment/cy927kk/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button)
to help root out my misconceptions.
(FWIW I merely comparely dequeued state values.)

Various mistakes I made at one point or another:
* I either missed the statement,

  >  Effects apply at the start of both the player's turns and the boss' turns.

  or I misudnerstood it.
* I treated Magic Missile and Drain as effects, when that isn't really accurate.
  (In retrospect, I'm not entirely sure why it shouldn't work,
  but now that I have it working I don't care to try again.)
* I had the wrong value for one spell's bonuses.
* I applied a minimum of 1 damage to the boss when applying effects.
* I applied the spell configuration to the boss.
  In and of itself that's not necessarily wrong, as the boss has no spells,
  so it shouldn't affect him at all.
  Unfortunately, the effect of my implementation was that the boss ended up
  doing 0 Damage.

My solution is mysteriously "slow".
I may need to prune states that have already been observed,
but I tried that once and either I implemented it wrong
or it provided less benefit than cost!

### 🔒 Day 23: Opening the Turing Lock

A child is having trouble making sense of the output
of a program on her new computer.

In part 1, you figure out what the program's output should be.

In part 2, you figure out what the program's output would be
when one of the input parameters changes.

#### Unusual tools

None in particular.

#### Experience

Fun and easy, but it would have been less annoying
had the puzzle master specified that `jie` and `jio`
step forward one instruction when they _don't_ jump.
Granted, that's the only thing that really makes sense,
but I wasted too much time trying to see whether he specified it somewhere.

### ⚖️ Day 24: It Hangs in the Balance

Santa needs to balance the packages equally on his sleigh.
There can be more than one way to balance them,
and he needs the front to have the smallest number of packages **and**,
when there is more than one such, the smallest "quantum entanglement",
which is the product of the packages' weight.

In part 1, you help Santa divide the packages into 3 compartments.

In part 2, it's 4 compartments.

#### Unusual tools

* Custom number type because some entanglement values grew too large
  even for 64-bit values!
* Caching several values turned out to be useful here.
  (Maybe not _everything_ I cached, but certainly some of it.)

#### Experience

Fun, but not easy. The brute force approach definitely won't work here.
My input had 211,830 subsets that sum to 1/3 of the total weight,
and you can't just take any old subset, since they may not have 2 (or 3)
matching subsets:

> Only when all three groups weigh exactly the same amount
> will the sleigh be able to fly.

I dunno, maybe that doesn't matter, but when I first implemented it
without checking that, and took the grouping with
the smallest number of packages, breaking ties by smallest entanglement,
I didn't have the correct answer.
Perhaps I unknowingly fixed something later on.

I lost a bit of time on a bug that stopped me from finding
all the distributions, and on another where
I summed the remaining weights incorrectly,
but I found and fixed those pretty easily.

After that it was a matter of finding the correct way to:
* prune duplicate groupings;
* cut off invalid search paths;
* arrange the resulting subsets in such a way as to find quickly
  the smallest one to meet the requirements.

...and I worked that out on my own. My solution isn't terribly fast;
it takes a few seconds, so I added some output to indicate
its current state, but I'm happy with it.

### ❄️ Day 25: Let It Snow

Help Santa start up his weather machine.

In part 1, you determine the code necessary to start the weather machine.

Part 2 is free, as usual -- so long as you've completed the other 24 days!

#### Unusual tools

None in particular.

#### Experience

Fun and easy, if a little disappointing, in that
I was ready to use some number theory to speed through the list of values,
but there was no need! My number was too far
from where I could use Fermat's Little Theorem, and a brute force approach
terminates in less than a second, anyway.