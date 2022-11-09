
Last weekend was GDCR (Global Day of CodeRetreat).

This was my first physical visit since three years ago. I was looking forward to it.

It was smaller event compared to my last visits, but not less interesting. On the contrary. With ~50 or maybe less people it felt more familial. Thank you for hosting.

To give a quick round up of what GDCR is: it is a day of learning. On this day, which is happening across the globe, you practice Test-Driven Development. Usually there are 4 or 5 session with each 45 minutes followed by a retro session. Every year, and in all sessions you implement Conway's Game of Life. You might think, how boring is that. In fact this thought also crosses my mind each time. But each time I realize that it is all but boring. Why is it not boring? A few things: for each session you pair with someone else. Each session you may use a different set of technologies (the visitor has to prep a laptop with ready tooling). This leads to different discussions in each session. Also, each session has slightly different constrains.

Let me explain a bit more about those really great 4 sessions I had:

To find pairs for the first session the strategy normally is to have all people stand in a row and let them sort themselves for the amount of years of experience level in TDD. With 6-7 years I was more on the experienced side. Sometimes there are people doing this far longer, like this time there was one guy doing TDD since 2005. Then this line of people is folded in the middle so that for the first pair more experienced go together with less experienced.

**The first**

What a coincidence. Two Lispers got together on the first pair. :)
I think of all those ~50 people there were exactly two Lispers and those two got together for the first session. How is that.

So we were able to choose from Common Lisp, Lisp Flavoured Erlang, Emacs Lisp and Clojure. Since all of those variants are best coded in Emacs and I haven't seen too much of Emacs Lisp yet we settled on Emacs Lisp. That allowed a simple setup: just Emacs.

The guy I paired with didn't do too much TDD. Lisps usually have extremely interactive REPLs (Common Lisp is hard to beat here) that allow a very interactive and incremental development of code in the REPL the is then just copy to a source file. So the guy had experience with that. While this is nice, and I do it as well to try some code, it is problematic for the following reasons.  
First, the code produced this way doesn't necessarily result in automated tests. Second, it's very hard to get the coverage right when writing tests after. Third, it's difficult to recreate the mind set and the context at the time of writing the production code. All the thoughts are lost which otherwise could be captured in tests as specs and documentation.

_Lessons learned_: tests provide spec and documentation. <a href="https://www.gnu.org/software/emacs/manual/html_mono/ert.html" target="_blank" class="link">ert</a> is a nice little test framework for Emacs.

**The second**

The second pairing was with someone who was more experienced in TDD, maybe equally to me. We did Scala with ScalaTest and the `AnyFunSpec` style where you do `describe` blocks with `it` children for each test. I had to realize that on the day to day work I got a bit lazy. In this session I was reminded that the strictness of TDD, to properly categorize and describe the tests is incredibly valuable.
The funny part of this session was: we were incrementally implementing the Game of Life rules. The handout contained four rules written down. After implementing those and doing the refactorings we had production code that was 2 lines long (compared to x times the test code). After creating a few more edge-case tests all tests remained green. We were looking at each other like: how is this possible? It can't be that simple code. Then the session was over. Later we realized that we indeed had forgotten a rule. However, we figured it was not part of the spec written on the handout. :)

_Lessons learned_: if there are no well described tests that capture the context and the specs it may be close to impossible to extract the spec and context from just the production code later, even with properly named function and variable names.

**Lunch time**

We had all great Pizza and talks over lunch... :)


**The third**

The third session was with someone who was an experienced developer who doesn't do TDD too much at work. Mostly he creates tests after. This was also an incredibly interesting session. We did again Scala on my box. There was another constrained: don't talk. (But we ignored it. :) With a language that at least one doesn't know it gets difficult and tend to not get too much out of the session.)  
What one was able to recognize is that in this session the tendency was to think too big. We were thinking too many steps ahead instead of just satisfying the test at hand. This happens to me as well sometimes even with TDD that you get stuck in writing prod code for many many minutes. When that happens it is likely that you get lost in details.

_Lessons learned_: try to make small increments. That's what TDD is for. Focus on the small thing at hand.

**The fourth**

I have to say the fourth session was one of the most interesting ones. The constraint was <a href="https://williamdurand.fr/2013/06/03/object-calisthenics/" target="_blank" class="link">Object Calisthenics</a>. So we should not use primitives like numbers, strings, etc., only one level of indentation, no else keywords, etc.  
If you haven't seen it yet you might think: what? how else would I do programming if not with ints, longs, strings and such. Well, it's possible. You wrap them in types, and you make comparisons on types. A language with pattern matching is handy here. But let's get a little bit more concrete:

In Game of Life rules you have to make comparisons based on the number of living neighbour cells of a cell. 'Normally' you'd have comparisons like:

```scala
// the true/false defines the new state of the cell.
// does it live or die
if (livingNeighbours < 2) return false
if (livingNeighbours == 3) return true
...
```

Now, with the given constrains we can't do that. Instead we have looked at the input data and tried to categorize it. The categorization actually is:

```scala
case class NeighbourCategory((min: Int, max: Int))
object NeighbourCategory {
  // i.e. < 2 neighbours is underpopulation.
  val UnderPopulation = NeighbourCategory((0, 1))
  val Survival = NeighbourCategory((2, 3))
  val OverPopulation = NeighbourCategory((4, 9))
  val Creation = NeighbourCategory((3, 3))
}
```

Those defined categories as Scala types define the value set of living neighbours for the comparison we have to make.

Additionally we defined the information whether a cell is alive, or dead like this:

```scala
case class CellState(value: Boolean)
object CallState {
  val Alive = CellState(true)
  val Dead = CellState(false)
}
```

Now, this allowed us to do the comparison just on those types, no numbers involved:

```scala
neighbourCategory match {
  case UnderPopulation => Dead
  case Survival => Alive
  ...
}
```

What benefits could this have?

We didn't actually touch too much the other restrictions because we just didn't get that far. Much of the value of those sessions is actually the approach and discussions of the approach.

_Lessons learned_: _not_ using primitive types allows a better understanding of the domain. The domain is clearly written, it gets explicit. A reader of the code can more easily understand what this is about.
