#### Modern programming

Modern Programming is programming that is guided by tests and executed in small/micro steps,  incremental and reversible by checking in (VC) each successful test. No production code is produced without a test. With small steps is not meant the small (once/twice a day) steps used for Continuous Integration but really steps that are in the range of a few minutes, with a lot of THINKing between them and maybe pairing with a peer to bounce ideas.

This stems from decades and years of experience in the agile and craftsman ship movement and communities.  
It makes optimal use of tests as a tool where the tests guide the creation and structuring of code while providing immediate feedback, raising the quality bar for maintainable code and highly reduces the defect rate. Concentrating on small steps reduces the immediate mental load. And as a side-effect the tests provide a high test coverage.  
Tests done right also act as documentation and example for how to use the created code.

However, wielding this tool in this way is not easy. There are many intricasies that are important to successfully apply this (which may take years to fully master). It requires control of your workflow. Who doesn't know how easy it is to get carried away and do too many changes at once (when you lost control). It requires knowledge of what good design is to refactor the design when the test code gives indications of bad design.

##### When this is modern, what is not modern then?

Everything else. There is no standard but many variants.

I would go through a few examples of experienced variants. This should be the norm in most companies in slight variations.

When I started working at my first employer during studies around the year 2000 and beyond automated tests in any incarnation effectively did not exist. I've written code and then tested all 'by hand' either alone or in the team until 'it worked'. Since I did work on micro controllers these days this was very time consuming. At that time I didn't know how to abstract and design code in a way to allow most of it to be unit tested and reduce manual testing even on hardware to a minimum.

This way of coding continued for 5 or 6 years also on various other platforms like macOS (Mac OSX at that time), Windows .NET, Java. After that I've experienced a variant where I thought that it might be good to at least partially write a few tests for code areas that would be good to know they work, because testing those manually was extremely inconvenient and time consuming and QA too often came back with findings that could have been caught. Those tests were not part of an automated suite. They were just run on demand. The tests did their work and I was impressed by their effectiveness. But still there was a lot of manual testing.

The next variant was in a phase where code quality and tests were more important. But tests were still an aftermath. I was working at a banking enterprise at that time. Tests were considered important, but were done after the fact and were not enforced. So you were developing code for 3 days and then writing tests for 3 days to back that code that you did write earlier. Quite unsatisfactory for my taste, and again, quite a waste of time because many parts probably have been manually tested already during development. Yes, those tests still have their advantage. Yet, it is likely that while writing those tests it turned out that they are hard to write and the code needs to be refactored partially to make it better testable.

When production code is written without the immediate feedback of a test it is very likely that the code ends up being difficult to test. Code that is difficult to test is difficult to maintain. The tests have this advantage to give a feedback if code is too coupled, uses too many collaborators, uses different levels of abstraction, etc. But it requires some skill and experience to listen to this feedback and use it for the better.

##### Returning to the light...

Another variant, that I did experience (and am still on my way to mastery on this) in the last 5 to 7 years, is that of the TDD world. I think Kent Beck isn't so lucky with naming his inventions (XP (eXtreme Programming) could be more popular if it had a different name, he said this himself some time ago). I think a better expression of what TDD is could be: **"Development Guided by Tests"** (thanks Allan Holub for coming up with this). The tight workflow of TDD is something Kent Beck invented. When done right (and that takes a bit of practice) it can unfold all those attributes that I mentioned in the first paragraphs.

The <a href="http://manifesto.softwarecraftsmanship.org" class="link" target="_blank">craftspeople</a> all adopted this way of working to raise the quality bar of software.

During the last few years some additions to classic TDD were invented. I.e. there is the "London school" of TDD which advocates outside-in development. There is also ATDD (Acceptance TDD) which is similar to the double test loop TDD (that I <a href="/blog/Test-driven+Web+application+development+with+Common+Lisp" class="link" target="_blank">blogged</a> about).

Today all of those variants of programming are still in use. Companies of all sizes do one or the other variant, or a mixture. Often it's up to the developer.

Some references you might find interesting.

Books:

- <a href="https://www.goodreads.com/book/show/4268826-growing-object-oriented-software-guided-by-tests" class="link" target="_blank">Growing Object-Oriented Software, Guided by Tests</a>
- <a href="https://www.goodreads.com/book/show/387190.Test_Driven_Development" class="link" target="_blank">Test-Driven Development: By Example</a>
- <a href="https://www.goodreads.com/book/show/3735293-clean-code" class="link" target="_blank">Clean Code: A Handbook of Agile Software Craftsmanship</a>

Videos:

There are many. Those in particular are interesting:

- Sandro Mancuso: <a href="https://www.youtube.com/watch?v=KyFVA4Spcgg" class="link" target="_blank">Does TDD Really Lead to Good Design?</a>
- Ian Cooper: <a href="https://www.youtube.com/watch?v=EZ05e7EMOLM" class="link" target="_blank">TDD, Where Did It All Go Wrong</a>
