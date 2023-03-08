Things went not as planned. But that's what plans are for, right? To change them.

I've planned to do small blog entries for the micro steps taken in development. I've created tags for many those steps in Git. But unfortunately due to private and business matters I couldn't find the time to do what and how I wanted.

But, I finished the tooling, and it is in production in my home doing its job, running 24/7 since a few months successfully. To recall, this tool captures temperatur/sensor data from a wood chip boiler and reports it to an openHAB system.  
I've added a few additions to the original spec, i.e. I found it important to calculate averages of the captured values and report them additionally at specified time intervalls. See below for more info.

So, I'd like to finalize this blog series by writing about some best practices that I used as well as some obstacles I had to solve.

Again, the project can be seen here: <a href="https://github.com/mdbergmann/cl-etaconnector" class="link" target="_blank">cl-eta</a>

#### Noteworthy

##### Sento (cl-gserver) works. Let's use more of actors

This was basically the first real world project that uses <a href="https://github.com/mdbergmann/cl-gserver" target="_blank" class="link">Sento</a>.  
There was one change I had to make to satisfy proper testing capability of the actor in use. That change required to ensure that the actor-system is fully shutdown and all actors stopped at the end of a test (as the last part of a test fixture). Actors not fully closed at the end of a test can interfere with the next test and can produce weird test results that are hard to debug.

Aside of that Sento works well. The tool uses one actor that exposes the complete public functions interface. While internally code is structured in multiple modules all functions are driven by messaging the actor. From opening the serial, reading and writing from/to serial, generating averages to reporting those values to openHAB.

The testing also works well. Even though Sento has no sophisticated test support like i.e. Akka has (TestKit) I think that this is not necessary. Sento is simple enough to allow exhaustive testing. Of course, since actors are (or can be) asynchronous, one has to probe for responses or state repeatedly.


##### Switching serial library

If you read the first blog post of the series I've settled on a serial library <a href="https://github.com/jetmonk/cl-libserialport" class="link" target="_blank">cl-libserialport</a>. As it turned out, this library had a serious memory leak. After 1-3 days things stopped working and I had to restart the REPL. I've reported the issue to the maintainer (but unfortunately I wasn't able to test the fix). With some minor adaptions I could switch to <a href="https://github.com/snmsts/cserial-port" class="link" target="_blank">cserial-port</a>. This since works well.


##### Fix cl-mock with multi-threading support

If you look at the tests where I do Outside-In TDD a lot, I used mocking extensively. However, <a href="https://github.com/Ferada/cl-mock/" class="link" target="_blank">cl-mock</a> didn't work well in multi-threaded environments. Function invocations were not properly captures when executed in a different threat than the test runner threat. But I was able to fix this issue and cl-mock now has multi-threading support. I think it's the only CL mocking library that has that.


##### Integration test using easy-routes

Eventually I was eager to add proper integration tests that can test also the HTTP reporting to openHAB. So I set up <a href="https://github.com/mmontone/easy-routes" class="link" target="_blank">easy-routes</a>, a REST routing framework based on Hunchentoot server. This library is easy and has a nice DSL. See the integ/acceptance <a href="https://github.com/mdbergmann/cl-etaconnector/blob/master/test/eta-atest.lisp" class="link" target="_blank">test</a>.


##### Additional features - generate and report average values

Instead of relying on openHAB to generate averages I thought why not do this here. This thing is running all the time, all values are passing though it. So why not capture or generate average data and submit those at specifed times. So this was an additional feature which already runs successfully in production. I've used <a href="https://github.com/ciel-lang/cl-cron" class="link" target="_blank">cl-cron</a>, a simple cron library to specify when and in which intervals average values are to be reported. This can be daily, weekly or so.

##### Testing

Of course the project was implemented using TDD and partially Outside-In TDD. Without having run a coverage tooling I'd say that the coverage should be very high. Testing asynchronous operations is not as straight forward as normal function/method calls. But it's not only that. The actor in my case did a fair bit of side-effects where a result can't be captured as message response. Even if some parts of the program were straight modules/packages with just pure functions, they were called as a side-effect from the higher-level business logic implemented as actor. In this case you can only verify and control what the business-logic does by setting up mocks that capture how the business-logic module 'drives' the subordinate modules. Sometimes people tend to confuse this with testing implementation detail. But this is not the case. It just verifies and controls the in- and output of the unit under test.