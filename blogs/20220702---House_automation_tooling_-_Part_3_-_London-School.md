Last post was more research and about prototyping some code related to how the serial communication can work using actors.

In this post we start the project by a first use-case. We'll do this using a methodology called "Outside-in TDD" but with a double test loop.

#### Outside-in TDD (London School)

There are a few variants of TDD. The classic one, which usually does inside-out, is called _Classic TDD_ also known as "Detroit School", because that's where TDD was invented. When you have a use-case to be developed, sometimes this is a vertical slice through the system maybe touching multiple layers, then Classic TDD starts developing at the inner layers providing the modules for the above layer.

Outside-in TDD also known as "London School" (because it was invented in London) goes the opposite direction. It touches the system from the outside and develops the modules starting at the outside or system boundary layers towards the inside layers. If the structures don't yet exist they are created by imagining how they should be and mocking the immediate inner layer modules. Outside-in is known to go well together with YAGNI (You Ain't Gonna Need It) because it creates exactly the structures and modules as needed for each use-case.

#### Double loop TDD

Here we use outside-in TDD with a double test loop, also known as Double Loop TDD.

<figure>
<img src="/static/gfx/blogs/outer-inner.png" alt="Outer-Inner" />
</figure>

Double Loop TDD creates acceptance tests on the outer test loop. This usually happens on a use-case basis. The created acceptance test fails until the use-case was fully developed. Doing this has multiple advantages. The acceptance test can verify the integration of components, acting as integration test. It can also check against regression because the acceptance criteria are high-level and define how the system should work, or do.

Double Loop TDD was first explained in detail by the book <a href="http://www.growing-object-oriented-software.com" target="_blank" class="link">Growing Object-Oriented Software, Guided by Tests</a>. This book got so well-known in the TDD practicing community that it is just known as "GOOS".

#### Let's get started

Let's excercise this.

Our first use-case is that we send a certain command to the boiler (via serial) which will instruct the boiler to send sensor data on a regular basis, like every 30 seconds.
