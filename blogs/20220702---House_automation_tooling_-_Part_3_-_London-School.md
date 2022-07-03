Last post was more research and about prototyping some code related to how the serial communication can work using actors.

In this post we start the project by a first use-case. We'll do this using a methodology called "Outside-in TDD" but with a double test loop.

#### Outside-in TDD (London School)

There are a few variants of TDD. The classic one, which usually does inside-out, is called _Classic TDD_ also known as "Detroit School", because that's where TDD was invented. When you have a use-case to be developed, sometimes this is a vertical slice through the system maybe touching multiple layers, then Classic TDD starts developing at the inner layers providing the modules for the above layer.

Outside-in TDD also known as "London School" (because it was invented in London) goes the opposite direction. It touches the system from the outside and develops the modules starting at the outside or system boundary layers towards the inside layers. If the structures don't yet exist they are created by imagining how they should be and mocking the immediate inner layer modules. Outside-in is known to go well together with YAGNI (You Ain't Gonna Need It) because it creates exactly the structures and modules as needed for each use-case.

#### Double loop TDD

We use outside-in TDD with a double test loop, also known as Double Loop TDD. 
