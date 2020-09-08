This article is intended to give an introduction to creating web sites using <a href="https://common-lisp.net/" class="link">Common Lisp</a>.

I wished I had found the Common Lisp world (or the Lisp world in general) earlier. So, I just found Common Lisp somewhen in early 2019 coming from the Java/Scala ecosystem which I worked in for almost 20 years, and still work in today.
I had the opportunity to work with a few web frameworks in the Java world. From pure markup extension frameworks like JSP over MVC frameworks like <a href="https://www.playframework.com/" class="link">Play</a> or <a href="https://grails.org/" class="link">Grails</a> to component based server frameworks like <a href="https://vaadin.com/" class="link">Vaadin</a> and <a href="https://tapestry.apache.org/" class="link">Tapestry</a> until I finally have settled with <a href="https://wicket.apache.org/" class="link">Wicket</a>, which I now work with since 2008.

The frameworks I worked with are usually based on the Java Servlet technology specification (this more or less represents and is an abstraction to the HTTP server and some session handling), which they pretty much all have in common. On top of the Java Servlets are the web frameworks which all enforce certain workflows, patterns and principles. The listed frameworks are a mixture of pure view frameworks and frameworks that also provide data persistence. They provide a routing mechanism and everything needed to make the user interface (UI). Some do explicit separation according to MVC with even folder separation to 'views', 'controllers', 'models' while others do this less explicit but it still exists. Of course many of those frameworks are opinionated to some degree. But since they usually have many contributors and maintainers the opinions are flattened and less expressive.

The Common Lisp ecosystem regarding web applications is very diverse. There are many framework approaches considering the relatively small over-all community of Common Lisp. The server abstraction exists in form of a self-made opinionated abstraction layer called <a href="https://github.com/fukamachi/clack" class="link">Clack</a> which allows to use a bunch of available HTTP servers.  
Those are the frameworks I have had a look at: <a href="http://40ants.com/weblocks/" class="link">Weblocks</a>, <a href="http://borretti.me/lucerne/" class="link">Lucerne</a>, <a href="https://shirakumo.github.io/radiance/" class="link">Radiance</a>, <a href="http://8arrow.org/caveman/" class="link">Caveman2</a>.

The listed frameworks either base on Clack or directly on the defacto standard HTTP server <a href="https://edicl.github.io/hunchentoot/" class="link">Hunchentoot</a>. Pretty much all frameworks allow to define REST styled and static routes.  
I am not aware of a framework that adds or enforces MVC ('model', 'view', 'controllers'). So if you want MVC you'll have to come up with something yourself.  
The HTML generation is either based on a Django clone called <a href="https://github.com/mmontone/djula" class="link">Djulia</a> or is done using one of the brilliant HTML generation libraries for Common Lisp <a href="https://github.com/edicl/cl-who" class="link">cl-who</a> (for HTML 4) and <a href="https://github.com/ruricolist/spinneret" class="link">Spinneret</a> (for HTML 5). Those libraries are HTML DSLs that allow you to code 'HTML' as Lisp macros and hence can be type checked and debugged (if needed). Very powerful.  
I think the only framework that enforces the use of Djulia is Lucerne. The others allow you to use what you want.  
All frameworks also do some convenience wrapping of the request/response for easier access to parameters.  
The only one that creates some 'model' abstractions for views is Weblocks. The only one that adds data persistence is Caveman2. But this is just some glue code that you get as convenience. The same libraries can be used in other frameworks.

The most complete one for me seemed to Caveman2. It also sets up configuration, and creates test and production environments. But the documentation situation is worst for Caveman2 (and/or <a href="http://8arrow.org/ningle/" class="link">Ningle</a> which Caveman2 is based on). I really had a hard time finding things. The other framework documentations are better. However, since the frameworks for a large part glue together libraries it is possible to look at the documentation for those libraries directly. The documantation for Hunchentoot server, cl-who, Spinneret, etc. are sufficiently complete.

The web application we will be developing during this blog post is based on an old web page design of mine that I'd like to revive. The web application will include some static pages that are based on Markdown and are loaded from the file system. Also the web application will include a blog feature that allows to write blog posts as either HTML or Markdown files that are converted on the fly to HTML before presenting them. It is meant as an example and it shows a workflow of test-driven development that is excercised during the development.

The application started out with Caveman2, because it seemed to be the most complete framework. But due to the lack of documentation for the framework itself and also for Clack some refactorings were conducted quite early in development where pretty much all of Caveman2 was removed.

So the web application is based on the following libraries (web application relevant only):

- plain <a href="https://edicl.github.io/hunchentoot" class="link">Hunchentoot</a> (replaced Clack)
- <a href="https://github.com/joaotavora/snooze" class="link">Snooze</a> REST routing library. This library is implemented with plain CLOS and hence can be easily unit tested. I didn't find this easily possible with any other routing definitions of the other frameworks. We'll see later how this works.
- a simple self-made MVC like structure
- <a href="https://notabug.org/cage/cl-i18n" class="link">cl-i18n</a> internationalization library (I've tried a few others. You'll see later why I did stick with this one)
- <a href="https://github.com/edicl/cl-who" class="link">cl-who</a> for HTML generation, because this old web page is heavy on HTML 4.01. Otherwise I had used Spinneret.
- <a href="https://github.com/3b/3bmd" class="link">3bmd</a> for Markdown to HTML conversion.
- <a href="https://github.com/VitoVan/xml-emitter" class="link">xml-emitter</a> for generating XML. Used for the Atom feed generation.
- <a href="https://github.com/lispci/fiveam" class="link">fiveam</a> as unit test library.

The project is hosted on <a href="https://github.com/mdbergmann/cl-swbymabeweb" class="link">GitHub</a>. So you can checkout the sources yourself. The life web page is available <a href="http://retro-style.software-by-mabe.com/blog" class="link">here</a>.

We will go through some of the development interatively in a test-driven outside-in approach where we will slice vertically through the application and implement a feature with a full integration test and inner unit tests. We will have a look at the following things:

1. project setup. Though this will be minimal as we add stuff incrementally as needed. But we need a minimum to get started
2. add the imprint page
	- setup an integration test for the full feature loop
	- create unit tests for all components we touch
	- add routing for when a request touches the web server
	- delegate to a MVC controller
	- create the view with loading content from file
	- pass the generated HTML back to the web server for delivery to the client 
3. add the blog page and feature that loads blog posts from file system
	- setup integration tests for the full feature loop
	- create or extend unit tests for all components we touch
	- add or extend routing
	- create dedicated MVC controller
	- create blog post repository facility
	- create views that generate a result HTML that is delivered back to the caller

During those points we will cover many things like how to properly test using mocks, i18n, how to use cl-who, how to convert Markdown to HTML and other things.

### <a name="project_setup"></a>Project setup

This blog post, and in particular the workflow, is a smoothed version that tries to look a bit behind the scenes and actually partly carve out what a framework would or should do. I don't write about all the bumps and issues I had to deal with (or only if revelant), otherwise this would get the size of a small book (maybe I do that one day).

Since this was my first web project with Common Lisp I had to do some research for how to integrate and run the server and add routes, etc. This is where the scaffolding that frameworks like Caveman2 produce are appeciated.

But, once we know how that works we can start a project from scratch. Along the way we can create a template for future projects. (This can also be in combination with one of the mentioned frameworks.)

That means we don't have a lot of setup to start with. We create a project folder and a 'src' and 'tests' folder therein. That's it. We'll add an asdf based project/system definition as we go along.

To get started and since we use a test-driven approach we'll start with adding an integration (or acceptance) test for the first feature, the imprint page.

In order to add tests that are part of a full test suite we'll start creating an overall 'all-tests' test suite. Create a new Lisp file and add the following code and save it as 'tests/all-tests.lisp':

```
(defpackage :cl-swbymabeweb.tests
  (:use :cl :fiveam)
  (:export #:run!
           #:all-tests
           #:nil
           #:test-suite))

(in-package :cl-swbymabeweb.tests)

(def-suite test-suite
  :description "All catching test suite.")

(in-suite test-suite)
```

This will help us later when creating the asdf test system as we can point it to this 'all-tests' suite and it'll automatically run all tests of it.


### <a name="imprint"></a>Implementing a page

#### <a name="imprint-outer_test_loop"></a>The outer test loop (integration test)

We will excercise the first integration test cycle with the 'imprint' page. There is not much content on this page. Just some text to be displayed. But we want to make sure that all components involved with serving this page are properly integrated and are operational. So let's create a new Lisp file, save it as 'tests/it-routing.lisp' and add the following code:

```
(defpackage :cl-swbymabeweb-test
  (:use :cl :fiveam)
  (:local-nicknames (:dex :dexador))
  (:import-from #:cl-swbymabeweb
                #:start
                #:stop))
(in-package :cl-swbymabeweb-test)

(def-suite it-routing
  :description "Routing integration tests."
  :in cl-swbymabeweb.tests:test-suite)

(in-suite it-routing)

(def-fixture with-server ()
  (start :address "localhost")
  (sleep 0.5)
  (unwind-protect 
       (&body)
    (stop)
    (sleep 0.5)))

(test handle-imprint-route
  "Test integration of imprint."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Imprint"
                       (dex:get "http://localhost:5000/imprint")))))
```

Let's go though it quickly. It creates a new test package and a new test suite. The `:in cl-swbymabeweb.tests:test-suite` adds this test suite to the 'all-tests' test suite that we've created before.

The test `handle-imprint-route` is a full cycle integration test that uses dexador HTTP client to run a request against the server and expect a certain page title which must be part of the result HTML. Of course, more assertions should be added to make this a proper acceptance test.  
Since fiveam does not support 'before' or 'after' setup/cleanup functionality we have to workaround this using a fixture that is defined by `def-fixture`. The fixture will `start` and `stop` the HTTP server and in between run code that is the body of `with-fixture`. We also want to wrap all in `unwind-protect` in order to force shutting down the server even if the `@body` raises an error which would otherwise unwind the stack and the HTTP server would keep running which had consequences on the next test we run.

Now, as part of adding this test we define a few things that don't exist yet. For example do we define a package called `cl-swbymabeweb` where we import `start` and `stop` from. Those `start` and `stop` functions obviously do start and stop the web server, so the package `cl-swbymabeweb` should be an application entry package that does those things.  
This is part of what tests-first and TDD does, it is the first user of production code and hence defines things how they should be from an API user perspective.

When evaluating this buffer/file (I use `sly-eval-buffer`) we realize (from error messages) that there are some missing packages. So in order to at least get this compiled we have to load the dependencies using `quicklisp`. Here this would be `:dexador`, `:fiveam` and `:str` (string library).  
We also have to create the defined package `cl-swbymabeweb` and add stubs (for now) for the `start`and `stop` functions. That's what we do now. Create a new buffer, add the following code to have the minimum code to make the integration test compile, evaluate and save it under 'src/main.lisp'.

```
(defpackage :cl-swbymabeweb
  (:use :cl)
  (:export #:start
           #:stop))

(in-package :cl-swbymabeweb)

(defun start (&key address))
(defun stop ())
```

We can now go into the test package by doing `(in-package :cl-swbymabeweb-test)` and run the test where we will see the following output:

```
CL-SWBYMABEWEB-TEST> (run! 'handle-imprint-route)

Running test HANDLE-IMPRINT-ROUTE X
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 HANDLE-IMPRINT-ROUTE in IT-ROUTING [Test integration of imprint.]: 
      Unexpected Error: #<USOCKET:CONNECTION-REFUSED-ERROR #x30200389ACBD>
Error #<USOCKET:CONNECTION-REFUSED-ERROR #x30200389ACBD>.
 --------------------------------
```

So, of course. Dexador is trying to connect to the server, but there is no server running. The `start`/`stop` functions are only stubs. This is OK. It is expected.

<a name="imprint-start_the_server"></a>*Start the server, for real*

In order for the integration test to do it's job and test the full integration we still have a bit more work to do here before we move on. The HTTP server should be working at least. Now, let's do that:

Add the following to 'main.lisp' on top of the `start` function:

```
(defvar *server* nil)
```

For the `start` function we'll change the signature like this in order to be able to also specify a different port: `&key (port 5000) (address "0.0.0.0")`. Finally we'll now start the server like so in `start`:

```
(defun start (&key (port 5000) (address "0.0.0.0") &allow-other-keys)
  (log:info "Starting server.")
  (when *server*
    (log:info "Server is already running."))
  (unless *server*
    (setf *server*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :address address))    
    (hunchentoot:start *server*)))
```

This code will make sure that there is no server instance currently being set and if not it will create a server instance and start it.

As a general dependency we use `log4cl`, a logging framework. So we'll also add this to the package `:use` directive.

The `stop` function can be implemented like this:

```
(defun stop ()
  (when *server*
    (log:info "Stopping server.")
    (prog1
        (hunchentoot:stop *server*)
      (log:debug "Server stopped.")
      (setf hunchentoot:*dispatch-table* nil)
      (setf *server* nil))))
```

After 'quickloading' `log4cl` and `hunchentoot` and running the test again we will see the following output instead:

```
CL-SWBYMABEWEB-TEST> (run! 'handle-imprint-route)

Running test HANDLE-IMPRINT-ROUTE 
 <INFO> [21:35:21] cl-swbymabeweb (start) - Starting server.
::1 - [2020-09-07 21:35:22] "GET /imprint HTTP/1.1" 404 339 "-" 
"Dexador/0.9.14 (Clozure Common Lisp Version 1.12  DarwinX8664); Darwin; 19.6.0"
X
 <INFO> [21:35:22] cl-swbymabeweb (stop) - Stopping server.
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 HANDLE-IMPRINT-ROUTE in IT-ROUTING [Test integration of imprint.]: 
      Unexpected Error: #<DEXADOR.ERROR:HTTP-REQUEST-NOT-FOUND #x3020032527FD>
An HTTP request to "http://localhost:5000/imprint" returned 404 not found.

<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>
The requested URL /imprint was not found on this server.<p><hr><address>
<a href='http://weitz.de/hunchentoot/'>Hunchentoot 1.3.0</a> 
<a href='http://openmcl.clozure.com/'>
(Clozure Common Lisp Version 1.12  DarwinX8664)</a> 
at localhost:5000</address></p></body></html>.
 --------------------------------
```

This looks a lot better. The test still fails, which is good and expected. But the server works and responds with `404` for a request to `http://localhost:5000/imprint`.

The test will fail until the server responds with the proper page title. In order to have the right page title we'll still have some work to do. So the right thing now is to move on to the smaller components, develop them in the similar style (tests-first, TDD) only that the unit tests for those components should all pass.

<a name="imprint-asdf_system"></a>*The ASDF system*

But before we do that, and since we still have in mind what we did to make this all work so far we should setup an asdf system that we'll add to and expand as we go along.

So create a new buffer/file, save it as 'cl-swbymabeweb.asd' in the root folder of the project and add the following:

```
(defsystem "cl-swbymabeweb"
  :version "0.1.1"
  :author "Manfred Bergmann"
  :depends-on ("hunchentoot"
               "uiop"
               "log4cl"
               "str")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-swbymabeweb/tests"))))


(defsystem "cl-swbymabeweb/tests"
  :author "Manfred Bergmann"
  :depends-on ("cl-swbymabeweb"
               "fiveam"
               "dexador"
               "str")
  :components ((:module "tests"
                :components
                ((:file "all-tests")
                 (:file "it-routing" :depends-on ("all-tests"))
                 )))
  :description "Test system for cl-swbymabeweb"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (uiop:find-symbol* '#:test-suite
                                                    '#:cl-swbymabeweb.tests))))
```

This defines the necessary ASDF system and test system to fully load the project to the system so far. When the project is in a folder where asdf can find it (like `~/common-lisp`) then it can be loaded into the image by:

```
;; load (and compile if necessary) the production code
(asdf:load-system "cl-swbymabeweb")

;; load (and compile if necessary) the test code
(asdf:load-system "cl-swbymabeweb/tests")

;; run the tests
(asdf:test-system "cl-swbymabeweb/tests")
```

Notice `test-system` vs. `load-system`. Since Common Lisp is image based, ASDF is a facility that can load a full project into the CL image. Keeping the system definition up-to-date is a bit combersome because loading the system must be performed on a clean image to really see if it works or not and if all dependencies are named proper. This is something that must be tried manually on a clean image. I usually do this by issuing `sly-restart-inferior-lisp` with loading the system, test system and finally testing the test system. But otherwise it is quite easy to continue working on a project which is merely just:

1. open Emacs
2. run Sly/Slime REPL
3. `load-system` (also the test system if tests should be run) of the project to work on.


#### <a name="imprint-inner_test_loops"></a>The inner test loops

Now we will move on to the inner components. The first component that is hit by a request is the routing. We have to define which requests, request paths are handled by what and how. As mentioned earlier most frameworks come with a routing mechnism that allows defining routes. We will use <a href="https://github.com/joaotavora/snooze" class="link">Snooze</a> for this. The difference between Snooze and other URL router frameworks is more or less that routes are defined using plain Lisp functions in Snooze and HTTP conditions just Lisp conditions. The author says: _"Since you stay inside Lisp, if you know how to make a function, you know how to make a route. There are no regular expressions to write or extra route-defining syntax to learn."_. The other good thing is that the routing can be easily unit tested.

##### URL routing

