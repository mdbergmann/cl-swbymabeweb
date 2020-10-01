This article is intended to give an introduction to creating web sites using <a href="https://common-lisp.net/" class="link">Common Lisp</a>.

I wished I had found the Common Lisp world (or the Lisp world in general) earlier. So, I just found Common Lisp somewhen in early 2019 coming from the Java/Scala ecosystem which I worked in for almost 20 years, and still work in today.

I had the opportunity to work with a few web frameworks in the Java world. From pure markup extension frameworks like JSP over MVC frameworks like <a href="https://www.playframework.com/" class="link">Play</a> or <a href="https://grails.org/" class="link">Grails</a> to component based server frameworks like <a href="https://vaadin.com/" class="link">Vaadin</a> and <a href="https://tapestry.apache.org/" class="link">Tapestry</a> until I finally have settled with <a href="https://wicket.apache.org/" class="link">Wicket</a>, which I now work with since 2008.

The frameworks I worked with are usually based on the Java Servlet technology specification (this more or less represents and is an abstraction to the HTTP server and some session handling), which they pretty much all have in common. On top of the Java Servlets are the web frameworks which all enforce certain workflows, patterns and principles. The listed frameworks are a mixture of pure view frameworks and frameworks that also provide data persistence. They provide a routing mechanism and everything needed to make the user interface (UI). Some do explicit separation according to MVC with even folder separation to 'views', 'controllers', 'models' while others do this less explicit but it still exists. Of course many of those frameworks are opinionated to some degree. But since they usually have many contributors and maintainers the opinions are flattened and less expressive.

The Common Lisp ecosystem regarding web applications is very diverse. There are many framework approaches considering the relatively small over-all community of Common Lisp. The server abstraction exists in form of a self-made opinionated abstraction layer called <a href="https://github.com/fukamachi/clack" class="link">Clack</a> which allows to use a bunch of available HTTP servers.  
Those are the frameworks I have had a look at: <a href="http://40ants.com/weblocks/" class="link">Weblocks</a>, <a href="http://borretti.me/lucerne/" class="link">Lucerne</a>, <a href="https://shirakumo.github.io/radiance/" class="link">Radiance</a>, <a href="http://8arrow.org/caveman/" class="link">Caveman2</a>.

The listed frameworks either base on Clack or directly on the defacto standard HTTP server <a href="https://edicl.github.io/hunchentoot/" class="link">Hunchentoot</a>. Pretty much all frameworks allow to define REST styled and static routes.  
I am not aware of a framework that adds or enforces MVC ('model', 'view', 'controllers'). So if you want MVC you'll have to come up with something yourself.  
The HTML generation is either based on a Django clone called <a href="https://mmontone.github.io/djula/" class="link">Djulia</a> or is done using one of the brilliant HTML generation libraries for Common Lisp <a href="https://github.com/edicl/cl-who" class="link">cl-who</a> (for HTML 4) and <a href="https://github.com/ruricolist/spinneret" class="link">Spinneret</a> (for HTML 5). Those libraries are HTML DSLs that allow you to code 'HTML' as Lisp macros and hence can be type checked and debugged (if needed). Very powerful.  
I think the only framework that enforces the use of Djulia is Lucerne. The others allow you to use what you want.  
All frameworks also do some convenience wrapping of the request/response for easier access to parameters.  
The only one that creates some 'model' abstractions for views is Weblocks. The only one that adds data persistence is Caveman2. But this is just some glue code that you get as convenience. The same libraries can be used in other frameworks.

The most complete one for me seemed to be Caveman2. It also sets up configuration, and creates test and production environments. But the documentation situation is worst for Caveman2 (and/or <a href="http://8arrow.org/ningle/" class="link">Ningle</a> which Caveman2 is based on). I really had a hard time finding things. The other framework documentations are better. However, since the frameworks for a large part glue together libraries it is possible to look at the documentation for those libraries directly. The documantation for Hunchentoot server, cl-who, Spinneret, etc. are sufficiently complete.

The web application we will be developing during this blog post is based on an old web page design of mine that I'd like to revive. The web application will primarily be about a 'blog' feature that allows blog posts be written in HTML or Markdown as files and the application will pick them up and convert them on the fly (in case of Markdown).  
It is meant as an example for developing web pages with Common Lisp. The development workflow will be test-driven.

The application started out with Caveman2, because it seemed to be the most complete framework. But due to the lack of documentation for the framework itself and also for Clack some refactorings were conducted quite early in development where pretty much all of Caveman2 was removed.

So the web application is based on the following libraries (web application relevant only):

- a simple self-made MVC like structure
- <a href="https://edicl.github.io/hunchentoot" class="link">Hunchentoot</a> HTTP server
- <a href="https://github.com/joaotavora/snooze" class="link">Snooze</a> REST routing library. This library is implemented with plain CLOS and hence can be easily unit tested. I didn't find this easily possible with any other routing definitions of the other frameworks. We'll see later how this works.
- <a href="https://notabug.org/cage/cl-i18n" class="link">cl-i18n</a> internationalization library (I've tried a few others. You'll see later why I did stick with this one)
- <a href="https://github.com/edicl/cl-who" class="link">cl-who</a> for HTML generation, because this old web page is heavy on HTML 4.01. Otherwise I had used Spinneret.
- <a href="https://github.com/3b/3bmd" class="link">3bmd</a> for Markdown to HTML conversion.
- <a href="https://github.com/VitoVan/xml-emitter" class="link">xml-emitter</a> for generating XML. Used for the Atom feed generation.
- <a href="https://github.com/dlowe-net/local-time" class="link">local-time</a> for dealing with date and time formats. Conversions from timestamp to string and vise-versa.
- <a href="https://github.com/sharplispers/log4cl" class="link">log4cl</a> a logging library.
- <a href="https://github.com/lispci/fiveam" class="link">fiveam</a> as unit test library.

The project is hosted on <a href="https://github.com/mdbergmann/cl-swbymabeweb" class="link">GitHub</a>. So you can checkout the sources yourself. The life web page is available <a href="http://retro-style.software-by-mabe.com/blog" class="link">here</a>.

We will go through some of the development in a test-driven outside-in approach where we will slice vertically through the application and implement a feature with a full integration test and inner unit tests. We will have a look at the following things:

1. project setup. Though this will be minimal as we add stuff incrementally as needed. But we need a minimum to get started
2. add the blog page and feature that loads blog posts from file system
	- setup integration tests for the full feature loop
	- create or extend unit tests for all components we touch
	- add or extend routing
	- create dedicated MVC controller
	- create blog post repository facility
	- create views that generate a result HTML that is delivered back to the caller

During those points we will cover many things like how to properly test using mocks, how to use i18n and cl-who, how to convert Markdown to HTML and other things.

## <a name="project_setup"></a>Project setup

This blog post, and in particular the workflow, is a smoothed version that tries to look a bit behind the scenes and actually partly carve out what a framework would or should do. I don't write about all the bumps and issues I had to deal with (or only if revelant), otherwise this would get the size of a small book (maybe I do that one day).

Since this was my first web project with Common Lisp I had to do some research for how to integrate and run the server and add routes, etc. This is where the scaffolding that frameworks like Caveman2 produce are appeciated.

But, once we know how that works we can start a project from scratch. Along the way we can create a template for future projects. (This can also be in combination with one of the mentioned frameworks.)

That means we don't have a lot of setup to start with. We create a project folder and a _src_ and _tests_ folder therein. That's it. We'll add an asdf based project/system definition as we go along.

To get started and since we use a test-driven approach we'll start with adding an integration (or acceptance) test for the first feature, the blog page.

In order to add tests that are part of a full test suite we'll start creating an overall 'all-tests' test suite. Create a new Lisp file and add the following code and save it as _tests/all-tests.lisp_:

```lisp
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


## <a name="blog-feature"></a>The blog feature

We will excercise the first integration test cycle with the _blog_ page. There are a few use cases for the blog page that we will go through. The tests needs to make sure that all components involved with serving this page are properly integrated and are operational.

### <a name="blog-feature_outer-test-loop-index"></a>The outer test loop (blog index page)

Let's start with the blog index page. This page is shown when a request goes to the path _/blog_. On this path the last available blog post is to be selected.  
So let's start with the first integration test and create a new Lisp file, save it as 'tests/it-routing.lisp' and add the following code:

```lisp
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

(test handle-blog-index-route
  "Test integration of blog - index."
  (with-fixture with-server ()
    (is (str:containsp "<title>Manfred Bergmann | Software Development | Blog"
                         (dex:get "http://localhost:5000/blog")))))
```

Let's go through it quickly. It creates a new test package and a new test suite. The `:in cl-swbymabeweb.tests:test-suite` adds this test suite to the _all-tests_ test suite that we've created before.

The test `handle-blog-index-route` is a full cycle integration test that uses dexador HTTP client to run a request against the server and expect a certain page title which must be part of the result HTML. Of course, more assertions should be added to make this a proper acceptance test.  
Since fiveam does not support 'before' or 'after' setup/cleanup functionality we have to workaround this using a fixture that is defined by `def-fixture`. The fixture will `start` and `stop` the HTTP server and in between run code that is the body of `with-fixture`. We also want to wrap all in `unwind-protect` in order to force shutting down the server even if the `@body` raises an error which would otherwise unwind the stack and the HTTP server would keep running which had consequences on the next test we run.

Now, as part of adding this test we define a few things that don't exist yet. For example do we define a package called `cl-swbymabeweb` where we import `start` and `stop` from. Those `start` and `stop` functions obviously do start and stop the web server, so the package `cl-swbymabeweb` should be an application entry package that does those things.  
This is part of what tests-first and TDD does, it is the first user of production code and hence defines things how they should be from an API user perspective.

When evaluating this buffer/file (I use `sly-eval-buffer`) we realize (from error messages) that there are some missing packages. So in order to at least get this compiled we have to load the dependencies using `quicklisp`. Here this would be `:dexador`, `:fiveam` and `:str` (string library).  
We also have to create the defined package `cl-swbymabeweb` and add stubs (for now) for the `start`and `stop` functions. That's what we do now. Create a new buffer, add the following code to have the minimum code to make the integration test compile, evaluate and save it under _src/main.lisp_.

```lisp
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
CL-SWBYMABEWEB-TEST> (run! 'handle-blog-index-route)

Running test HANDLE-BLOG-INDEX-ROUTE X
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 HANDLE-BLOG-INDEX-ROUTE in IT-ROUTING [Test integration of blog - index.]: 
      Unexpected Error: #<USOCKET:CONNECTION-REFUSED-ERROR #x30200389ACBD>
Error #<USOCKET:CONNECTION-REFUSED-ERROR #x30200389ACBD>.
 --------------------------------
```

So, of course. Dexador is trying to connect to the server, but there is no server running. The `start/stop` functions are only stubs. This is OK. It is expected.

<a name="blog-feature-start_the_server"></a>*Start the server, for real*

In order for the integration test to do it's job and test the full integration we still have a bit more work to do here before we move on. The HTTP server should be working at least. Now, let's do that:

Add the following to _src/main.lisp_ on top of the `start` function:

```lisp
(defvar *server* nil)
```

For the `start` function we'll change the signature like this in order to be able to also specify a different port: `&key (port 5000) (address "0.0.0.0")`. Finally we'll now start the server like so in `start`:

```lisp
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

```lisp
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
CL-SWBYMABEWEB-TEST> (run! 'handle-blog-index-route)

Running test HANDLE-BLOG-INDEX-ROUTE 
 <INFO> [21:35:21] cl-swbymabeweb (start) - Starting server.
::1 - [2020-09-07 21:35:22] "GET /blog HTTP/1.1" 404 339 "-" 
"Dexador/0.9.14 (Clozure Common Lisp Version 1.12  DarwinX8664); Darwin; 19.6.0"
X
 <INFO> [21:35:22] cl-swbymabeweb (stop) - Stopping server.
 Did 1 check.
    Pass: 0 ( 0%)
    Skip: 0 ( 0%)
    Fail: 1 (100%)

 Failure Details:
 --------------------------------
 HANDLE-BLOG-INDEX-ROUTE in IT-ROUTING [Test integration of blog - index.]: 
      Unexpected Error: #<DEXADOR.ERROR:HTTP-REQUEST-NOT-FOUND #x3020032527FD>
An HTTP request to "http://localhost:5000/blog" returned 404 not found.

<html><head><title>404 Not Found</title></head><body><h1>Not Found</h1>
The requested URL /blog was not found on this server.<p><hr><address>
<a href='http://weitz.de/hunchentoot/'>Hunchentoot 1.3.0</a> 
<a href='http://openmcl.clozure.com/'>
(Clozure Common Lisp Version 1.12  DarwinX8664)</a> 
at localhost:5000</address></p></body></html>.
 --------------------------------
```

This looks a lot better. The test still fails, which is good and expected. But the server works and responds with `404` for a request to `http://localhost:5000/blog`.

The test will fail until the server responds with the proper page title. In order to have the right page title we'll still have some work to do. So the right thing now is to move on to the smaller components, develop them in the similar style (tests-first, TDD) only that the unit tests for those components should all pass.

<a name="blog-feature_asdf-system"></a>*<a href="https://common-lisp.net/project/asdf/" class="link">ASDF</a> - a quick detour*

But before we do that, and since we still have in mind what we did to make this all work so far we should setup an ASDF system that we'll add to and expand as we go along.

For a quick recall, ASDF is the de-facto standard to define Common Lisp systems (or projects if you want). It allows to define library dependencies, source dependencies, tests, and a lot of other metadata.

So create a new buffer/file, save it as _cl-swbymabeweb.asd_ in the root folder of the project and add the following:

```lisp
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

This defines the necessary ASDF system and test system to fully load the project to the system so far. When the project is in a folder where asdf can find it (like _~/common-lisp_) then it can be loaded into the image by:

```lisp
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

Until here we have a ditrectory structure like this:

```
.
├── cl-swbymabeweb.asd
├── src
│   └── main.lisp
└── tests
    ├── all-tests.lisp
    └── it-routing.lisp
```

I need to mention that the ASDF systems we defined explicitely name the source files and dependencies. ASDF can also work in a different mode where it can determine source dependencies according to the `:use` directive in the defined packages that are spread in files (I tend to use one package per file). This mode then just requires the root source file definition and it can sort out the rest. Look in the ASDF documentation for _package-inferred-system_ if you are interessted.

### <a name="blog-feature_inner-test-loops-first"></a>The inner test loops

Now we will move on to the inner components. The first component that is hit by a request is the routing. We have to define which requests, request paths are handled by what and how. As mentioned earlier most frameworks come with a routing mechnism that allows defining routes. We will use <a href="https://github.com/joaotavora/snooze" class="link">Snooze</a> for this. The difference between Snooze and other URL router frameworks is more or less that routes are defined using plain Lisp functions in Snooze and HTTP conditions just Lisp conditions. The author says: _"Since you stay inside Lisp, if you know how to make a function, you know how to make a route. There are no regular expressions to write or extra route-defining syntax to learn."_. The other good thing is that the routing can be easily unit tested.

#### <a name="blog-feature_url-routing"></a>URL routing / introducing the MVC controller

Of course we will start with a test for the routing. There will also be a new architectural component in the play, the _MVC controller_. The URL routing is still a component heavily tied to system boundary as it has to deal with HTTP inputs and outputs and reponse codes. In order to apply separation of concerns and single responsibility (SRP) we make the routing responsible for collecting all relevant input from the HTTP request and pass it on to the _controller_. At this stage we have to establish a contract between the router and the _controller_. So we define the input, expected output of the _controller_ as we see fit from our level of perspective in the router. The output also includes the errors the _controller_ may raise. All this is primarily carved out during developing the routing tests.

So let's put together the first routing test. Create a new buffer/file, save it as _tests/routes-test.lisp_ and put the following in:

```lisp
(defpackage :cl-swbymabeweb.routes-test
  (:use :cl :fiveam :cl-mock :cl-swbymabeweb.routes)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.routes-test)

(def-suite routes-tests
    :description "Routes unit tests"
    :in cl-swbymabeweb.tests:test-suite)

(in-suite routes-tests)

(test blog-route-index
  "Tests the blog index route.")
```

This just defines an empty test. The new thing here is <a href="https://github.com/Ferada/cl-mock/" class="link">cl-mock</a>. It is a mocking framework.

Why do we need mocking here? Well, we want to use a collaborating component, the _controller_. But we'd want to defer the implementation of the _controller_ until it is due. That is not now. The mock allows us to define the interface to the _controller_ without having to implement it. This also allows us to stay focused on the routing and the _controller_ interface definition. We don't need to be distracted with any _controller_ implementation details.

In order to get the test package compiled we have to _quickload_ two things, that is `snooze` and `cl-mock`. We also have to create the 'package-under-test' package. This can for now simply look like so (save as _src/routes.lisp_):

```lisp
(defpackage cl-swbymabeweb.routes
  (:use :cl :snooze))
(in-package :cl-swbymabeweb.routes)
```

Once the test code compiles and we can actually run the empty test (use `in-package` and `run!` as above) we can move on to implementing more of the test.

One thing to remember is to update the ASDF system definition with the new files we added and library dependencies. However, in order to not interrupt the workflow I'd like to defer that until we can make a clear head again. The best time might be when we are done with the unit tests for the routes.

Now, let's add the following to the test function `blog-route-index`:

```lisp
  (with-mocks ()
    (answer (controller.blog:index) (cons :ok ""))

    (with-request ("/blog") (code)
      (is (= 200 code))
      (is (= 1 (length (invocations 'controller.blog:index))))))
```

`with-mocks` is a macro that comes with `cl-mock`. Any mocking code must be wrapped inside it. To actually mock a function call we use the `answer` macro which is also part of `cl-mock`. The use of `answer` in our test code basically means: _answer_ a call to the function `(controller.blog:index)` with the result `(cons :ok "")`. Since the _controller_ does not yet exist we did define the interface for it here and now. This is how we want the _controller_ to work. We did define that there should be a dedicated _controller_ for the _blog_ family of pages. We also defined that if there is no query parameter we want to use the `index` function of the _controller_ to deliver an appropriate result. The result should be a `cons` consisting of an `atom` (_car_) and a string (_cdr_). The _car_ indicates success or failure result (the exact failure atoms we don't know yet). The _cdr_ contains a string for either the generated HTML content or a failure description. `answer` doesn't call the function, it just records what has to happen when the function is called.    
Let's move on: the `with-request` macro is copied from the _snooze_ sources. It takes a request path and fills the `code` parameter with the result of the route handler. (This macro we have to copy to the test code, I'll show it shortly.) In the body of the `with-request` macro we can verify the `code` with an expected code. Also we want to verify that the request handler actually called the _controller_ index function by checking the number of `invocations` that `cl-mock` recorded.

Now to compile and run the test there are a few things missing. First of all the `with-request` macro. Copy the following to _routes-test.lisp_:

```lisp
(defmacro with-request ((uri
                         &rest morekeys
                         &key &allow-other-keys) args
                        &body body)
  (let ((result-sym (gensym)))
    `(let* ((snooze:*catch-errors* nil)
            (snooze:*catch-http-conditions* t)
            (,result-sym
              (multiple-value-list
               (snooze:handle-request
                ,uri
                ,@morekeys)))
            ,@(loop for arg in args
                    for i from 0
                    when arg
                      collect `(,arg (nth ,i ,result-sym))))
       ,@body)))
```

Also we need a stub of the _controller_. Create a new buffer/file, save it as _src/controllers/blog.lisp_ and add the following:

```lisp
(defpackage :cl-swbymabeweb.controller.blog
  (:use :cl)
  (:export #:index)
  (:nicknames :controller.blog))

(in-package :cl-swbymabeweb.controller.blog)

(defun index ())
```

The test and the overall code should now compile. When running the test we see that the HTTP result code is 404 instead of the expected 200. We also see that 

```
(LENGTH (INVOCATIONS 'CL-SWBYMABEWEB.CONTROLLER.BLOG:INDEX))
```

evaluated to `0`. Which means that the _controller_ index function was not called.  
This is good, because there is no route defined in _src/routes.lisp_. In contrast to the outer loop tests, which we shouldn't solve immediatele, we should of course solve this one. So let's add a route now to make the test 'green'. Add this to _routes.lisp_:

```lisp
(defroute blog (:get :text/html)
  (controller.blog:index))
```

This define a route with a root path of _/blog_. It defines that it must be a _GET_ request and that the output has a content-type of _text/html_.  
When we now evaluate the new route and run the test again we have both `is` assertions passing.

At this point we should add a failure case as well. What could be a failure for the _index_ route? The index is supposed to take the last available blog entry and deliver it. Having no blog entry is not an error I would say, rather the HTML content that the controller delivers should be empty, or should contain a simple string saying "there are no blog entries". So the only error that I'd imaging could be returned here is an internal error that was raised somewhere which bubbles up through the controller to the route handler.

Let's add an additional test:

```lisp
(test blog-route-index--err
  "Tests the blog index route. internal error"
  (with-mocks ()
    (answer (controller.blog:index) (cons :internal-error "Foo"))

    (with-request ("/blog") (code)
      (is (= 500 code))
      (is (= 1 (length (invocations 'controller.blog:index)))))))
```

Running the test has one assertion failing. The _code_ is actually 200, but we expect it to be 500. In order to fix it we have to add some error handling and differentiate between `:ok` and `:internal-error` results from the _controller_. Let's do this, change the route definition to this:

```lisp
(defroute blog (:get :text/html)
  (let ((result (controller.blog:index)))
    (case (car result)
      (:ok (cdr result))
      (:internal-error (http-condition 500 (cdr result))))))
```

This will make all existing tests green. But I'd like to add another test for a scenario where the controller result is undefined, or at least not what the router expects. I'd like to prepare for any unforseen that might happen on this route handler so that the response can be a defined proper error code. So add this test:

```lisp
(test blog-route-index--err-undefined-controller-result
  "Tests the blog index route.
internal error when controller result is undefined."
  (with-mocks ()
    (answer (controller.blog:index) nil)

    (with-request ("/blog") (code)
      (is (= 500 code))
      (is (= 1 (length (invocations 'controller.blog:index)))))))
```

When executing this test the response code is a 204, which represents 'no content'. And indeed, this is correct. When the _controller_ result is `nil` the router handler will also return `nil` because there is no `case` that handles this _controller_ result so the functions runs through without having defined an explicit return, which then makes the funtion return `nil`. So we have to change the router code a bit to handle this and more cases. Change the route definition to:

```lisp
(defroute blog (:get :text/html)
  (handler-case
      (let ((result (controller.blog:index)))
        (case (car result)
          (:ok (cdr result))
          (:internal-error (http-condition 500 (cdr result)))
          (t (error "Unknown controller result!"))))
    (error (c)
      (let ((error-text (format nil "~a" c)))
        (log:error "Route error: " error-text)
        (http-condition 500 error-text)))))
```

The outer `handler-case` catches any error that may happen and produces a proper code 500. Additionally it logs the error text. The `case` has been enhanced with a 'otherwise' handler which actually produces an error condition that is caught in the outer `handler-case`.  
When running the tests again we should be fine.

The tests of the router don't actually test the string content (_cdr_) of the _controller_ result because it's irrelevant to the router. It's important to only test the responsibilities of the unit-under-test. Any tests that go beyond the responsibilities, or the public interface of the unit-under-test leads to more rigidity and the potential is much higher that tests fail and must be fixed when changes are made to production code elsewhere.

We are now done with this feature slice in the router. There is more to some, but we will add another outer loop cycle first.  
It is now a good time to bring the ASDF system definition up to date. Add the new library dependencies: _snooze_, _cl-mock_. Also change the `:components` section to look like this:

```lisp
  :components ((:module "src"
                :components
                ((:module "controllers"
                  :components
                  ((:file "blog")))
                 (:file "routes")
                 (:file "main"))))
```

This adds the 'controllers' sub-folder as a sub component that can name additional source files under it. When done, restart the REPL, load both systems and also run `test-system`. At this point this should look like this:

```
 Did 7 checks.
    Pass: 6 (85%)
    Skip: 0 ( 0%)
    Fail: 1 (14%)
```

The only expected failing test is the integration test. Though the fail reason is still 404 'Not Found'. This is because we have not yet registered the route with the HTTP server. But I'd like to postpone this for when we have implemented the _controller_.

#### <a name="blog-feature_blog-controller-first"></a>The blog controller

Before we go to implement the tests for the _controller_ and the code for the _controller_ itself we have to think a bit about the position of this component in relation to the other components and what the responsibilies of the blog _controller_ are. In the MVC pattern it has the role of controlling the _view_. It also has the responsibility to generate the data, the _model_, that the _view_ needs to do its job. The _view_ is responsible to produce a representation in a desired output format. The _model_ usually consist of a) the data to present to the user and b) the attributes to control the _view_ components for visibility (enabled/disabled, etc.).  
In our case we want the _view_ to create a HTML page representation that contains the text and images of a blog entry, the blog navigation, and all the rest of the page. So the _model_ must contain everything that the _view_ needs in order to generate all this.  
Let's have a look at this diagram:

<figure>
<img src="/static/gfx/blogs/class-deps-diag.png" alt="Class dependencies" width="375" />
</figure>

The blog _controller_ should not have to deal with loading the blog entry data from file. This is the responsibility of the _blog repository_. Stripping out this functionality into a separate component has a few advantages. It keeps the _controller_ code small and clean maintaining _single responsibility_. This is reflected in the tests, they will be simple and clean as well. The _blog-repo_ will have to be mocked for the tests. The interface to the _blog-repo_ will also be defined when implementing the tests for the blog _controller_ on a use-case basis. The _blog-repo_ as being in a separate area of the application will have its own model that can carry the data of the blog entries. The _controller_ s job will be to map all relevant data from the _blog-repo_ model to the _view_ model. A _model_ separation is important here for orthogonality. Both parts, the _blog-repo_ and the _controller_ / _view_ combo should be able to move and develop separately and in their own pace. Of course the _controller_, as user of the _blog-repo_ has to adapt to changes of the _blog-repo_ interface and model. But this should only be necessary in one direction.  
The purpose of _blog-repo-factory_ is to be able to switch the kind of _blog-repo_ implementation for different environments. It allows us to make the _controller_ use a different _blog-repo_ for test and production environment. The _controller_ will only access the _blog-repo_ through the _blog-repo-facade_ which is a simplified interface to the _blog-repo_ that hides away all the inner workings of the blog repository. So the _controller_ will only use two things: the _blog-repo-facade_ and the blog repo model. This simplified interface to the blog repository will also be simple to mock in the _controller_ tests as we will see shortly.

The arrows in this diagram mark the direction of dependencies. The _router_ has an inward dependency on the _controller_. The _controller_ in turn has a dependency on the _view_ and _model_ because it must create both. The _controller_ also has a dependency on the _blog-repo-facade_ and on the model of the _blog-repo_. But none of this should have a dependency on the _controller_. Nor should the _controller_ know about anything happening in the _router_ or deal with HTTP codes directly. This is the responsibility of the _router_.

<a name="blog-feature_mvc-detour"></a>*MVC - a quick detour*

MVC pattern first actually wasn't a pattern. It was added to Smalltalk as a way to program UIs in the late 70's and only later MVC was adopted by other languages and frameworks. It allows a decoupling and a separation of concerns. Different teams can work on _view_, _controller_ and _model_ code. It also allows better testability and a higher reusability of the code. Use-cases grouped as MVC have a high cohesion and a low coupling.

It's interesting, the 70's to 90's were amazing times. Pretty much all technological advancements of programming languages and patterns of computer science come from this time frame. Structured programming, object-oriented programming, functional programming, statically typed languages (Standard ML) and type inference (Hindley-Milner) were invented then. It was a time of open-minded exploration and ideas.

-- _detour end_

Again, we start with test code, the plain blog _controller_ test package. Save this as _tests/blog-controller-test.lisp_:

```lisp
(defpackage :cl-swbymabeweb.blog-controller-test
  (:use :cl :fiveam :cl-mock)
  (:export #:run!
           #:all-tests
           #:nil))
(in-package :cl-swbymabeweb.blog-controller-test)

(def-suite blog-controller-tests
  :description "Tests for blog controller"
  :in cl-swbymabeweb.tests:test-suite)

(in-suite blog-controller-tests)
```

The first test is already a bummer. It is slightly more complex than anything we had so far. But it won't get a lot more complex. It's not trivial to write how this slowly develops applying TDD using the red-green-refactor phases. So I'm just pasting the complete test with all additional needed data. But of course this was developed in the classic TDD style.

```lisp
(defparameter *expected-page-title-blog*
  "Manfred Bergmann | Software Development | Blog")

(defparameter *blog-model* nil)

(test blog-controller-index-no-blog-entry
  "Test blog controller index when there is no blog entry available"

  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post nil))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok nil))
    (answer (blog-repo:repo-get-all) (cons :ok nil))
    (answer (view.blog:render *blog-model*) *expected-page-title-blog*)

    (is (string= (cdr (controller.blog:index)) *expected-page-title-blog*))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))
```

The first, simpler test assumes that no blog entry exists. Let's go through it:  
We have now two things that come into play. It's 1) the _blog-repo-facade_ here represented as `blog-repo` package and 2) the blog _view_ package `view.blog`. The blog _views_ s `render` function will produce HTML output. We will mock the _view_ generation and will `answer` with the pre-defined `*expected-page-title-blog*`. The blog _view_ will also need a _model_, represented as `*blog-model*` parameter.  
Again we need to setupo mocks using `with-mocks` macro. The `answer` calls represent the interfaces and function calls the _controller_ should do to the `blog-repo` in order to a) retrieve all blog entries (`blog-get-all`) which is internally triggered through the call `blog-get-latest`. So the way the _blog-repo_ works is that in order to retrieve the latest entry all entries must be available which must be collected before. Again we define an output interface of the _blog-repo_ to be a `cons` with an _atom_ as `car` and a result as the `cdr`. The two `blog-repo` facade calls both return with `:ok` but contain an empty result. This is not an error. The _view_ has to render appropriately, which is not tested here. Also again, the _controller_ tests only test the expected behavior of the _controller_ which is more or less: generate the _model_ for the view, pass it to the _view_ and take a response from the _view_.  
The `view.blog:render` function takes as parameter the blog _model_ and should return just the expected page title. The `*blog-model*` is a class structure which is here initialized kind of empty (`nil` represents empty).

The assertions make sure that a call to `controller.blog:index` actually returns the expected page title as `cdr` and also that all expected functions have been called.

In order for this to compile we have to add a few things. Create a new buffer/file and add the following code for the stub of the _blog-repo-facade_ and save it as _src/blog-repo.lisp_:

```lisp
(defpackage :cl-swbymabeweb.blog-repo
  (:use :cl)
  (:nicknames :blog-repo)
  (:export ;; facade for repo access
           #:repo-get-latest
           #:repo-get-all))

(in-package :cl-swbymabeweb.blog-repo)

(defun repo-get-latest ()
  "Retrieves the latest entry of the blog.")

(defun repo-get-all ()
  "Retrieves all available blog posts.")
```

Also we have to add the _view_ stub. Create a new buffer/file, save it as _src/views/blog.lisp_ and add the following:

```lisp
(defpackage :cl-swbymabeweb.view.blog
  (:use :cl)
  (:nicknames :view.blog)
  (:export #:render
           #:blog-view-model))

(in-package :cl-swbymabeweb.view.blog)

(defclass blog-view-model ()
  ((blog-post :initform nil
              :initarg :blog-post
              :reader model-get-blog-post)
   (all-blog-posts :initform '()
              :initarg :all-blog-posts
              :reader model-get-all-posts)))

(defun render (view-model))
```

This package also defines the _model_ class. The `blog-view-model` is the aggregated _model_ that is passed in from the _controller_. The `blog-post` and `all-blog-posts` slots model up the 'to-be-displayed' blog entry and all available blog entries which is relevant for the navigation view component. To have a separation from the _blog-repo_ model data (orthogonality) we will add separate model classes that are used only for the _view_. We will do this shortly.  
Considering the dependencies we have two options of where to define the _model_. It's either right here (this works if we have a simple model class), or we define it in a separate package. In case we have more model classes and more complex ones this would be the better approach.

There is one addition we have to add to the test package. Add the following to right below the `:export`:

```lisp
  (:import-from #:view.blog
                #:blog-view-model)
```

We explicitly import only the things we really need. This is the minimal code to get all compiled but the tests should still fail. So we have to implement some logic to the `index` function of the _controller_.

In order for the blog _controller_ code in _src/controllers/blog.lisp_ to know the _view_ functions we should import the `:view.blog` package and let's implement some code to make the tests pass.

<a name="blog-feature_tdd-detour"></a>*TDD - a quick detour*

We just have to look at the test expectations and implement them. An aspect of TDD I haven't talked about is the faking and cheating one can do in order to get the tests green (pass) as quickly as possible. When the tests pass we can refactor and replace the cheating with some 'real' code. Until now I have presented you full implemententations of the production code that fit to the test expectations. But a TDD cycle moves in fast paced iterations  with only small changes from red to green, then refactor, then from green to red for a new test case to restart the cycle. The cycle from red to green can contain cheating. This is because we want feedback as quickly as possible about what we did is either good or no good. When we cheat, this 'good' means just that we meet the current expectation. Iteration for iteration we add new expectations that at some point can't be cheated anymore. This workflow that switches between test code and production code very rapidly and the immediate feedback we get puts _us_ kind of into a symbiosis between the test code and the production code. The realization and the feeling of the code building up this way is enormously satisfying. The fact that you just can concentrate on small fractions of code but have a certain security that you prepared to not forget something because the outer, larger scale test is setting a 'protected' frame and savety net you to work in which allows you can have trust (when done right hopefully).

-- _detour end_

<a name="blog-feature_tdd_cheat"></a>*The cheating*

So now I'll introduce a bit of cheating that just makes the one existing test case and all assertions pass. As I said this actually builds up in much smaller steps. And eventually of course we need to get rid of the cheating.

So, replace the `index` function with the following implementation and also add the other small functions.

```lisp
(defun index ()
  (let ((lookup-result (blog-repo:repo-get-latest))
        (all-posts-result (blog-repo:repo-get-all)))
    (make-controller-result
     :ok
     (view.blog:render
      (make-view-model (cdr lookup-result) (cdr all-posts-result))))))

(defun make-controller-result (first second)
  "Convenience function to create a new controller result.
But also used for visibility of where the result is created."
  (cons first second))

(defun make-view-model (the-blog-entry all-posts)
  (make-instance 'blog-view-model
                 :blog-post nil
                 :all-blog-posts nil))
```

This is partly cheated insofar as the view model is generated with hardcoded `nil` values just as the tests expect it. When we compile this we're getting warnings shown for unused variables `the-blog-post` and `all-posts`. Those warnings should be treated seriously. We'll fix them shortly.  
To better _reveil the intention_ of how the _controller_ works, and the output it generates we add a function `make-controller-result` that can generate the result (which after all is just a `cons`).  
When we run the tests they will all pass:

```
Running test BLOG-CONTROLLER-INDEX-NO-BLOG-ENTRY ....
 Did 4 checks.
    Pass: 4 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

When we now add another test case we will have no other choice as to remove the cheating in order to make the tests pass. We will see now. For the new test case we will need quite a bit additional production code, even if it's just stubs (more or less) but we need to get things compiled in order to even only run the new test. Add the following test case:

```lisp
(defparameter *blog-entry* nil)
;; 12 o'clock on the 20th September 2020
(defparameter *the-blog-entry-date* (encode-universal-time 0 0 12 20 09 2020))

(test blog-controller-index
  "Test blog controller for index which shows the latest blog entry"

  (setf *blog-entry*
        (blog-repo:make-blog-entry "Foobar"
                                   *the-blog-entry-date*
                                   "<b>hello world</b>"))
  (setf *blog-model*
        (make-instance 'blog-view-model
                       :blog-post
                       (blog-entry-to-blog-post *blog-entry*)
                       :all-blog-posts
                       (mapcar #'blog-entry-to-blog-post (list *blog-entry*))))
  (with-mocks ()
    (answer (blog-repo:repo-get-latest) (cons :ok *blog-entry*))
    (answer (blog-repo:repo-get-all) (cons :ok (list *blog-entry*)))
    (answer (view.blog:render model-arg)
      (progn
        (assert
         (string= "20 September 2020"
                  (slot-value (slot-value model-arg 'view.blog::blog-post)
                              'view.blog::date)))
        (assert
         (string= "20-09-2020"
                  (slot-value (slot-value model-arg 'view.blog::blog-post)
                              'view.blog::nav-date)))
        *expected-page-title-blog*))

    (is (string= (cdr (controller.blog:index)) *expected-page-title-blog*))
    (is (= 1 (length (invocations 'view.blog:render))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-all))))
    (is (= 1 (length (invocations 'blog-repo:repo-get-latest))))))
```

The parameter `*blog-entry*` is set up with the _model_ from the _blog-repo_, which we have to define still. Otherwise it is similar to the previous test case. The difference is that we expect the _blog-repo_ now to actually get us blog entries which are mapped to the _view_ model and passed on to the _view_ to generate the display. We also use a new functionality of the `answer` macro. It can do pattern matching on the provided function parameter and so we can validate the _date_ and _nav-date_ formatting (we will add the model for this shortly). We also pre-define a timestamp with the `*the-blog-entry-date*` parameter which we require to be stable for the test case.  
Now let's add the missing code to get this compiled. Stay close as we have to modify a few files.

To _src/blog-repo.lisp_ add the following class which represents the blog _model_:

```lisp
(defclass blog-entry ()
  ((name :initform ""
         :type string
         :initarg :name
         :reader blog-entry-name
         :documentation "the blog name, the filename minus the date.")
   (date :initform nil
         :type fixnum
         :initarg :date
         :reader blog-entry-date
         :documentation "universal timestamp")
   (text :initform ""
         :type string
         :initarg :text
         :reader blog-entry-text
         :documentation "The HTML representation of the blog text.")))
         
(defun make-blog-entry (name date text)
  (make-instance 'blog-entry :name name :date date :text text))         
```

The `make-blog-entry` is a convenience function to more easily create a `blog-entry` instance. This class structure has three slots. The `name` represents the name of the blog entry. The `date` is the date (timestamp, type `fixnum`) of the last update of the blog entry. And `text` is the HTML representation of the blog entry text. We don't go into detail about the blog `text`. The _blog-repo_ takes care of this detail. What is important is that it delivers the text in a representational format that is immediately usable. There may be different strategies at play in the _blog-repo_ that are able to convert from different sources to HTML. As initially pointed out the goal should be to allow plain HTML and Markdown texts. So at this point _blog-repo_ is a black box for us. We use the data as is.

Then we'll have to add some additional `export`s in this package so that the class itself and the `reader` accessors can be used from importing packages.

```
(:export #:make-blog-entry
         #:blog-entry-name
         #:blog-entry-date
         #:blog-entry-text
         ;; facade for repo access
         #:repo-get-latest
         #:repo-get-all)
```

In _src/controllers/blog.lisp_ we need the following additions:

```lisp
(defun blog-entry-to-blog-post (blog-entry)
  "Converts `blog-entry' to `blog-post'.
This function makes a mapping from the repository 
blog entry to the view model blog entry."
  (log:debug "Converting post: " blog-entry)
  (when blog-entry
    (make-instance 'blog-post-model
                   :name (blog-entry-name blog-entry)
                   :date (format-timestring nil
                                            (universal-to-timestamp
                                             (blog-entry-date blog-entry))
                                            :format
                                            '((:day 2) #\Space
                                              :long-month #\Space
                                              (:year 4)))
                   :nav-date (format-timestring nil
                                                (universal-to-timestamp
                                                 (blog-entry-date blog-entry))
                                                :format
                                                '((:day 2) #\-
                                                  (:month 2) #\-
                                                  (:year 4)))
                   :text (blog-entry-text blog-entry))))
```

I'll explain in a bit what this does. Suffice to say for now that this is the function that maps the data from a `blog-entry` data structure to the `blog-post-model` data structure (which we'll define next) as used in the `blog-view-model`.  
This function uses date-time formatting, so we need an import for the functions `format-timestring` and `universal-to-timestamp`. Those are date-time conversion functions that allows the Common Lisp `get-universal-time` timestamp to be converted to a string using a defined format. Import and quickload the package `local-time` for that. Additionally we need the _controller_ to import `:blog-repo` so that is has access to the `blog-entry` and the _readers_ accessors.

We also need to define another view model class that represents the blog entry to be displayed. Add the following to _src/views/blog.lisp_:

```
(defclass blog-post-model ()
  ((name :initform ""
         :type string
         :initarg :name)
   (date :initform ""
         :type string
         :initarg :date)
   (nav-date :initform ""
             :type string
             :initarg :nav-date)
   (text :initform ""
         :type string
         :initarg :text)))
```

This class is relatively close to the _blog-repo_ class `blog-entry`. The _controller_ function `blog-entry-to-blog-post` makes the mapping from one to the other. The _view_ has a different responsibility than the _blog-repo_ has. For example has the `blog-post-model` an additional slot, the `nav-date`. It is used in the 'recents' navigation and must present the blog post create/update date in a different string format than is shown in the full blog post display. Generally we use a `string` type for the `date` and `nav-date` slots here because the instance that controls how something is displayed is the _controller_.  
So `blog-entry-to-blog-post` makes a full mapping from a `blog-entry` to a `blog-post-model` with all that is actually needed for the _view_. With this we make the _view_ a relatively dump component that just shows what the _controller_ wants. The _controller_ test also defines the date formats to be used. Those formats and date strings as displayed by the _view_ are validated in the `answer` call. Let's have a look at this in more detail:

```
(answer (view.blog:render model-arg)
  (progn
    (assert
     (string= "20 September 2020"
              (slot-value (slot-value model-arg 'view.blog::blog-post)
                          'view.blog::date)))
    (assert
     (string= "20-09-2020"
              (slot-value (slot-value model-arg 'view.blog::blog-post)
                          'view.blog::nav-date)))
    *expected-page-title-blog*))
```

The `answer` macro captures the function call arguments, so we can give the argument a name and check on its values. In our case we want to assert that the date strings are of the correct format, which are two different formats. The `nav-date` for example has to be a bit more condensed than the standard `date` format. After all `answer` has to still return something so we use `progn` which returns the last expression. Since we did not export the slots of `blog-view-model` and `blog-post-model` we use the double colon `::` to access them. We didn't export those symbols because no one except the _view_ itself needs to access them. This is a bit of a grey area because we tap into a private area of the _model_ data structures. On the other hand it would be good to control and verify the format of the date string. So we choose to accept to live with possible test failures when the structure of the _model_ changes.

With the last addition the code compiles now. So we can run the new test. The test of course fails with:

```
 BLOG-CONTROLLER-INDEX in BLOG-CONTROLLER-TESTS 
 [Test blog controller for index which shows the latest blog entry]: 
      Unexpected Error: #<SIMPLE-ERROR #x3020039689ED>
NIL has no slot named CL-SWBYMABEWEB.VIEW.BLOG::DATE..
```

This is logical. Because we still have our cheating in place that creates a _view_ model with hard coded `nil` values. So the mocks don't have any effect due to this.

To make the tests pass we have to add the proper implementation of the `make-view-model` function in the _controller_ code (see above). Replace the function with this:

```
(defun make-view-model (the-blog-entry all-posts)
  (make-instance 'blog-view-model
                 :blog-post
                 (blog-entry-to-blog-post the-blog-entry)
                 :all-blog-posts
                 (mapcar #'blog-entry-to-blog-post all-posts)))
```

This will now pass the _blog-repo_ blog entry through the mapping function for the single `blog-post` slot as well as for all available blog posts generated by `mapcar` for `all-blog-posts`. Compiling this will now also remove the warnings we have had with this previously as the two function arguments are now used. Running the tests again now will give us a nice:

```
Running test BLOG-CONTROLLER-INDEX ....
 Did 4 checks.
    Pass: 4 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

<a name="blog-feature_reflection"></a>*Taking a step back and reflect*

The MVC blog _controller_ is a relatively complex and central piece in this application. Let's take a step back for a moment and recapture what we have done and how we should continue.

The _controller_ is using two collaborators to do its work. Those two are the _blog-repo_ and the _view_. Both are not part of the _controller_ unit and hence must be tested reparately. The _controller_ as the driver of the functionality wants to control how to talk to the two collaborators. So the _controller_ tests define the interface which is then implemented in the _controller_ code and in both the _blog-repo_ and the _view_. For now the collaborators are only implemented with stubs so that the mocking can be applied. Since the _controller_ is the only 'user' of the two collaborating components it can more or less freely define the interface as it requires it. Would there be more 'users' there would be a bit more of 'pulling' from each 'user' so that eventually the interface would be more of a compromise or just had more functionality. We have also decided that the _controller_ controls what and how the _view_ displays the data. This was done for the date formats that are being displayed in the view.

Before we move on to working on the _view_ we should recheck the outer loop test to verify that it still fails. Also now is a good time to register the routing on the HTTP server.

<a name="blog-feature_outer-loop-revisit"></a>*Revisit the outer test loop*

The registration of the routes on the HTTP server is a missing piece in the full integration. The error the test provokes should get more accurate the closer we get to the end. So we're closing that gap now. To recall, the integration test raises a 404 'Not Found' error on the _/blog_ route. That can be 'fixed' because we have implemented the route.

Extend the _erc/routes.lisp_ with the following function:

```
(defun make-routes ()
  (make-hunchentoot-app))
```

This function must also be exported: `(:export #:make-routes)`.  
Then in _src/main.lisp_ import this function:

```
(:import-from #:cl-swbymabeweb.routes
              #:make-routes)
```
and change the `start` function to this (partly):

```
  (unless *server*
    (push (make-routes)
          hunchentoot:*dispatch-table*)
    (setf *server*
          (make-instance 'hunchentoot:easy-acceptor
                         :port port
                         :address address))    
    (hunchentoot:start *server*)))
```

The `make-routes` function will create route definitions that can be applied on Hunchentoot HTTP server `*dispatch-table*`. This is a feature of _snooze_. It can do this for other servers as well.

Running the integration test now will give a different result.

```
<ERROR> [13:20:51] cl-swbymabeweb.routes routes.lisp (blog get text/html) - 
Route error: CL-SWBYMABEWEB.ROUTES::ERROR-TEXT: 
"The value \"Retrieves the latest entry of the blog.\" 
is not of the expected type LIST."
```

This is a bit odd. What's going on?  
When looking at it more closely it makes sense. This is in fact great. It shows us that many parts are still missing for a full integration of all components. The text 'Retrieves the latest entry of the blog.' is returned by the function `repo-get-latest` of the _blog-repo_ facade. Since it defines no explicit return it will implicitly return the only element in the function, which here is the docstring. But later, in the controller, the return of the `repo-get-latest` is expected to be a `cons` (the error says LIST, but a list is a list of `cons` elements) but apparently it is no `cons`. So when trying to access elements of the `cons` with `car` or `cdr` we will see this error.  
This tells us that there is still quite a bit of work left. We will later fix the integration test without fully implementing the _blog-repo_. This blog post has already grown too large, so we'll shortcut this part later.

<a name="blog-feature_ctrl-update-asd"></a>*Updating ASDF*

Also a good time to bring the ASDF system up-to-date. Add all the new files and library dependencies. Add the _src/views/blog.lisp_ component similarly as we added the _controller_ component. Also add _src/blog-repo.lisp_. It should be defined before the _view_ and the _controller_ definitions due to the direction of dependency. Also add _local-time_ library dependency.

To check if the system definitions work you can always do those four steps:

1. restart the inferior lisp process.
2. load the default system (`asdf:load-system`) and fix missing components if it doesn't compile.
3. load the test system. Fix missing components if necessary.
4. test the test system (`asdf:test-system`).


#### <a name="blog-feature_blog-view"></a>The blog view

We are free to generate the view representation as we like. We can use any library we like to. We could even mix those. What the _controller_ expects the _view_ to deliver is HTML as a string. This is the only contract we have. What are our options to generate HTML? We could use:

1. a templating library. Templating libraries are based on template files which represent pages or smaller page components. A page template is usually composed of smaller component templates. Templates also allow inheritance. Templating libraries usually provide 'language' constructs that allow 'structured programming' (to some extend) in the template, like _for_ loops, _if/else_, etc. The template library then evaluates the template and expands the language constructs to HTML code:
	- <a href="https://mmontone.github.io/djula/" class="link">Djulia</a>: This one is close to Pythons Django and provides a custom template language.
	- <a href="https://gitlab.common-lisp.net/mraskin/cl-emb" class="link">cl-emb</a>: This is a templating library that allows using Common Lisp code in the template. It's use-case is not limited to HTML.

2. a DSL that allows to write Lisp code which looks close to HTML constructs. There are a few libraries in Common Lisp that do this. Generally DSLs are relatively easy to create in Common Lisp using macros:
	- <a href="https://github.com/edicl/cl-who" class="link">cl-who</a>: This library has a long history. It is very mature. However, it is limited to HTML 4.
	- <a href="https://github.com/ruricolist/spinneret" class="link">spinneret</a>: This is a younger library that concentrates on HTML 5.

There are a few more options for both variants. If you are curious you can have a look at <a href="https://github.com/CodyReichert/awesome-cl#html-generators-and-templates" class="link">awesome-cl</a>.  
We are choosing cl-who for this project mainly because I like the expressivness of Lisp code. When I can write HTML this way, the better. In addition, since it is Lisp code I get an almost immediate feedback about the validity of the code in the editor when compiling a code snippet. For larger projects however the templating variant may be a better choice because of the separation between the HTML and the backing code it provides, even though the template language weakens this separation. But yet it might be easier to non-coders to work with the HTML template files.

<a name="blog-feature_view-test"></a>*Testing the view*

If we recall, we had created a package for the _view_ where we did define the view _model_ classes that the _controller_ instantiates and fills with data. But we had not created tests for this because the `render` function of the _view_ was mocked in the _controller_ test.

