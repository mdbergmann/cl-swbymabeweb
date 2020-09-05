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

The web application we will be developing during this blog post is based on an old web page design of mine that I'd like to revive. The web application will include some static pages that are based on Markdown and are loaded from the file system. Also the web application will include a blog feature that allows to write blog posts as either HTML or Markdown files that are converted on the fly to HTML before presenting them.

The application started out with Caveman2, because it seemed to be the most complete framework. But due to the lack of documentation for the framework itself and also for Clack some refactorings were conducted quite early in development where pretty much all of Caveman2 was removed.

So the web application is based on the following libraries (web application relevant only):

- plain Hunchentoot (replaced Clack)
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
	- setup an acceptance test for the full feature loop
	- create unit tests for all components we touch
	- add routing for when a request touches the web server
	- delegate to a MVC controller
	- create the view with loading content from file
	- pass the generated HTML back to the web server for delivery to the client 
3. add the blog page and feature that loads blog posts from file system
	- setup acceptance tests for the full feature loop
	- create or extend unit tests for all components we touch
	- add or extend routing
	- create dedicated MVC controller
	- create blog post repository facility
	- create views that generate a result HTML that is delivered back to the caller

During those points we will cover many things like how to properly test using mocks, i18n, how to use cl-who, how to convert Markdown to HTML and other things.

What I write about in this blog, and in particular the workflow, is a smoothed version. I don't write about all the bumps and issues I had to deal with (or only if revelant), otherwise this would get the size of a small book (maybe I do that one day).

### Project setup

Since this was my first web project with Common Lisp I had to do some research for how to integrate and run the server and add routes, etc. This is where the scaffolding that frameworks like Caveman2 produce are appeciated.

But, once we know how that works we can start a project from scratch. Along the way we can create a template for future projects.
That means we don't have a lot of setup to start with. We create a project folder and a 'src' and 'tests' folder therein. That's it. We'll add an asdf based project/system definition as we go along.

To get started and since we use a test-driven approach we'll start with adding an acceptance test for the first feature, the imprint page.

In order to add tests that are part of a full test suite we'll start creating an overall 'all-tests' test suite. So we'll create a new Lisp file and add the following code and save it as 'tests/all-tests.lisp':

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

### Implementing a page

Let's take the 'imprint' page for this. There is not much on this page. Just some text to be displayed. So let's create a new Lisp buffer and add the following code:

```

```
