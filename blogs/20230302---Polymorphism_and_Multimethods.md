### Polymorphism

What is Polymorphism and what is it useful for?

In OOP (Object-Oriented Programming) polymorphism is well-known. It allows to separate an interface from multiple implementation that can have different behaviour.

Polymorphism comes from the greek `polús` (many) and `morphe` (form). Multiple forms, makes sense.

On which concrete object a method of the interface is called is determined at runtime. This is called _dynamic dispatch_.

Let's make a simple example in Scala:

```scala
trait IPerson {
  def sayHello()
}

class Teacher extends IPerson {
  override def sayHello() {
    println("Hello, I'm a teacher.")
  }
}

class Pupil extends IPerson {
  override def sayHello() {
    println("Hello, I'm a pupil.")
  }
}

class Student extends IPerson {
  override def sayHello() {
    println("Hello, I'm a student.")
  }
}
```

This implements three different persons which say 'hello' in a different way. The beauty with this is that when you have an object of type `IPerson` you don't need to know which concrete implementation it is. It usually is sufficient to know that it supports saying hello by calling `sayHello`. This abstraction is great because it allows a decoupling of the interface and the concrete implementations which may even be defined in different areas or modules of the application sources.

OO languages like Scala, Java, C#, etc. combine data and behaviour in classes. An additional step in separation and decoupling one could separate data and behaviour also in OO languages, though that is often not the norm, and once the language allows to add data (state) into classes it needs a lot of discipline to refrain from it.

Other languages separate data from behaviour naturally, which enables more decoupled design because data and behaviour can develop orthogonally. Many of those languages implement polymorphism with a concept called _multimethods_.

### Multimethods

I choose Common Lisp as representative to show multimethods (because I like Lisps and this one in particular :), but also Groovy, JavaScript, Python or other languages support multimethods either natively or via libraries.

#### Single dispatch

In Common Lisp multimethods are implemented as _generic functions_. Common Lisp in general has a very powerful object system.

As a first step we create the classes used later in the dispatch:

```lisp
(defclass person () ())  ;; base
(defclass teacher (person) ())
(defclass pupil (person) ())
(defclass student (person) ())
```

Now, similarly as the `trait` in Scala we first create a generic function definition:

```lisp
(defgeneric say-hello (person))
```

Now we can add the concrete methods:

```lisp
(defmethod say-hello ((person teacher))
  (format t "Hello, I'm a teacher."))

(defmethod say-hello ((person pupil))
  (format t "Hello, I'm a pupil."))

(defmethod say-hello ((person student))
  (format t "Hello, I'm a student."))
```

At this point we have a complete multimethod setup.  
We can now call the methods and see if it works:

```plain
CL-USER> (say-hello (make-instance 'teacher))
Hello, I'm a teacher.

CL-USER> (say-hello (make-instance 'student))
Hello, I'm a student.
```

The runtime system will search for methods it can dispatch on based on a generic function definion. The method implementations can be in different source files or packages/namespaces which makes this extremely flexible. This lookup does come with a performance penalty, but implementations can often apply some kind of caching to mitigate this.

#### Multi dispatch

The above is a 'single dispatch' because the dispatching is based on a single parameter, the person class.

Multi dispatch can dispatch on multiple parameters. Let's extend the example a bit to show this:

```lisp
(defgeneric say-hello (person time-of-day))

(defmethod say-hello ((person teacher) (time-of-day (eql :morning)))
  (format t "Good morning, I'm a teacher."))

(defmethod say-hello ((person teacher) (time-of-day (eql :evening)))
  (format t "Good evening, I'm a teacher."))

(defmethod say-hello ((person pupil) (time-of-day (eql :noon)))
  (format t "Good appetite, I'm a pupil."))

(defmethod say-hello ((person student) (time-of-day (eql :evening)))
  (format t "Good evening, I'm a student."))
```

Now we have a second parameter `time-of-day` which doesn't represent a time, but whether it's morning, noon or evening. Since `time-of-day` is not a class we have to use the `eql` specializer for the dispatching, but it could also be another class.

```plain
CL-USER> (say-hello (make-instance 'teacher) :evening)
Good evening, I'm a teacher.

CL-USER> (say-hello (make-instance 'teacher) :morning)
Good morning, I'm a teacher.

CL-USER> (say-hello (make-instance 'pupil) :noon)
Good appetite, I'm a pupil.
```

So looks like that the dispatching works, by taking both parameters into consideration. Of course this works also with more than two parameters.

The _generic functions_ in Common Lisp have a lot more features than those simple examples. For instance, with method specializers `:before`, `:after` or `:around` it is possible to implement aspect oriented programming. However, this is not the topic of this post.

### Conclusion

Multimethods and separating data from behaviour allows more decoupling and a more data-driven programming paradigm. When the data is immutable we are closer in the realm of functional programming. Functional programming and data-driven programming have pros and cons which should be named and weighted when starting a new project.
