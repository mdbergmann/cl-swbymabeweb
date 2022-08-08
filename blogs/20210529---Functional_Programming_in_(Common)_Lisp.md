### Intro

Functional programming (FP) is again getting popular in recent years. Why again? Because the paradigm of FP is very old. Early languages like Lisp, APL from the 1950/60s were already functional. Later, imperative languages like C, Pascal, etc. replaced functional languages to some degree. Now, since some years functional languages become again more popular.

This article will describe some key concepts of functional programming. While there exist many pure functional programming languages (like ML, Erlang, Haskell, Clojure) which lock you in into a pure functional style of programming, this article will describe some techniques to allow functional programming with multi-paradigm languages. The language of choice for this article is Common Lisp, a well established industrie standard that despite of its age provides a broad range of modern language features. The techniques apply to any language that provides lambdas and functions as objects (like Java, Python, C#, Scala, etc.), which is a key feature for functional programming.


### What is the difference between the two styles?

#### Functional programming

As the name suggests, FP is all about functions. About composing functions and about applying those functions on data to transform the data. Functions can be anonymous functions created at runtime or named functions. FP is declarative. It is more important to say _what_ should be done than _how_. For instance, FP languages have higher-order functions that operate on lists. You tell this function _what_ transformation of each list element you want to have, by supplying a function that is called with each list element and does the transformation, rather than manually writing a loop (`while`, `for`, etc.) construct that iterates over a list and implicitly does the transformation as part of the loop construct.  
Pure functions are a key element of FP. Pure functions are _referencial transparent_ which means that they always produce the same output for the same input and hence this function could be replaced with just the value of the function output (<a href="https://en.wikipedia.org/wiki/Referential_transparency" target="_blank" class="link">wiki</a>). This means that functions that have side-effects are not referencial transparent. Referencial transparency then also implies that parameters to functions (the input set) are not changed. A pure function produces new data, but doesn't change the old data that was used to produce the new data from. Functions that don't alter the input set are _not descructive_.   Additionally this implies that there exists some immutability of data structures, whether the data structures are immutable by design or functions just don't mutate the input data structures (the later requires a lot of discipline from the programmer). Those characteristics are often also mentioned together with data-driven development. Pure functions, as you can imagine, are thread-safe and can hence be easily used in multi-threaded environments.  
Aside of the practical benefits of the above, like: it's a lot easier to reason on pure functions and they tend to be easier to maintain (especially in multi-core and multi-threaded environments) and a (subjective) nicer way to code. Though it's of course still possible to write crap code that no one understands and is hard to maintain. The skill of the programmer to write readable and maintainable code is still imperative.  
There are also disadvantages: FP is more memory consuming and requires more CPU cycles. Because practically, due to immutability of objects and functions producing new data structures rather than modifying existing ones more data is produced which means more memory allocations are necessary and more memory must be cleaned up which costs garbage collector time. However, with current multi-core hardware those disadvantages are neglectable. Developer time and a maintainable code base that is easy to reason on is much more valuable for a business.  
For people interested in mathmatics, FP involves quite a bit of it with <a href="https://en.wikipedia.org/wiki/Category_theory" target="_blank" class="link">category theory</a>, morphisms, functors, etc. But this is not something that is part of this article.

##### Design considerations

From a software design perspective, FP programms usually have a purity core where only data transformations are taking place in form of pure functions (a 'functional core'). But even FP programms have to deal with side-effects and of course there is state. Languages deal differently with this. Haskell for example knows 'monads' to deal with side-effects and state. Erlang/Elixir have efficient user-space processes where state is locked in. Side-effects, state, as well as threading should happen on the system boundaries. The <a href="https://en.wikipedia.org/wiki/Hexagonal_architecture_%28software%29" target="_blank" class="link">hexagonal architecture</a> is an architectural style that can nicely fit to FP. It can be used with an 'imperative shell' and a 'functional core'.

##### Assignments

Due to the immutability pure FP languages allow assignments to variables only ones. Erlang/Elixir for example has a `=` operator, but it's not an assignment operator. It is a match operator, similar as in mathmatics where you say `x = y` which means that `x` is equal to `y`. Let's have a quick look at Erlang:

```erlang
4> X = 1.
1
5> Y = 2.
2
6> X = Y.
** exception error: no match of right hand side value 2
```

The `X` and `Y` variables here take the corresponding values of `1` and `2` because at this point they are unset. Since they are unset the `=` matches and assigns `X` with `1`.  
But when both variables are set and `=` is used the match fails because 1 is _not_ equal to 2.

Elixir is a little less strict on the last part:

```elixir
iex(1)> x = 1
1
iex(2)> y = 2
2
iex(3)> x = y
2
```

See, the last part works in Elixir. But there is still an important difference to a normal assignment. The new `x` has a different memory location than the previous `x` which means the value in the old memory locations is not altered.

##### Immutability

Immutability is also a key characteristic in FP languages. FP languages provide immutable datatypes like tuples, lists, arrays, maps, trees, etc.  
The provided functions that operate on the datatypes are pure functions. For example removing or adding items to a list create a new list.  
This behavior is built into FP languages (ML, Haskell, Erlang/Elixir, just to name a few). So using those languages your are operating in an immutability bubble. To maintain immutability inside the immutable bubble a 'shallow' copy of the data is usually sufficient. This is much more efficient for the runtime system and means that functions operating on datatypes create a copy of the datatype but share the instances that the datatype holds. For instance, adding a new head to a list (`cons`) will create a new list that is constructed from the given head element and the given input list as tail.

```lisp
CL-USER> (defvar *list* '(1 2 3 4))
*LIST*
CL-USER> (defun prepend-head (head list)
           (cons head list))
PREPEND-HEAD
CL-USER> (prepend-head 0 *list*)
(0 1 2 3 4)
CL-USER> *list*
(1 2 3 4)
```

What is not built-it are deep copies that are necessary when one leaves the immutable bubble, i.e. to do I/O.

##### Design Patterns (GoF)

Just a few words on this. Some of the design patterns of object-oriented programming, as captured in the Gang of Four book apply also for FP, but their implementation is in many cases much simpler. I.e. a strategy pattern in FP is just a higher-order function. A visitor pattern in a simple form could just be a reduce function.

#### Imperative programming

Imperative programming (IP) on the other hand is more about mutating the state of a machine with every instruction/statement. I would tend to say that also Object-Oriented Programming (OOP) is imperative. The difference being that OOP allows to give more structure to the programm. From a memory and CPU perspective the paradigm of IP is more efficient. And I've read somewhere (can't remember where, but it makes sense to me) that IP did replace FP in the mid to late of last century because memory was expensive and CPUs were not fast, and IP clearly had an advantage there when the value of a memory location is just changed instead of a new memory location allocated and the old one has to be cleaned up.  
But in todays multi-core and multi-threaded computing world state is a problem. It's not possible without state but how it is dealt with is important and different in FP.

#### Functional style in multi-paradigm language, Common Lisp

Common Lisp is a Lisp (obviously). Lisps have always had functions as first-class elements of the language. Functions can be anonymous (lambdas), or can have a name, they can be created at runtime and they can be passed around in the same way as strings or other objects can. Every function returns something, even if is it's just `nil` (for an empty function).  
Common Lisp allows to program in muliple paradigms. Historically it had to capture, consolidate and modernize all the Lisp implementations flowing around at the time. So it has a rather large but well balanced feature spec and it allows both OOP (with <a href="https://en.wikipedia.org/wiki/Common_Lisp_Object_System" target="_blank" class="link">CLOS</a>) and FP. Common Lisp doesn't have the very strict functional characteristics, like the mentioned assignments above. Functions in Common Lisp can have side effects and Common Lisp has assignments in form of `set`, `setq` and `setf`. For FP this means that the programmer has to be more disciplined in how he programms and which elements of the language he uses. But it's of course possible. This applies in the similar way to Java, Scala, C#, etc.  
When we look at the important characteristics for FP:

- first-class functions
- pure functions
- non-destructive functions
- immutable data structures

Then we have to be a bit careful what to use of all the things available in Common Lisp. All built-in data structures in Lisp are mutable. Lists are a bit of an exception because they are usually used in a non-destructive way. Like the function `cons` (construct) creates a new list by prepending a new element to the head of to a list, in which case the old list is not modified or destroyed but a new list is created. But `delete-if` (in contrast to `remove-if`) is destructive because it modifies the input list. The array/vector and hashmap data structures and their functions are destructive and shouldn't be used when doing FP. So when developing pure and non-destructive functions (i.e. for a functional core) one has to make sure to use the right higher-order functions for operating on lists or other data structures. Depending on which data structure it is it could also mean to manually make deeps copies of the input parameters, operate on the copy and return the copy. `modf` can help with this, see later.

##### Immutable data structures - FSet

The immutable data structures library <a href="https://common-lisp.net/project/fset/Site/index.html" target="_blank" class="link">FSet</a> should be a good fit when doing FP in Common Lisp. FSet defines a large set of functions for all sorts of operations.

There are more alternatives offering this functionality. See for example <a href="https://github.com/ndantam/sycamore" target="_blank" class="link">Sycamore</a>.

Other languages, like Java and Scala offer a range of immutable data structures out of the box.

##### Custom immutable types

Immutable maps are commonly used in FP instead of classes or structure types. They have one disadvantage. They don't create a specific type. In some FP languages like Erlang/Elixir it's possible to dispatch a function based on a destructuring of the function arguments, like a map or list. But Elixir also allows to define a type for a map structure which then can be used to dispatch on a function level.  
In Common Lisp it would be cool to use _generic functions_ also for FP because it allows dynamic/multi-dispatch and is generally a nice feature. But it can't destructure lists or maps on function arguments. It can only check on a type or equality of objects using `eql`. Both won't work well when using FSet with just maps, or sets.  
So in addition to the data structures available in FSet the standard structure type in Common Lisp (`defstruct`) could still be useable. It defines a new type, so we can use it with generic functions, we can check equality on the slots/instance vars with `equalp`, and we can set the slots/instance vars to `:read-only` which prevents from changing the slot/variable values. `defstruct` automatically generates a 'copier' function that copies the structure. This copy is just a flat copy and it doesn't allow to change values while copying. Let's have a quick look at some of the structure things:

```lisp
CL-USER> (defstruct foo (bar "" :read-only t))
FOO
CL-USER> (defstruct bar (foo "" :read-only t))
BAR
```

The option `:read-only` has the effect that `defstruct` doesn't generate 'setter' functions to change the slot. (though it is still possible to change the slot values using lower-level API, i.e. `slot-value` function. But the public interface does indeed disallow it.)

The next snippet shows how the dynamic dispatch works with the created new structure types.

```lisp
CL-USER> (defgeneric m-dispatch (arg))
#<STANDARD-GENERIC-FUNCTION M-DISPATCH #x3020034B2EEF>

CL-USER> (defmethod m-dispatch ((arg foo))
           (format t "me: foo~%"))
#<STANDARD-METHOD M-DISPATCH (FOO)>

CL-USER> (defmethod m-dispatch ((arg bar))
           (format t "me: bar~%"))
#<STANDARD-METHOD M-DISPATCH (BAR)>

CL-USER> (m-dispatch (make-foo :bar "bar"))
me: foo
NIL
CL-USER> (m-dispatch (make-bar :foo "foo"))
me: bar
NIL
```

The above shows the dynamic dispatch on the different structure types `'foo` and `'bar`. This is quite nice. For using the structure type in FP we'd 'just' have to come up with a copy function that allows changing the values when doing a copy of the object.

In Scala this works quite nicely with case classes where a copy of an immutable object can be done like this:

```scala
case class MyObject(arg1: String, arg2: Int)

val myObj1 = MyObject("foo", 1)

val myObj2 = myObj1.copy(arg1 = "bar", arg2 = 2)
```

###### Modf to the rescue

The <a href="https://github.com/smithzvk/modf" target="_blank" class="link">Modf</a> library does exactly that for Common Lisp. `modf` has to be used instead of `setf`. But it works in the same way as `setf`, except that it creates a new instance of the structure instead of modifying the existing structure. Lets see this in action:

```lisp
CL-USER> (defstruct foo (x 1) (y 2))
FOO
CL-USER> (defparameter *foo* (make-foo))
*FOO*
CL-USER> *foo*
#S(FOO :X 1 :Y 2)
CL-USER> (modf (foo-x *foo*) 5)
#S(FOO :X 5 :Y 2)
CL-USER> *foo*
#S(FOO :X 1 :Y 2)
```

Following this little example we see that `modf` doesn't touch the original `*foo*` instance but creates a new one with `x = 5`. This is pretty cool. It's getting better. This also works for standard objects of CLOS:

```lisp
CL-USER> (defclass my-class () 
           ((x :initform 1)
            (y :initform 2)))
#<STANDARD-CLASS MY-CLASS>
CL-USER> (defparameter *my-class* (make-instance 'my-class))
*MY-CLASS*
CL-USER> *my-class*
#<MY-CLASS #x302002101F8D>
CL-USER> (slot-value *my-class* 'x)
1 (1 bit, #x1, #o1, #b1)
CL-USER> (slot-value *my-class* 'y)
2 (2 bits, #x2, #o2, #b10)
CL-USER> (modf (slot-value *my-class* 'x) 5)
#<MY-CLASS #x302002250CFD>
CL-USER> (slot-value * 'x)
5 (3 bits, #x5, #o5, #b101)
CL-USER> (slot-value *my-class* 'x)
1 (1 bit, #x1, #o1, #b1)
```

We see from the memory reference that a new instance was created: `#x302002101F8D` vs. `#x302002250CFD`.

So now we basically have our immutable custom types. The only important thing to remember, which again requires discipline, is to use `modf` instead of `setf`.

Even though `modf` also works on the built-in data structures like lists, arrays and hashmaps I would probably still tend to use a library like FSet.

One thing to mention here is that `modf` only does a 'shallow' copy of the data, which means that only a new instance of the 'container' is created while the internal objects (if they are references) are shared.

##### More things that help doing FP

###### Function composition

Common Lisp does not have a construct to compose functions other than just wrapping function calls like: `(f(g(h()))`. But that is not nice to read and it actually turns around the logical order of function calls. The defacto standard Common Lisp library <a href="https://common-lisp.net/project/alexandria/" target="_blank" class="link">alexandria</a> has a function for that which can ben used like `(compose h g f)`:

```lisp
CL-USER> (funcall (alexandria:compose #'1+ #'1+ #'1+) 1)
4 (3 bits, #x4, #o4, #b100)
```

This generates a composition function of the three functions `1+` like: `(1+ (1+ (1+ 1)))` which then can be called using `funcall`, or provided as a higher-order function. The call order is still from right to left.

There is another alternative way of composing functions which comes from Clojure. It's actually rather a piping than a composition. Elixir also knows this as the `|>` operator. In Clojure it's called 'threading'. In Common Lisp there 3 different libraries available which implement this. The one used here is <a href="https://github.com/phoe/binding-arrows/" target="_blank" class="link">binding-arrows</a>. There are a few more operators (macros) available for threading with slightly different features than the used `->`. I like this a lot and use it often.

```lisp
CL-USER> (binding-arrows:->
           1
           1+
           1+
           1+)
4 (3 bits, #x4, #o4, #b100)
```

The normal 'thread' arrow `->` passes the previous value as the first argument to the next function. There is also a `->>` arrow operator which passes the value as the last argument.

###### Pattern matching

Pattern matching is kind of standard on languages that have FP features. In Common Lisp pattern matching is not part of the standard language features. But the library <a href="https://github.com/guicho271828/trivia" target="_blank" class="link">Trivia</a> fills that gap. Trivia has an amazing feature set. It can match (and capture) on all native Common Lisp data structures, including structure and class slots. There are extensions for pattern matching on regular expressions and also for the before mentioned FSet library. And it can be relatively easily expanded with new patterns. The documentation is OK but could be more and better structured.

Here a simple example:

```lisp
;; matching on an FSet map
(match (map (:x 5) (:y 10))
   ((fset-map :x x :y y)
    (list x y)))
=> (5 10)
          
;; matching on a list with capturing the tail
(match '(1 2 3)
   ((list* 1 tail)
    tail))
=> (2 3)
```

###### Currying

<a href="https://en.wikipedia.org/wiki/Currying" target="_blank" class="link">Currying</a> is something you see in most FP languages. It is a way to decompose one function with multiple arguments into a sequence of functions with less arguments. In practical terms it reduces the dimension of available inputs to a function. For example, say you have a function `coords` that takes 2 arguments and produces a coordinate in a x-y coordinate system. With currying we can lock one dimension, x or y.

Say we have a function:

```lisp
CL-USER> (defun coords (x y)
           (cons x y))
COORDS
```

Now I want to lock the x coordinate to a value, say 1:

```lisp
CL-USER> (curry #'coords 1)
#<COMPILED-LEXICAL-CLOSURE (:INTERNAL CURRY) #x3020022BB83F>
```

The `curry` function here creates a new function that locks the x coordinate to 1 and now supports only one argument. Calling this now produces:

```lisp
CL-USER> (funcall * 2)
(1 . 2)
CL-USER> (funcall ** 5)
(1 . 5)
```

(`*` denotes the last, `**` the second last result in the REPL.)  
So currying did destructure the `coord` function call into two function calls. But the curried function can be stored and reused. It represents only a single dimention from the original 2 dimentional set.

Common Lisp also doesn't have currying built-in. But it's easy to create. The following function did the trick above:

```lisp
CL-USER> (defun curry (fun &rest cargs)
           (lambda (&rest args)
             (apply fun (append cargs args))))
CURRY
```

Though there is no need to create this. This is also part of Alexandria library. Which in addition to this also provides `rcurry` to curry from the right.

#### Conclusion

It is possible to do functional programming in languages that are not made for pure FP. It is important to separate the areas where side-effects may happen ('imperative-shell') and where not ('functional-core'). Functions in the 'functional core' should be pure functions that don't modify input parameters. Using immutable data structutes is a big help doing that. But immutable data structures are not always available, in that case one has to manually copy mutable data structures and operate on the copies. This requires discipline. In multi-threaded environment it might still be worth the effort for the gain of simplicity and reasonability.