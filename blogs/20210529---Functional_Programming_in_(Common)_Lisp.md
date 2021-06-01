Functional programming in (Common) Lisp

Functional programming (FP) is again getting popular in recent years. Why again? Because the paradigm of FP is very old. Early languages like Lisp, APL from the 1950/60s were already functional. Later imperative languages like C, Pascal, etc. replaced functional languages to some degree. Since some years functional languages become again more popular.

### What is the difference between the two styles?

### Functional programming

As the name suggests, FP is all about functions. About composing functions and about applying those functions on data to transform the data. Functions can be anonymous functions created at runtime or named functions. FP is declarative. It is more important to say _what_ should be done than _how_.  
A more strict application of FP uses pure functions. Expressions consisting of pure functions are _referencial transparent_ which means that an expression can be replaced with the resulting value, and vice versa (<a href="https://en.wikipedia.org/wiki/Referential_transparency" target="_blank" class="link">wiki</a>). This means that functions that have side-effects are not referencial transparent. Referencial transparency then also implies that parameters to functions (the input set) are not changed. A pure function produces new data, but doesn't change the old data where the new data was produced from. It is said that those functions are not "destructive". Additionally this implies that there exists some immutability regarding data structures, whether the data structures are immutable by design or functions just don't mutate the not-necessarily-immutable data structures (the former requires a lot less discipline from the programmer than the later). Those characteristics are often also mentioned together with data-driven development.  
Aside of the practical benefits of the above, like: it's a lot easier to reason on pure functions and they tend to be easier to maintain (especially in multi-core and multi-threaded environments) and a (subjective) nicer way to code. Though it's of course still possible to write crap code that no one understands and is hard to maintain. The skill of the programmer is still imperative.  
There are also disadvantages like: FP tends to be more memory consuming and requires more CPU cycles. Because practically more data is produced which means more memory allocations are necessary and more memory must be cleaned up which costs garbage collector time. FP languages from very early on had garbage collectors. In FP is quite a bit of math involved with <a href="https://en.wikipedia.org/wiki/Category_theory" target="_blank" class="link">category theory</a>, morphisms, functors, etc. I will concentrate on practical aspects in this blog post.

#### Design considerations

From a design perspective, FP programs usually have a purity core where only data transformations are taking place in form of pure functions, a 'functional core'. But even in FP programs there have to be side-effects, and there is state. Languages deal differently with this. Haskell for example knows 'monads' to deal with side-effects and state. Erlang/Elixir have processes where state is locked in. Side-effects, state as well as threading should happen on the system boundaries. The <a href="https://en.wikipedia.org/wiki/Hexagonal_architecture_%28software%29" target="_blank" class="link">hexagonal architecture</a> is also something that nicely applies to FP.

#### Assignments 

Some FP languages even don't know assignments. Erlang/Elixir for example has a `=` operator, but it's not an assignment operator. The meaning is different. The `=` actually is a match operator, similar as in mathmatics where you say `x = y` which means that `x` is equal to `y`. Let's have a quick look at Erlang:

```erlang
4> X = 1.
1
5> Y = 2.
2
6> X = Y.
** exception error: no match of right hand side value 2
```

The `X` and `Y` variables here take the corresponding values of `1` and `2` because at this point they are unset. Since they are unset the `=` sees to it that `X` matches with `1` and fills `X`.  
But when both variables are set and `=` is used the match fails.

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

#### Design Patterns

Just a few words on this. Most of the design patterns of object-oriented programming, as captured in the Gang of Four book, apply also for FP, but their implementation is in many cases much simpler. I.e. A strategy pattern in FP is just a function. A visitor pattern could in a simple form just be a reduce function. But more on that in my <a href="/blog/Patterns+-+Abstract-Factory" target="_blank" class="link">blog series</a> about desing pattern in Common Lisp.


### Imperative programming

Imperative programming (IP) on the other hand is more about mutating the state of a machine with every instruction/statement. I would tend to say that also Object-Oriented Programming (OOP) is imperative. The difference being that OOP allows to give more structure to the program. From a memory and CPU perspective the paradigm of IP is actually more efficient. And I've read somewhere (can't remember where, but it makes sense to me) that IP did replace FP in the mid to late of last century because memory was expensive and CPUs were not fast, and IP clearly had an advantage there when the value of a memory location is just changed instead of a new memory location allocated and the old one has to be cleaned up.  
But in todays multi-core and multi-threaded computing world state is a problem. It's not possible without state but how it is dealt with is important and different in FP.

So, but this blog post should actually be about FP and Common Lisp.

### Now more on Common Lisp

Common Lisp is a Lisp (obviously). Lisps have always had functions as first-class elements of the language. Functions can be anonymous, or can have a name, they can be created at runtime and they can be passed around in the same way as strings or other elements can. Every function returns something, even is it's just `nil` (for an empty function).  
Common Lisp is insofar a bit special as it allows to program in muliple paradigms. Historically it had to capture and consolidate all the Lisp implementations flowing around at the time. So it has a rather large feature spec and it allows both OOP (with <a href="https://en.wikipedia.org/wiki/Common_Lisp_Object_System" target="_blank" class="link">CLOS</a>) and FP. <a href="https://en.wikipedia.org/wiki/Scheme_(programming_language)" target="_blank" class="link">Scheme</a> for example doesn't have OOP elements. Common Lisp doesn't have the very strict functional characteristics, like the mentioned assignments above. Functions in Common Lisp can have side effects and Common Lisp has assignments in form of `set`, `setq` and `setf`. For FP this means that the programmer has to be more disciplined in how he programms and which elements of the language he uses. But it's of course possible.  
When we look at the important characteristics for FP:

- first-class functions
- immutable data structures
- non-destructive functions

Then we have to be a bit careful what to use of all the things available in Common Lisp. All data structures in Lisp are mutable. Lists are still useable because they are usually used in a non-destructive way. Like the function `cons` creates a new list by prepending a new element to the head of to a list. This means that the old list is not modified or destroyed but a new list is created. `delete-if` (in contrast to `remove-if`) is destructive because it modifies the input list. The array/vector and hashmap data structures and their functions are destructive and shouldn't be used when doing FP, or you create a copy for each change. See `modf` later.

#### Immutable data structures - FSet

The immutable data structures library <a href="https://common-lisp.net/project/fset/Site/index.html" target="_blank" class="link">FSet</a> should be a good fit when doing FP in Common Lisp. Of course FSet defines a large set of functions for finding, comparing, building unions, etc.

There are more alternatives offering this functionality. See for example <a href="https://github.com/ndantam/sycamore" target="_blank" class="link">Sycamore</a>.

#### Custom immutable types

While immutable maps are commonly used in FP instead of classes or structure types, they have one disadvantage. They don't allow mapping on a type. In some FP languages like Erlang/Elixir it's possible to dispatch a function based on a destructuring of the function arguments, like a map or list. But Elixir also allows to define a type for a map structure which helps in the dispatch. And to give the map a 'name' could be a good thing.  
In Common Lisp it would be cool to use the generic functions facility also for FP because it allows multi-dispatch and is generally a nice feature. But it can't destructure lists or maps on function arguments, it can only check on a type or on symbols using `eql`. Both won't work when using FSet.  
So in addition to the data structures available in FSet the structure type in Common Lisp (`defstruct`) could still be useable. It defines a type, so we can use it with generic functions, we can check equality on the slots with `equalp`, and we can set the slots to `:read-only` which prevents from changing the slot values. `defstruct` automatically generates a 'copier' function that copies a structure. This copy is just a flat copy and it doesn't allow to change values while copying. Let's have a quick look at some of the structure things:

```lisp
CL-USER> (defstruct foo (bar "" :read-only t))
FOO
CL-USER> (defstruct bar (foo "" :read-only t))
BAR
```

The option `:read-only` has the effect that `defstruct` doesn't generate 'setter' functions to change the slots. (though to be thorough it is still possible to change the slot values using `slot-value` function. But the public interface does indeed disallow it.)

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

The above shows the multi-dispatch on the different structure types `'foo` and `'bar`. This is quite nice. For using the structure type in FP we'd 'just' have to come up with a copy function that allows changing the values on the copy.

##### Modf to the rescue

The <a href="https://github.com/smithzvk/modf" target="_blank" class="link">Modf</a> library does exactly that. `modf` has to be used instead of `setf`. But it works in the same way as `setf`, except that it creates a new instance of the structure instead of modifying the existing structure. Lets see this in action:

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

Regular classes don't implement `print-object` automatically. But we see from the memory reference that a new instance was created: `#x302002101F8D` vs. `#x302002250CFD`.

So now we basically have our immutable custom types. The only important thing to remember, which again requires discipline, is to use `modf` instead of `setf`.

Even though `modf` also works on the built-in data structures like lists, arrays and hashmaps I would probably still tend to use a library like FSet.

#### More things that help doing FP

##### Function composition

Common Lisp does not have a construct to compose functions other than just wrapping function calls like: `(f(g(h()))`. But that is not nice to read. The defact standard Common Lisp library <a href="https://common-lisp.net/project/alexandria/" target="_blank" class="link">alexandria</a> has a function for that which then looks like this:

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

##### Pattern matching

Pattern matching is kind of standard on languages that have FP features. In Common Lisp pattern matching is not part of the standard. But the library <a href="https://github.com/guicho271828/trivia" target="_blank" class="link">Trivia</a> fills that gap. Trivia has an amazing feature set. It can match (and capture) on all native Common Lisp data structures, including structure and class slots. There are extensions for pattern matching on regular expressions and also for the before mentioned FSet library. And it can be relatively easily expanded with new patterns. The documentation is OK but could be more and better structured.

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


##### Currying

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

`*` denotes the last, `**` the second last result in the REPL.
So currying did destructure the `coord` function call into two function calls. But the curried function can be stored and reused. It represents only a single dimention from the original 2 dimentional set.

Common Lisp also doesn't have currying built-in. But it's easy to create. The following function did the trick above:

```lisp
CL-USER> (defun curry (fun &rest cargs)
           (lambda (&rest args)
             (apply fun (append cargs args))))
CURRY
```

Though there is no need to create this. This is also part of Alexandria library. Which also provides `rcurry` to curry from the right.

### More info

There are more references on the web regarding functional programming in Common Lisp. The following I found useful.

- <a href="https://ambrevar.xyz/modern-common-lisp/index.html" target="_blank" class="link">https://ambrevar.xyz/modern-common-lisp/index.html</a>
- <a href="https://lisp-journey.gitlab.io/blog/functional-programming-in-common-lisp/" target="_blank" class="link">https://lisp-journey.gitlab.io/blog/functional-programming-in-common-lisp/</a>

