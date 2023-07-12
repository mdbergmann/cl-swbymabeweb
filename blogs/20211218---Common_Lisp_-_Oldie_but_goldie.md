This article should be a brief introduction to Common Lisp. Brief, because Common Lisp is a rather large and complex system. It has many features. I will try to concentrate on the basics and some exceptional features that stand out for me. I started writing it for myself in order to understand certain concept better, like symbols. But it might be useful for others as well.  


<a id="orgbd1d8a8"></a>

#### How did I come to Common Lisp

I have been working with various languages and runtimes since the start of my career 22 years ago. Beginning of 2019 I wanted to find something else to closely look into that is not JVM based (which I'm mostly been working with since close to 20 years starting with Java 1.1).  
For some reason, which I can't recall, I haven't really been introduced to Lisps, ever. I also can't recall why 2019 I thought that I should take a look at Lisps. So I took a look at Clojure first. Clojure is a great language but it was again on the JVM. I wanted something native, or at least some other runtime. After some excursions to Erlang, Elixir and LFE (Lisp Flavoured Erlang, which all three are extremely interesting as well) and Scheme I finally found Common Lisp and didn't regret it.  


<a id="org1626589"></a>

#### Brief history

First drafts of Common Lisp appeared 1981/82. While mostly a successor of Maclisp it tried to unify and standardize Maclisp and the various other successors of Maclisp. In 1994 Common Lisp was an ANSI standard.  


<a id="org6d8e1e5"></a>

#### Age advantages

Since then the standard hasn't changed. That can of course be seen as a bad thing, when things don't change. But actually I believe it is a good thing. Common Lisp is even today surprisingly 'modern' and has many features of todays languages, partially even more features than 'modern' languages. And what it doesn't have can be added in form of libraries so that it feels as being part of the language.  
Common Lisp is a quite large and complex package. After this long time there are of course some dusty corners. But all in all it is still very attractive and has an active community.  
Because the standard didn't change since 1994 any code written since then should still be compile and runable on newer compilers and runtime implementations (where there are a few, see below) which were written in a portable way.  

#### Content

- <a href="#orgbd62e83" class="link">Basics</a>
    - <a href="#org0669756" class="link">Lists</a>
    - <a href="#orgfeceb96" class="link">Functions</a>
        - <a href="#org7ce6981" class="link">Mandatory arguments</a>
        - <a href="#org59e6b50" class="link">Optional arguments</a>
        - <a href="#orgde5876c" class="link">Key arguments</a>
        - <a href="#org26456fd" class="link">Rest arguments</a>
        - <a href="#orga703ee0" class="link">Mixing arguments</a>
    - <a href="#orgac51641" class="link">Lambdas</a>
    - <a href="#org2266afa" class="link">Macros</a>
    - <a href="#org0fb504c" class="link">Packages</a>
    - <a href="#org6d16428" class="link">Symbols</a>
        - <a href="#org08a2a66" class="link">Unbound symbols</a>
        - <a href="#orgbe23e88" class="link">Bound symbols</a>
        - <a href="#orgcff0d95" class="link">The Lisp reader</a>
- <a href="#org0e6fc54" class="link">Types</a>
    - <a href="#org5bec702" class="link">Everything has a type</a>
    - <a href="#org19d1a44" class="link">Create new types</a>
    - <a href="#org7f1b0b1" class="link">Check for types</a>
        - <a href="#org8c56ab4" class="link">check-type</a>
        - <a href="#orgaf49559" class="link">declaim</a>
- <a href="#org8f1ca12" class="link">Error handling</a>
    - <a href="#org21a3108" class="link">Conditions</a>
    - <a href="#orgecc7ced" class="link">unwind-protect</a>
    - <a href="#org3abe3e7" class="link">Handle condition with stack unwind</a>
    - <a href="#org50c740a" class="link">Restarts / Handle condition without stack unwind</a>
- <a href="#org0761f12" class="link">CLOS and object-oriented programming</a>
- <a href="#orgab03075" class="link">Multi dispatch</a>
- <a href="#orga5282f9" class="link">Debugging</a>
- <a href="#orgb5c1978" class="link">Library management with Quicklisp</a>
- <a href="#org36aa9a1" class="link">Runtimes/compilers (CCL, SBCL, ECL, Clasp, ABCL | LispWorks, Allegro)</a>
- <a href="#org833736f" class="link">Image based</a>
    - <a href="#org1b55d4e" class="link">image snapshot</a>
    - <a href="#org6ad489a" class="link">load from image</a>
- <a href="#org5f6afae" class="link">Functional programming</a>
- <a href="#org1ce34e8" class="link">Resources</a>


<a id="orgbd62e83"></a>

### Basics

Let me run through some of the basic features of Common Lisp. Those basic features are likely also available in other languages. Common Lisp has some unique features that I'll be talking about later.  


<a id="org0669756"></a>

#### Lists

Since the name 'Lisp' is an abbreviation for List Processing we should have a quick look at lists. Lists are the corner stone of the Lisp language because every Lisp construct/form is a list, also called s-expression. A list is bounded by parentheses `(` and `)`. So `'(1 2 3 4)` is a list of the numbers 1, 2, 3 and 4. This list represents data. Lists representing data are usually quoted. Quoted means that this list, or the list elements, are not evaluated. The `'` in front of the first parenthesis denotes a quoted list. But `(myfunction "abc")` is also a list representing code, which is evaluated. By convention the first list entry must either be a function name, a macro operator, a lambda expression or a special operator. `if` for example is a special operator. The other list elements are usually function, or operator arguments. Lists can be nested. In most cases Lisp programs are trees of lists.  


<a id="orgfeceb96"></a>

#### Functions

Functions are nothing special. Every language knows them. A simple function definition (which does nothing) looks like this:

```lisp
(defun my-fun ())
(my-fun)
```

```nohighlight
+RESULTS:
: NIL
```

A function in Common Lisp always returns something, even if not explicitly. This simple function just returns `NIL`, which in Common Lisp has two meanings. a) it has a boolean meaning of `false` and b) it means the empty list equal to `'()`.  

Common Lisp provides a very sophisticated set of features to structure function arguments.  


<a id="org7ce6981"></a>

##### Mandatory arguments

Mandatory arguments are simply added to the list construct following the function name. This list construct that represents the arguments is commonly known as *lambda list*. In the following example `arg1` and `arg2` are mandatory arguments.

```lisp
(defun my-fun (arg1 arg2)
  (list arg1 arg2))
(my-fun "Hello" "World")
```

```nohighlight
+RESULTS:
| Hello | World |
```

<a id="org59e6b50"></a>

##### Optional arguments

Optional arguments are defined using the `&optional` keyword:

```lisp
(defun my-fun (arg1 &optional opt1 (opt2 "buzz" opt2-p))
  (list arg1 opt1 opt2 opt2-p))
(list
 (my-fun "foo")
 (my-fun "foo" "bar")
 (my-fun "foo" "bar" "my-buzz"))
```

```nohighlight
+RESULTS:
| foo | NIL | buzz    | NIL |
| foo | bar | buzz    | NIL |
| foo | bar | my-buzz | T   |
```

The first optional `opt1` does not have a default value, so if undefined it'll be `NIL`. The second optional `opt2` when undefined is populated with the given default value "buzz". The  optional `opt2-p` predicate indicates whether the `opt2` parameter has been given or not. Sometimes this is useful in succeeding code.  


<a id="orgde5876c"></a>

##### Key arguments

`key` arguments are similar as named arguments in other languages. The ordering of `key` arguments is not important and is not enforced. They are defined with a the `&key` keyword:

```lisp
(defun my-fun (&key key1 (key2 "bar" key2-p))
  (list key1 key2 key2-p))
(list
 (my-fun)
 (my-fun :key1 "foo")
 (my-fun :key1 "foo" :key2 "buzz"))
```

```nohighlight
+RESULTS:
| NIL | Foo  | NIL |
| Bar | Foo  | NIL |
| Bar | Buzz | T   |
```

`key` arguments are optional. Similarly as `&optional` arguments a default value can be configured and a predicate that indicates whether the parameter was provided or not. Defining `key2-p` is optional.  


<a id="org26456fd"></a>

##### Rest arguments

`rest` arguments are arguments that have not already been captured by mandatory, optional, or key. So they form a rest. This rest is available in the body as a list. In the example below defined by `rest` keyword. `rest` arguments are sometimes useful to pass them on to `APPLY` function.

```lisp
(defun my-fun (arg1 &optional opt1 &rest rest)
        (list arg1 opt1 rest))
(list
 (my-fun "foo" :rest1 "rest1" :key1 "buzz")
 (my-fun "foo" "opt1" :rest1 "rest1" :key1 "buzz"))
```

```nohighlight
+RESULTS:
| foo | :REST1 | (rest1 :KEY1 buzz)        |
| foo | opt1   | (:REST1 rest1 :KEY1 buzz) |
```

<a id="orga703ee0"></a>

##### Mixing arguments

As you can see it is possible to mix `optional`, `key` and `rest` arguments. However, some care must be taken with mixing `optional` and `key` because the key of the `key` argument could be taken as an `optional` argument. Similarly with `rest` and `key` arguments as can be seen in the examples above. In most use-cases you'd either have `optional` or `key` together with mandatory arguments.  


<a id="orgac51641"></a>

#### Lambdas

Lambdas are anonymous functions created at runtime. Other than that they are similar to `defun`s, regular/named functions. They can be used in place of a function name like this:

```lisp
((lambda (x) x) "foo")  ;; returns "foo"
```

```nohighlight
+RESULTS:
: foo
```

In which case the lambda is immediately evaluated. The function 'is applied' on the value "foo", represented as the argument x. The function then returns x.  
In other cases, i.e. when a lambda is bound to a variable one need to invoke the lambda using `funcall`:

```lisp
(let ((my-fun (lambda (x) x)))
  (funcall my-fun "foo"))
```

```nohighlight
+RESULTS:
: foo
```

This is in contrast to Scheme, or other Lisp-1s, where also `my-fun` can be used in place of the function name and would just be evaluated as a function.  
Common Lisp is a Lisp-2, which means that there are separate environments for variables and functions. In the above example `my-fun` is a variable. In order to evaluate it as a function one has to use `FUNCALL`.  

Lambdas are first-class objects in Lisp which means they can be created at runtime, bound to variables and passed around as function arguments or function results:

```lisp
(defun my-lambda ()
  (lambda (y) y))
(list (type-of (my-lambda)) 
      (funcall (my-lambda) "bar"))
```

```nohighlight
+RESULTS:
| function | bar |
```

The "Lambda-calculus" (Alonzo Church, 1930) is a mathematical formal system based on variables, function abstractions (lambda expressions) and applying those using substitution. This can be used for any kind of computation and is Turing machine equivalent (or can be used to simulate a Turing machine).  
So if one would stack/nest lambda expression in lambda expression in lambda expression and so on, where a lambda expression is bound to a variable and the computation of this again substitutes a variable, you could have such a lambda-calculus.  
This is of course not so practical and hard to read but this alone would be enough to calculate anything that is calculatable.  


<a id="org2266afa"></a>

#### Macros

Macros are an essential part in Common Lisp. One should not confuse Lisp macros with C macros which just do textual replacement. Common Lisp macros are extremely powerful.  
In short, macros are constructs that generate and/or manipulate code. Lisp macros still stand out in contrast to other languages because Lisp macros just generate and manipulate ordinary Lisp code whereas other languages use an AST (Abstract Syntax Tree) representation of the code and hence the macros must deal with the AST. In Lisp, Lisp is the AST. Lisp is homoiconic.  

Macros are not easy to distinguish from functions. In programs one can not see the difference. Many functions could be replaced by macros. But functions can usually not replace macros. There is a fundamental difference between the two.  
The arguments to macros are passed in a quoted form, meaning they are not evaluated (remember the lists as data above). Whereas parameters to functions are first evaluated and the result passed to the function. The output of macros is also quoted code. For example let's recreate the `when` macro:

```lisp
(defmacro my-when (expr &body body)
  `(if ,expr ,@body))
```

```nohighlight
+RESULTS:
: MY-WHEN
```

When using the macro it prints:

```nohighlight
CL-USER> (my-when (= 1 0)
           (print "Foo"))
NIL
CL-USER> (my-when (= 1 1)
           (print "Foo"))
"Foo"
```

The macro expands the `expr` and `body` arguments. Macros always (should) generate quoted Lisp code, that's why the result of a macro should be a quoted expression. Quoted expressions are not evaluated, they are just plain data (a list), so the macro expression can be replaced with the macro body wherever the macro is used.  
We can expand the macro (using `MACROEXPAND`) to see what it would be replaced with. Let's have a look at this:

```nohighlight
CL-USER> (macroexpand-1 '(my-when (= 1 1)
                          (print "Foo")))
(IF (= 1 1) (PRINT "Foo"))
```

So we see that `my-when` is replaced with an `if` special form. As we said, a quoted expression is not evaluated, so would we use the `expr` argument in the quoted expression we would just get `(IF EXPR ...)`, but we want `expr` to be expanded here so that the right `if` form is created with what the user defined as the `if` test expression. The `,` 'escapes' the quoted expression and will expand the following form. `,expr` is thus expanded to `(` 1 1)= and `,@body` to `(print "Foo")`. The `@` is special as it unwraps (splices) a list of expressions. Since the body of a macro can denote many forms they are wrapped into a list for the `&body` argument and hence have to be unwrapped again on expansion. I.e.:

```lisp
(my-when t
  (print "Foo")
  (print "Bar"))
```

Here the two print forms represent the body of the macro and are wrapped into a list for the `&body` argument like:

```lisp
((print "Foo")
 (print "Bar"))
```

The `@` will remove the outer list structure.  

**when are macros expanded?**  

Macros are expanded during the 'macro expansion' phase. This phase happens before compilation. So the Lisp compiler already only sees the macro expanded code.  


<a id="org0fb504c"></a>

#### Packages

Packages are constructs, or namespaces, to separate and structure data and code similar as in other languages. `DEFPACKAGE` declares a new package. `IN-PACKAGE` makes the named package the current package. Any function, macro or variable definitions are then first of all local to that package where they are defined in. Function, macro or variable definitions can be exported, which means that they are then visible for/from other packages. A typical example of a package with some definitions would be:

```lisp
(defpackage :foo
  (:use :cl)
  (:import-from #:bar
                #:bar-fun
                #:bar-var)
  (:export #:my-var
           #:my-fun))
(in-package :foo)
    
(defparameter my-var "Foovar")
(defun my-fun () (print "Foofun"))
(defun my-internal-fun () (print "Internal"))
```

A package is kind of a lookup table where function names, variable names, etc., represented as symbols (later more on symbols) refer to an object which represents the function, variable, etc. The function `MY-FUN` would be referred to using a package qualified name `foo:my-fun`. The exported 'symbols' are the public interface of the package. Using a double colon one can also refer to internal symbols, like: `foo::my-internal-fun` but that should be done with care as it means accessing implementation details.  
It is also possible to import specific package symbols (functions, variables, etc.) by using the `IMPORT` or `IMPORT-FROM` functions. Any package added as parameter of `:use` will be inherited by the defined package and so all exported symbols of the packages mentioned at `:use` will be known and can be used without using the package qualified name.  


<a id="org6d16428"></a>

#### Symbols

Symbols in Common Lisp are almost everywhere. They reference data and are data themselves. They reference variables or functions. When used as data we can use them as identifiers or as something like enums.  

We can create symbols by just saying `'foo` in the REPL. This will create a symbol with the name "FOO". Notice the uppercase. We also create symbols by using the function `INTERN`.  

Let's have a look at the structure of symbols. We create a symbol from a string by using the `INTERN` function.  


<a id="org08a2a66"></a>

##### Unbound symbols

```lisp
(intern "foo")
```

```nohighlight
+RESULTS:
: |foo|
```

This symbol `foo` was created in the current package (`*PACKAGE*`). We can have a look at `*PACKAGE*` (in Emacs by just evaluating `*PACKAGE*` and clicking on the result):

```nohighlight
#<PACKAGE #x30004000001D>
--------------------
Name: "COMMON-LISP-USER"
Nick names: "CL-USER"
Use list: CCL, COMMON-LISP
Used by list: 
2 present symbols.
0 external symbols.
2 internal symbols.
1739 inherited symbols.
0 shadowed symbols.
```

We'll see that there are 2 internal symbols. One of them is our newly created symbol `foo`. Let's drill further down to the internal symbols.

```nohighlight
#<%PACKAGE-SYMBOLS-CONTAINER #x3020014B3FCD>
--------------------
All internal symbols of package "COMMON-LISP-USER"

A symbol is considered internal of a package if it's
present and not external---that is if the package is
the home package of the symbol, or if the symbol has
been explicitly imported into the package.
    
Notice that inherited symbols will thus not be listed,
which deliberately deviates from the CLHS glossary
entry of `internal' because it's assumed to be more
useful this way.
    
  [Group by classification]
   
Symbols:                Flags:
----------------------- --------
foo                     --------
```

So `foo` is listed as symbol. Let's look at `foo` in detail (in Emacs we can click on `foo`).

```nohighlight
#<SYMBOL #x3020012F958E>
--------------------
Its name is: "foo"
It is unbound.
It has no function value.
It is internal to the package: COMMON-LISP-USER [export] [unintern]
Property list: NIL
```

Here we see the attributes of symbol `foo`. Symbols can be bound to variables, or they can have a function value (Common Lisp is a Lisp-2, which means it separates variables from function names. In a Lisp-1, like Scheme, one cannot have the same name for a variable and function), in which case they refer to a variable or function. Our symbol is neither, it's just a plain symbol.  

We can get the name of the symbol by:

```lisp
(symbol-name (intern "foo"))
```

```nohighlight
+RESULTS:
: foo
```

<a id="orgbe23e88"></a>

##### Bound symbols

Whenever we define a variable (not lexical variables (`let`)), or function we bind a symbol to a variable or function. Let's do this:

```lisp
;; create a variable definition in the current package
(defvar *X* "foo")
```

When we look again in the current package `*PACKAGE*` we see an additional symbol:

```nohighlight
#<%PACKAGE-SYMBOLS-CONTAINER #x3020014B3FCD>
...
Symbols:                Flags:
----------------------- --------
*X*                     b-------
foo                     --------
```

And it is flagged with "b", meaning it is bound, see below.

```nohighlight
#<SYMBOL #x30200145E2EE>
--------------------
Its name is: "*X*"
It is a global variable bound to: "foo" [unbind]
It has no function value.
It is internal to the package: COMMON-LISP-USER [export] [unintern]
Property list: NIL
```

The same can be done with functions. Defining a function with `DEFUN` will create a symbol in the current package whose function object is the function. Let's create a function: `(defun foo-fun ())` and look at the symbol:

```nohighlight
#<%PACKAGE-SYMBOLS-CONTAINER #x3020015C0E8D>
--------------------
Symbols:                Flags:
----------------------- --------
FOO-FUN                 -f------
    
#<SYMBOL #x3020014D1C4E>
--------------------
Its name is: "FOO-FUN"
It is unbound.
It is a function: #<Compiled-function FOO-FUN #x3020014D0A8F> [unbind]
```

<a id="orgcff0d95"></a>

##### The Lisp reader

When a Lisp file is read, or some input from the REPL, it is first of all just a sequence of characters. What the *reader* reads it turns into objects, symbols, and stores those (using `INTERN`) into the current package. It also applies some rules for how the character sequence is converted to the symbol name. Usually those rules include turning all characters to uppercase. So i.e. a function name "foo" creates a symbol with the name `FOO`.  
It is possible to have symbol names with literals. We have seen that when we defined the symbol `|foo|` above. The reader puts vertical bars around "foo" which means the symbol name is literally "foo". This is because we have not applied the conversion rules when using `INTERN`. If we had defined the symbol as `(intern "FOO")` we wouldn't see the vertical bars.  

Let's make an example with a function. Say, we are in a package `MY-P` and we define a function:

```lisp
(defun my-fun () "fun")
```

```nohighlight
+RESULTS:
: MY-FUN
```

The REPL responds with `MY-FUN`. This is the returned symbol from the function definition that was added to the package. When we now want to execute the function we write: `(my-fun)`. When the reader reads "my-fun", it uses `INTERN` to either create or retrieve the symbol (`INTERN` retrieves the symbol if it already exists). It is retrieved if previously the function was defined with `DEFUN` which implicitly, through the reader, creates the symbol and 'attaches' a function object to it. The attached function object can then be executed.  


<a id="org0e6fc54"></a>

### Types

Even though Common Lisp is not statically typed it has types. In fact everything in Common Lisp has a type.  


<a id="org5bec702"></a>

#### Everything has a type

And there are no primitives as they are in Java.

```lisp
(defun my-fun ())
(list
 (type-of 5)
 (type-of "foo")
 (type-of #\a)
 (type-of 'foo)
 (type-of #(1 2 3))
 (type-of '(1 2 3))
 (type-of (cons 1 2))
 (type-of (lambda () "fun"))
 (type-of #'my-fun)
 (type-of (make-condition 'error)))
```

```nohighlight
+RESULTS:
| (INTEGER 0 1152921504606846975) |
| (SIMPLE-BASE-STRING 3)          |
| STANDARD-CHAR                   |
| SYMBOL                          |
| (SIMPLE-VECTOR 3)               |
| CONS                            |
| CONS                            |
| FUNCTION                        |
| FUNCTION                        |
| ERROR                           |
```

<a id="org19d1a44"></a>

#### Create new types

There are different ways to create new types. One is to just create a new structure, or class. New structure types can be created with `DEFSTRUCT`. `DEFCLASS` will create a new class type.

```lisp
(defstruct address 
  (street "" :type string)
  (streetnumber nil :type integer)
  (plz nil :type integer))
(type-of (make-address 
          :street "my-street"
          :streetnumber 1
          :plz 51234))
```

```nohighlight
+RESULTS:
: ADDRESS
```

The `:type` specified in `DEFSTRUCT` is optional but when provided the type is checked on creating a new structure.  
`DEFCLASS` can be used instead of `DEFSTRUCT`. If you build object-oriented software and want to work with inheritance then use `DEFCLASS`, because a struct can't do it. Classes also have the feature of updating the its structure at runtime which structs can't do.  

`deftype` allows to create new types as a combination of existing types. Let's create a new type that represents the numbers from 11 to 50.

```lisp
(defun 10-50-number-p (n)
  (and (numberp n)
       (> n 10)
       (<= n 50)))
(deftype 10-50-number ()
  `(satisfies 10-50-number-p))
```

This snipped creates a predicate function that ensures the number argument is within 10 and 50 (excluding 10 and including 50). The type definition then uses `SATISFIES` with the given predicate function to check the type. So we can then say:

```lisp
(list
 (typep 10 '10-50-number)
 (typep 11 '10-50-number)
 (typep 50 '10-50-number)
 (typep 51 '10-50-number))
```

```nohighlight
+RESULTS:
| NIL | T | T | NIL |
```

The results show that the middle two satisfy this type, the other two not.  


<a id="org7f1b0b1"></a>

#### Check for types

Types can be checked on runtime, or also (partially) on compile time (SBCL has some static type check capability). Checking types usually makes sense for function parameters but can be done anywhere.  


<a id="org8c56ab4"></a>

##### check-type

`CHECK-TYPE` is used to do this. It can be used as follows, considering the `10-50-number` type from above:

```lisp
(defun add-10-50-nums (n1 n2)
  (check-type n1 10-50-number)
  (check-type n2 10-50-number)
  (+ n1 n2))
```

Do we call this as `(add-10-50-nums 10 11)` we will get a type error raised:

```nohighlight
The value 10 is not of the expected type 10-50-NUMBER.
   [Condition of type TYPE-ERROR]
```

Under the hoods `CHECK-TYPE` is a wrapper for `ASSERT` call.  


<a id="orgaf49559"></a>

##### declaim

With `DECLAIM` one can make declarations for variables or functions we'd have to:

```lisp
(declaim (ftype (function (10-50-number 10-50-number) 10-50-number) add-10-50-nums))
(defun add-10-50-nums (n1 n2)
  (+ n1 n2))
```

This declares the input and output types of the function `ADD-10-50-nums`. However, this will not do type checks at runtime, and whether it will be checked at compile time depends on the Common Lisp implementation. SBCL will check it, CCL doesn't, in which case it will be useable as documentation only.  

It's not nicely readable though. The library <a href="https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#types" class="link">Serapeum</a> adds some syntactic sugar to make this more nice. I.e. the `DECLAIM` from above can be written as:

```lisp
(-> add-10-50-nums (10-50-number 10-50-number) 10-50-number)
```

<a id="org8f1ca12"></a>

### Error handling

Common Lisp has some unique error handling properties. The "Restarts". We will see later some examples. Let's first check what conditions are.  


<a id="org21a3108"></a>

#### Conditions

Conditions are objects of a type `condition`. The CLHS says: "an object which represents a situation". So conditions are far more than errors. Any condition/situation can be transported by conditions. Now while a condition itself can represent a situation like an error, there are multiple ways to raise a condition and multiple ways to handle a condition depending on the need. For example: an error condition can be just signaled (using `SIGNAL`) in which case nothing much will happen if the condition is not handled at all. `SIGNAL` will just return `NIL` in that case. However, when an error condition is raised using `ERROR`, then it must be handled, otherwise the runtime will bring up the debugger. There is also `WARN`, which will print a warning message if the condition is not handled.  


<a id="orgecc7ced"></a>

#### unwind-protect

`UNWIND-PROTECT` is similar as a try-finally in other languages, Java for example. It protects the stack from unwinding further and allows to call a clean-up form.

```lisp
(defun do-stuff ())
(defun clean-up ())
    
(unwind-protect
     (do-stuff)  ;; can raises condition
  (clean-up))
```

```nohighlight
+RESULTS:
: NIL
```

<a id="org3abe3e7"></a>

#### Handle condition with stack unwind

`HANDLER-CASE` is a bit more sophisticated than `UNWIND-PROTECT`, it allows to differenciate on the raised condition and do a different handling. This is comparable to a try-catch-finally (i.e. in Java or elsewhere). This is nothing special really, so let's move on to Restarts.  


<a id="org50c740a"></a>

#### Restarts / Handle condition without stack unwind

Restarts is a unique feature of Common Lisp that I have not seen elsewhere (though that doesn't necessarily have to mean much). It allows to handle conditions without unwinding the stack. If not handled by a handler the runtime will drop you into the debugger with restart options where the user can choose an available way to continue. Let's make a very simple example to show how it works:

```lisp
(define-condition my-err1 () ())
(define-condition my-err2 () ())
(define-condition my-err3 () ())
(define-condition my-err4 () ())
    
(defun lower (err-cond)
  (restart-case
      (error err-cond)
    (restart-case1 (&optional arg)
      (format t "restart-case1 arg:~a~%" arg))
    (restart-case2 (&optional arg)
      (format t "restart-case2 arg:~a~%" arg))
    (restart-case3 (&optional arg)
      (format t "restart-case3 arg:~a~%" arg))))
    
(defun higher ()
  (handler-bind
      ((my-err1 (lambda (c)
                  (format t "condition: ~a~%" c)
                  (invoke-restart 'restart-case1 "foo1")))
       (my-err2 (lambda (c)
                  (format t "condition: ~a~%" c)
                  (invoke-restart 'restart-case2 "foo2")))
       (my-err3 (lambda (c)
                  (format t "condition: ~a~%" c)
                  (invoke-restart 'restart-case3 "foo3"))))
    (lower 'my-err1)
    (lower 'my-err2)
    (lower 'my-err3)
    (lower 'my-err4)))
```

In the example `HIGHER` calls `LOWER`. `LOWER` immediately raises a condition with `ERROR`. You'd normally of course have some other code here that would raise the conditions instead. To setup restarts one uses `RESTART-CASE` everywhere where there is potentially a way to get out of a situation without loosing the context. The `RESTART-CASE` actually looks very similar to a `HANDLER-CASE`. The restart cases can take arguments that can be passed in from a calling module. In our case here the restarts cases just dump a string to stdout.  
The magic in `HIGHER` to actually 'invoke' the restart targets is achieved with `HANDLER-BIND`. It is possible to automatically invoke restarts by differenciating on the condition. The restart cases are invoked with `INVOKE-RESTART`. This allows to also pass the argument to the restart case handler that could create the basis for resuming the computation. If a condition handler is not bound the condition will bubble further up the call chain. So it's possible to bind condition handlers on different levels where on a higher level one possibly has more oversight to decide which restart to use.  
Executing `HIGHER` will give the following output:

```nohighlight
CL-USER> (higher)
condition: Condition #<MY-ERR1 #x302001398D9D>
restart-case1 arg:foo1
condition: Condition #<MY-ERR2 #x30200139886D>
restart-case2 arg:foo2
condition: Condition #<MY-ERR3 #x30200139833D>
restart-case3 arg:foo3
```

This output is from calling `LOWER` function with condition types `MY-ERR1`, `MY-ERR2` and `MY-ERR3`. When we now call `LOWER` with `MY-ERR4` we will be dropped into the debugger, because there is no condition handler for `MY-ERR4`. But in this case that's exactly what we want. The debugger now offers the three restarts we have set up (plus some standard ones). So we see:

```nohighlight
Condition #<MY-ERR4 #x302001445A7D>
   [Condition of type MY-ERR4]
    
Restarts:
 0: [RESTART-CASE1] #<RESTART RESTART-CASE1 #x251B7B8D>
 1: [RESTART-CASE2] #<RESTART RESTART-CASE2 #x251B7BDD>
 2: [RESTART-CASE3] #<RESTART RESTART-CASE3 #x251B7C2D>
 3: [RETRY] Retry SLY mREPL evaluation request.
 4: [*ABORT] Return to SLY's top level.
 5: [ABORT-BREAK] Reset this thread
 --more--
    
Backtrace:
 0: (LOWER MY-ERR4)
 1: (HIGHER)
 2: (CCL::CALL-CHECK-REGS HIGHER)
 3: (CCL::CHEAP-EVAL (HIGHER))
 4: ((:INTERNAL SLYNK-MREPL::MREPL-EVAL-1))
 --more--
```

We could now choose one of our restarts manually to have the program continue in a controlled way by maybe retrying some operation with a different set of parameters.  


<a id="org0761f12"></a>

### CLOS and object-oriented programming

CLOS (Common Lisp Object System) is an object oriented class system (or framework) in Common Lisp. It has a separate name, but it is part of the Common Lisp standard and part of every Common Lisp runtime. In very basic terms it allows to define classes using `DEFCLASS`. CLOS supports multi-inheritance. The behavior of classes (if something like that exists in Common Lisp - I'd say it doesn't) are structures keeping state but don't have behavior as such (and that's a good thing). The behavior to classes is added with generic functions. There is some default behavior, like `INITIALIZE-INSTANCE`, or `PRINT-OBJECT`, etc. which is behavior defined as generic functions. This default behavior of classes is defined by **meta-classes**, classes that define classes. A pretty powerful thing. This would allow me to create my own base class behavior. Comparing this to Java one could very remotely say that this is like creating a new `Object` class that behaves different than the default `Object` class.  

Generic functions allow to be overridden. This is driven by providing method (`DEFMETHOD`) definitions which define certain concrete object types as parameters which are subclasses of some class. Say I have a class Person and have a method definition that works on that object. To override this method I'd have to define a method that works on, say Employee object type, a subclass ob Person. Then it's possible to also call the implementation of the super class using `CALL-NEXT-METHOD` (see chapter 'Multi dispatch'; `float` is a subtype of `number`). Though overriding behavior like that is something that one should try to avoid these days. Composition over inheritance is popular. Not without reason. Those very deep inheritance graphs are considered problematic for a few reasons. One is that it's harder to reason about the methods and what they do. The other problem is that inheritance has higher coupling than composition.  


<a id="orgab03075"></a>

### Multi dispatch

Multi, or dynamic dispatch is not something that all languages have (some do) but it's quite handy. In Common Lisp it's tied to generic functions. Let's have a look:

```lisp
(defgeneric print-my-object (obj))
    
(defmethod print-my-object ((obj number))
  (format nil "printing number: ~a~%" obj))
    
(defmethod print-my-object ((obj float))
  (format nil "printing float number: ~a, ~a~%" obj (call-next-method)))
    
(defmethod print-my-object ((obj string))
  (format nil "printing string: ~a~%" obj))
    
(defmethod print-my-object ((obj keyword))
  (format nil "printing keyword: ~a~%" obj))
    
(list
 (print-my-object "foo")
 (print-my-object :foo)
 (print-my-object 5)
 (print-my-object .5))
```

```nohighlight
+RESULTS:
| printing string: foo                             |
| printing keyword: FOO                            |
| printing number: 5                               |
| printing float number: 0.5, printing number: 0.5 |
```

Isn't this cool? This works with objects of classes defined with `DEFCLASS`, or structures defined with `DEFSTRUCT`, even conditions. Well, actually with objects of any, including built-in types. There is just an implicit type-check happening on the argument. But there is a certain performance downside. The runtime has to check which function to call by comparing the types on runtime.  


<a id="orga5282f9"></a>

### Debugging

As a TDD'er (Test-Driven Development) I don't much use the debugging facilities in general, also not in other languages. Because the TDD increments are so small and the feedback is so immediate that I have used a debugger very rarely in the last years.  
However, there are two facilities which I'd like to mention. One I use sometimes: `TRACE`. Trace allows to trace specific functions with its inputs and outputs. Say we have a function `FOO`:

```lisp
(defun foo (arg)
  (format nil "hello ~a" arg))
```

We can now enable the tracing of it by saying `(trace foo)`.  
When we now call `FOO` we'll see:

```nohighlight
CL-USER> (foo "world")
0> Calling (FOO "world") 
<0 FOO returned "hello world"
"hello world"
```

Another thing which I'd like to mention is `BREAK`. `BREAK` enters the debugger when placed in the source code. When we have the function:

```lisp
(defun foo (arg)
  (break))
```

and call `FOO` the debugger will open and we can get a glimpse at the stack trace and can inspect the variables.

```nohighlight
Break
   [Condition of type SIMPLE-CONDITION]
    
Restarts:
 0: [CONTINUE] Return from BREAK.
 1: [RETRY] Retry SLY mREPL evaluation request.
 2: [*ABORT] Return to SLY's top level.
 3: [ABORT-BREAK] Reset this thread
 4: [ABORT] Kill this thread
    
Backtrace:
 0: (FOO "world")
 1: ((CCL::TRACED FOO) "world")
 2: (CCL::CALL-CHECK-REGS FOO "world")
 3: (CCL::CHEAP-EVAL (FOO "world"))
 4: ((:INTERNAL SLYNK-MREPL::MREPL-EVAL-1))
 --more--
```

In Sly/Slime the Backtrace elements can be opened and further inspected. This is quite handy sometimes.  


<a id="orgb5c1978"></a>

### Library management with Quicklisp

Library (dependency) management was quite late in Common Lisp. Apache Maven in the Java world existed since 2004 and was probably one of the first of its kind. <a href="https://www.quicklisp.org/beta/" class="link">Quicklisp</a> exists since 2010 (as far as I could research). Nowadays remote and local library version management is common and supports even GitHub (or Git) repositories directly as resource URLs. However Quicklisp is still different. While others let you choose arbitrary versions Quicklisp is distribution based. This can be remotely compared with the package management of Linux distributions. It has pros and cons. The pro side is that it's consistent. A library that has other dependencies are all resolved from the distribution. While in Java many speak of the jar-hell. This comes from the fact that you may end up with different dependent versions in your classpath (the first one found by the class-loader wins) when you specify a direct dependency of a library in your project, but some other direct dependency has one of your direct dependencies also as direct dependencies but a different version of it. This cannot happen in Quicklisp. Well, actually it can. There are two ways: a) <a href="https://github.com/fukamachi/qlot" class="link">Qlot</a>, which allows to lock certain versions for a project, or b) it's possible within Quicklisp to clone single projects into Quicklisps 'local-projects' folder. Projects cloned in there take precedence over what the distributions offers. So this allows to use updated (or downgraded) versions still without getting into the jar-hell.

One other nice thing about Quicklisp is that you can load libraries directly in the REPL and just use them. So once Quicklisp is installed and made available when the REPL starts you can say: `(ql:quickload :cl-gserver)` and it will load the library into the image and it's ready to use. This is a big plus. It makes things extremely simple to just try out some code in the REPL.  


<a id="org36aa9a1"></a>

### Runtimes/compilers (CCL, SBCL, ECL, Clasp, ABCL | LispWorks, Allegro)

Common Lisp is available in quite a few different implementations which all have different features. Historically there were many implementations. Many of them started at universities. Some were and are are open-source implementations, some were commercial implementation but have been open-sourced and some remain commercial. Some are still being maintained, some are not and will only work on older systems.  
The current most popular one I would say is <a href="http://www.sbcl.org/" class="link">SBCL</a>. SBCL is a fork of <a href="https://cmucl.org/" class="link">CMUCL</a>. SBCL is fast and can do statical type checks (see above). I use SBCL myself for production. For development I use <a href="https://ccl.clozure.com/" class="link">CCL</a>. CCL is not as strict as SBCL, developing with is a bit smoother IMO but can also lead to weird effects sometimes. The compiler is said to be faster than SBCL, which I think it true. But the produced code is by far not as fast as SBCLs. CCL comes from a commercial product MCL (Macintosh Common Lisp). In fact I still have a version of MCL on my old PowerMac with MacOS 9 which still runs fine. But CCL is not limited to Apple. It works on Windows and Linux, too.  
<a href="https://common-lisp.net/project/ecl/main.htm" class="link">ECL</a> for Embeddable Common Lisp probably has the largest supported hardware and OS base. There aren't many systems where ECL is not available. Due to the nature of ECL and what it is geared for, namely to be easily embedded in applications, it doesn't work with images (see 'Image based'). It is also quite slow. But it is actively maintained and certainly has it's use-cases.  
<a href="https://github.com/clasp-developers/clasp" class="link">Clasp</a> is relatively new. I believe it uses some of ECL but is otherwise different and uses the LLVM backend with the goal to use any LLVM available libraries easily (like C++ libraries). Clasp, as I followed the project, is useable since a good while. But you have to compile it yourself (which isn't difficult). More work is being done on performance optimizations.  
<a href="https://abcl.org/" class="link">ABCL</a> started out as scripting engine for a Java editor application. Today it has come a long way and is a full featured Common Lisp that runs on the JVM. It even implements JSR-223 (the Java scripting API) and has nice but not as good Java interop as Clojure. It is not super fast, but is very robust due to the battle-proven Java runtime system.  
There are more not so much maintained implementations of Common Lisp, like <a href="https://clisp.sourceforge.io/" class="link">Clisp</a>, or <a href="https://www.gnu.org/software/gcl/" class="link">GCL</a>.  
Then there are the commercial products <a href="https://franz.com/products/allegrocl/" class="link">Allegro CL</a> and <a href="http://www.lispworks.com/index.html" class="link">LispWorks</a>. Both come with sophisticated IDEs and many features but are not cheap. Check them out. There are limited, but free editions available.  


<a id="org833736f"></a>

### Image based

Common Lisp is (usually) an image based system. The only other image based system that I know is Smalltalk. I haven't seen that in younger languages and runtimes. What is it? When you start a Common Lisp system, usually the REPL, then everything you do, like creating variables and functions, etc. is creating or manipulating state in the runtime memory. So far this is not different to other runtimes. What you do during your REPL session is just manipulating data in some memory area. The difference is that Common Lisp allows to create a snapshot (an image) of that runtime memory with all its state and can store it to disk. Then it's possible to run the REPL and load that image and all state is recovered, you could even reconnect to servers and reopen files and so on. The REPL allows to load multiple applications, because all is just variables and functions structured in packages. So you can make ready images to have a head start when starting to work. Usually all Common Lisps that support images actually start with an image when running the REPL. It's just an empty, or default, image.  


<a id="org1b55d4e"></a>

#### image snapshot

To give this a quick run, create a variable like this: `(defparameter *foo* "Hello World")`. Now save the image like this in CCL `(ccl:save-application filename)` (may be different on other implementations).  


<a id="org6ad489a"></a>

#### load from image

To load the image you start CCL with -I, like `ccl -I foo-ccl.image`.  
Then dump your variable `*foo*` and you'll see "Hello World".  


<a id="org5f6afae"></a>

### Functional programming

If you are interested in functional programming with Common Lisp then I'd want to redirect you to my <a href="/blog/Functional+Programming+in+(Common)+Lisp" class="link">blog post</a> on it.  


<a id="org1ce34e8"></a>

### Resources

Much of the information in here is either from my own experience or mentioned and linked web pages but also books like:  

-   <a href="https://gigamonkeys.com/book/" class="link">Practical Common Lisp</a>
-   <a href="https://lispcookbook.github.io/cl-cookbook/" class="link">Common Lisp Cookbook</a>
-   <a href="(http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm" class="link">Common Lisp Hyper Spec</a>
