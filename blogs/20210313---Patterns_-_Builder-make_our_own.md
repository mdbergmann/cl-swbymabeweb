Add-on to the post about the <a href="http://retro-style.software-by-mabe.com/blog/Patterns+-+Builder" class="link" target="_blank">Builder</a> pattern.  
In this post we'll create our own simple Common Lisp builder DSL using macros.

Macros are a crucial component of Common Lisp, making the language so enormously extendable. The term 'macro' is a bit convoluted. Because many things are called 'macro' but have little do to with Lisp macros. The C macros for example are just a simple textual replacements. Today other languages have macros as well. The difference with Lisp macros is that Lisp macros are just Lisp code while other languages have a different AST (Abstract Syntax Tree) representation of the code. This is much more complicated to deal with. Lisp has no AST.

And yet, it's not all that easy. There is a fundamental difference between normal functions and macros. This difference and the consequence of it can take a while to grasp. The difference is that macros are executed at compile time (or macro-expansion time) and the parameters of macros are not evaluated while functions are executed on runtime and parameters of functions are evaluated before they are applied on the function. I'm still trying to wrap my head around it. I can create simple macros but I'm not an expert.

Let's have a look.

I want to use the builder like this:

```lisp
(build 'person p
  (set-name p "Manfred")
  (set-lastname p "Bergmann")
  (set-age p 27)
  (set-gender p "m"))
```

The return of this is a new instance of `person` with the parameters set on the instance. So this `build` thing has to create an instance of the class `'person` which is represented by the variable `p`, evaluate all those `set-xyz` thingies and at last return the instance `p`.

We can easily come up with a simple macro that does this:

```lisp
(defmacro build (clazz var &body body)
  `(let ((,var (make-instance ,clazz)))
     ,@body
     ,var))
```

The parameters `clazz` is the class to create (here `'person`), `var` is the variable name we want to use for the instance, and `body` are all expressions inside `build` (`set-name`, etc.). What the macro creates is a 'quoted' (quasi-quote) expression. Quoted expressions are not evaluated. Effectively they are just data, a list. When we use the `build` macro then what the compiler does is to replace `build` and everything inside it with the quoted expression. After the compiler expanded the macro it looks like this:

```lisp
(let ((p (make-instance 'person)))
  (set-name p "Manfred")
  (set-lastname p "Bergmann")
  (set-age p 27)
  (set-gender p "m")
  p)
```

When we look again at the macro and compare the two then we see that the compiler actually used the macro arguments and replaced `,clazz`, `,var` and `,@body` with those. So this is what the `,` does in combination with the back-tick called quasi-quote. The `,` tells the compiler that it has to interpolate `'person` in place of `,clazz`, `p` in place of `,var` and the list of body expessions given to `build` macro in place of `,@body`. The `@` sign here means 'splice' and is needed because the body expressions are a list, like: `((expr1) (expr2) (expr3))`, but we don't want the list but just the expressions inside the list. So 'splice' removes the outer list.

Now, this is all good and nice. But it doesn't work. The setters `set-name`, etc. are not known to Lisp. They are no regular functions or macros. Slot access functions are auto-generated on classes. But using them in the builder macro doesn't look nice and is too much typing. What would already work with the macro as is:

```lisp
(build 'person p
  (setf (slot-value p 'name) "Manfred")
  (setf (slot-value p 'lastname) "Bergmann")
  (setf (slot-value p 'age) 27)
  (setf (slot-value p 'gender) "m"))
```

So we'll have to create those setter functions ourselves. A bit More DSL to create.

It would be cool if those setters (and also getters) could be auto-generated whenever we define a new class. So we want to define a class, that automatically generates setter and getters like this:

```lisp
(defbeanclass person () (name lastname age gender))
```

`defbeanclass` doesn't exist. The rest of the syntax is equal to `defclass`. So we'll create a macro that can do this:

```lisp
(defmacro defbeanclass (name
                        direct-superclasses
                        direct-slots
                        &rest options)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots ,@options)
     (generate-beans ,name)
     (find-class ',name)))
```

This macro basically just wraps the default `defclass` macro. `generate-beans` is another macro that generates the setters and getters. We'll look shortly at this. Then finally `find-class` is responsible to return the generated class. (There might be a better way to do this.)

`generate-beans` (you might remember Java) looks like this:

```lisp
(defmacro generate-beans (clazz)
  (cons 'progn
        (loop :for slot-symbol
                :in (mapcar #'slot-definition-name
                            (class-direct-slots 
                              (class-of (make-instance clazz))))
              :collect
              `(defbean ,slot-symbol))))
```

This adds something new. Macros can have code that is evaluated at compile time (or macro expansion time) and code that is generated by the macro. The 'quote' makes the difference. Let's see shortly what this macro generates. The unquoted code in there, in particular the `loop`, is executed at compile time and generates a list of quoted `defbean` expressions, one for each slot (name, age, gender, etc.).

Macro expanded this looks like:

```lisp
(progn (defbean name) (defbean lastname) (defbean age) (defbean gender))
```

(if someone knows a way to remove the `(cons 'progn`, please ping me.)

Cool. So `generate-beans` creates beans for each slot. But `defbean` is yet another macro. It does the real work of creating the setter and getter functions for a slot definition.

```lisp
(defmacro defbean (slot-symbol)
  (let ((slot-name (gensym))
        (getter-name (gensym))
        (setter-name (gensym)))
    (setf slot-name (symbol-name slot-symbol))
    (setf getter-name (intern (concatenate 'string "GET-" slot-name)))
    (setf setter-name (intern (concatenate 'string "SET-" slot-name)))
    `(progn
       (defun ,getter-name (obj)
         (slot-value obj ',slot-symbol))
       (defun ,setter-name (obj value)
         (setf (slot-value obj ',slot-symbol) value)))))
```

This macro has again some code that must execute on macro expansion. We have to define the getter and setter names and 'intern' them to the Lisp environment so that they are known. If we wouldn't do this, but just expand the `defun`s we would get errors at runtime that the functions are not known. The 'interning' makes the connection between the function name (as used in `defun`) and the 'interned' symbol of the function name in the Lisp environment. After all this macro expands to (example for name getter/setter):

```lisp
(progn (defun get-name (obj) (slot-value obj 'name))
       (defun set-name (obj value) (setf (slot-value obj 'name) value)))
```

Looking more closely this generates exactly the `setf` slot access we had above which we wanted to replace.  
So we can now define classes that auto-generate getters and setters the way we want to use them in the builder.

When we fully macro expand `defbeanclass`:

```lisp
(progn
  (defclass person () (name lastname age gender))
  (progn
    (progn
      (defun get-name (obj) (slot-value obj 'name))
      (defun set-name (obj value) (setf (slot-value obj 'name) value)))
    (progn
      (defun get-lastname (obj) (slot-value obj 'lastname))
      (defun set-lastname (obj value) (setf (slot-value obj 'lastname) value)))
    (progn
      (defun get-age (obj) (slot-value obj 'age))
      (defun set-age (obj value) (setf (slot-value obj 'age) value)))
    (progn
      (defun get-gender (obj) (slot-value obj 'gender))
      (defun set-gender (obj value) (setf (slot-value obj 'gender) value))))
  (find-class 'person))
```

We see that what the macro generates is just ordinary Lisp code. And yet on the top we have extended the language with new functionality.

Cheers