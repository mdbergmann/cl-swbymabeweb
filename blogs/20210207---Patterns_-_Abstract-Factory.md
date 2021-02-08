Peter Norvig (one of the main people behind Common Lisp at the time) claimed that many design patterns are either not needed or much simpler in Lisp or dynamic languages generally. See <a href="http://norvig.com/design-patterns/design-patterns.pdf" class="link" target="_blank">this PDF</a>.

In this series of blog posts I'd like to go through some of the well known design patterns and make a comparison between the implementation in Scala and Common Lisp.  
<a href="https://scala-lang.org/" class="link" target="_blank">Scala</a> is a statically typed, multi-paradigm language running on the Java Virtual Machine.  
<a href="https://common-lisp.net/" class="link" target="_blank">Common Lisp</a> is a dynamically typed, multi-paradigm language running natively on many platforms.


### Abstract Factory

Abstract Factory is a common creation pattern where details of object creation are abstracted and hidden behind a creation 'facade'. A factory generally allows to hide the details of object creation. For example when creating the object is complex the user of a factory is and should not be aware of those details. The factory hides those details.  
Another important feature is that a factory can hide the concrete class implementation of the object it creates. The created object just has to comply to an interface/protocol. This has the benefit of less coupling. It is also possible to separate the source code dependenies so that a module that uses a factory does not need to have a source code dependency on the class implementation but only on the interface/protocol.

An Abstract Factory goes a step further in that it handles a set of factories, or put differently, it is an abstraction of a set of factories. For example, you have a GUI framework, this framework allows to create buttons. It should work in the same way no matter which toolkit is the backend. The Abstract Factory is usually configured at startup of the application with the right concrete factory implementation. This also allows to configure a mock or fake factory in a test environment.  
An Abstract Factory is in a way Open-Closed. New button types and button factories can be added without affecting the existing buttons and factories.

In a static language like Scala usually two parallel class hierarchies are needed, one for the GUI button implementation and one for the factory that creates the button.

#### Example in Scala

```scala
trait IButton
class AbstractButton extends IButton

class GtkButton extends AbstractButton
class QtButton extends AbstractButton
```

```scala
trait IButtonFactory {
  def makeButton(): IButton
}

class GtkButtonFactory extends IButtonFactory
class QtButtonFactory extends IButtonFactory

object ButtonFactory extends IButtonFactory {
  var factoryInstance: IButtonFactory
  
  def makeButton(): IButton = {
    factoryInstance.makeButton()
  }
}
```

A user will now only use `ButtonFactory.makeButton()` to create buttons. It implements the same protocol as the concrete factories but it doesn't create a button itself, rather it delegates the creation to a concrete factory that has been configured.

#### Example in Common Lisp

In Common Lisp something similar could be easily created using CLOS (Common Lisp Object System). But there is a more simple way. Is it not necessary to maintain two parallel hierarchies. Just the buttons are needed.

In Common Lisp classes are designated by a symbol. For instance a class "foo" is designated by the symbol 'foo.

```lisp
(defclass foo () ())

(make-instance 'foo)
```

But the class definition does not need to be known when creating an instance. `find-class` can find the class on run-time (assuming the class exists in the environment).

```lisp
(make-instance (find-class 'foo))
#<FOO #x3020014BB27D>
```

So, the factory, which creates the button instance also does not need a source dependency on the concrete implementation of the button class. This gives us the separation, and we can define a default button class at run-time somewhere on startup which could be configured from a configuration file.

Then it is fully sufficient to create a simple button factory function which creates an instance of the button:

```lisp
(package :my-button-factory)

;; could be `(find-class 'qt-button)`, configured by startup code.
(defparameter *button-class* nil)

(defun make-button ()
  (make-instance *button-class*))
```

We also need the buttons: 

```lisp
(defclass abstract-button () ())
(defclass qt-button (abstract-button) ())
(defclass gtk-button (abstract-button) ())
```

In a test we can easily set a mock or fake class for `*button-class*`.

New button implementations can easily be added without affecting existing buttons or the factory.

#### Summary

The parallel factory hierarchy is not necessary in Common Lisp. Neither is there really a pattern here that would be worth describing. It is so simple.

To be fair, to some degree a similar approach is also possible for Scala/Java using reflection where it is possible to create new instances of classes from the class object. For example an instance of a class can be created with:

```
Foo.class.getDeclaredConstructors()[0].newInstance()
```

But the handling of this is quite combersome and by far not as convenient as with Common Lisp. In particular if there are different constructors. Also this approach leaves the type safe area that Scala provides. What `newInstance()` creates is just an `Object` which requires a manual cast.