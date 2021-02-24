The last blog <a href="http://retro-style.software-by-mabe.com/blog/Patterns+-+Abstract-Factory" class="link" target="_blank">post</a> was about the Abstract-Factory pattern. We have seen that in  Common Lisp there is hardly a pattern visible.

One could say patterns are code constructs that are repetetive. Almost like a language in a language. Paul Graham once asked: "Are Patterns a language smell?".

### Builder

Today we look at the Builder pattern. Similarly as the Abstract-Factory pattern is the Builder a creator pattern. It can help creating instances of objects. The difference to Abstract-Factory is that the Builder is tightly coupled to the class it creates. Yet, it allows to hide details of the class that only the Builder has access to while being in the same package. There can be different Builders that create instances of the same class but with a different configuration. If we wanted to do this with the classes directly we'd have to open them up. A Builder can also hide complexities when creating objects while providing a more simple interface to the user.

Since we make a comparison with Common Lisp the simple short story is: there is no Builder pattern needed in Common Lisp.

But let's see later why and how.

#### Example in Scala

First, we will look at some Scala code.  
We want to create an object (a dungeon) like this:

```scala
val dungeon = new CastleDungeonBuilder()
  .setDifficulty(VeryDifficult)
  .addMonsters(15)
  .addSpecialItems(5)
  .get()
```

First we create a Builder. It is a special kind of Builder that builds a castle dungeon. We set a difficulty, add monsters and some special items that the dungeon object should place somewhere.

The CastleDungeonBuilder looks like this:

```scala
class CastleDungeonBuilder extends IDungeonBuilder {
  override protected val theDungeon = new Dungeon(CastleDungeonKind)

  def addMonsters(n: Int): IDungeonBuilder = {
    // add nice monsters
    val filteredMonsters = Monsters.filter(m => m.creepyFactor < 5)
    theDungeon.monsters = (0 until n)
      .map(filteredMonsters(new Random().nextInt(filteredMonsters.size)))
    this
  }
}
```

As part of creating the Builder instance it creates a `Dungeon` instance. This `CastleDungeonBuilder` has a speciality, the monsters it adds are nice monsters that have a low 'creepy factor'. There is also a `CellarDungeonBuilder` that adds monsters with a 'creepy factor' >= 5 (on a scale from 0 to 10). The right monsters for a cellar.  
The method `addMonsters` also hides some complexity from the user. It just allows to say how many monsters to add, but to the dungeon instance the Builder sets a collection of pre-configured monsters instances.

The abstract Builder (where `CastleDungeonBuilder` and `CellarDungeonBilder` inherit from) actually only does some generic configuration. It looks like this:

```scala
trait IDungeonBuilder {
  protected val theDungeon: Dungeon

  def setDifficulty(difficulty: Difficulty): IDungeonBuilder = {
    theDungeon.difficulty = difficulty
    this
  }
  def addMonsters(n: Int): IDungeonBuilder = {
    theDungeon.monsters = 
      for(i <- 0 until n) 
      yield Monster(new Random().nextInt(3), new Random().nextInt(10))
    this
  }
  def addSpecialItems(n: Int): IDungeonBuilder = {
    theDungeon.specialItems = 
      for(i <- 0 until n) 
      yield SpecialItem(new Random().nextInt(7))
    this
  }
  def get: Dungeon = theDungeon
}
```

This is the `Dungeon` class itself:

```scala
class Dungeon(private _kind: DungeonKind) {
  private var _difficulty: Difficulty = Difficulty.NotDifficultAtAll
  private var _monsters: List[Monster] = Nil
  private var _specialItems: List[SpecialItem] = Nil

  def difficulty: Difficulty = _difficulty
  private[dungeon]
  def difficulty_=(d: Difficulty): Unit = _difficulty = d

  def monsters: List[Monster] = _monsters.copy
  private[dungeon]
  def monsters_=(list: List[Monster]): Unit = _monsters = list.copy

  def specialItems: List[SpecialItem] = _specialItems.copy
  private[dungeon]
  def specialItems_=(list: List[SpecialItems]): Unit = _specialItems = list.copy
}
```

While it allows to query the properties. It doesn't allow to set them except from within the same package. So the Builder must be defined in the same package as the dungeon class is.

##### The poor-man's Builder

Scala allows named and optional parameters in functions and constructors. A poor-man's Builder pattern in Scala could simply be to use those features on object creation together with auxiliary constructors. Though this doesn't allow the abstraction of a Builder and the encapsulation of the object properties but could be sufficient in some cases.

#### Example in Common Lisp

In Common Lisp we could certainly build a similar structure for Builders with separate classes and so on. But that's not needed. It is possible to allow the same features, the same level of abstraction and encapsulation by using multi-methods.

Let's also start with how we want the object to be created. I'd like to use the 'threading' (`->`) operator known from Clojure. I find it quite nice, but it is just some syntactic sugar around a `let`:

```lisp
(let ((dungeon (-> (make-dungeon :type 'cellar)
                   (set-difficulty 'very-difficult)
                   (add-monsters 15)
                   (add-special-items 5))))
  ;; do something with dungeon
  )
```

This first creates a dungeon object of `'cellar` type, then sets difficulty, adds monsters and special-items. Here are two different things at play. `make-dungeon` is a simple factory function. `set-*` and `add-*` functions are generic functions that we use to form a builder protocol. Each returns the dungeon object so that the 'threading' (or piping) can be done:

```lisp
;; builder protocol
(defgeneric set-difficulty (dungeon difficulty))
(defgeneric add-monsters (dungeon amount))
(defgeneric add-special-items (dungeon amount))
```

Similarly as the Builders we created in Scala those generic function definitions should be in the same package as the dungeon class and the factory function is. If we want to apply a different set of monsters for different dungeon types we have to do two things. First we need to define sub-classes for those dungeon types. And second, we have to provide different implementation of the `add-monsters` builder protocol. Let's have a look and the classes and the factory function:

```lisp
(defclass dungeon ()
  ((difficulty :initform 'not-difficult-at-all)
   (monsters :initform nil :reader monsters)
   (special-items :initform nil :reader special-items)))
(defclass castle-dungeon (dungeon) ())
(defclass cellar-dungeon (dungeon) ())

(defun make-dungeon (&key type)
  (make-instance (ecase type
                   (castle 'castle-dungeon)
                   (cellar 'cellar-dungeon))))
```

The specialization of the `add-monsters` generic function on the class type does the trick:

```lisp
;; specialized for 'castle-dungeon
(defmethod add-monsters ((obj castle-dungeon) amount)
  (with-slots (monsters) obj
    ;; set a bunch of nice looking monsters
    (setf monsters
          (filter-monsters-by-creepy-factor 5 #'< amount *monsters*)))
  obj)

;; specialized for 'cellar-dungeon
(defmethod add-monsters ((obj cellar-dungeon) amount)
  (with-slots (monsters) obj
    ;; set a bunch of creepy monsters
    (setf monsters
          (filter-monsters-by-creepy-factor 5 #'>= amount *monsters*)))
  obj)
```

Common Lisp automatically does a match on the first function parameter for the class type. This is called multi-dispatch or multi-methods.
There is not really a lot more to it. All we did here is use the language features.

#### Summary

The Builder pattern in many object-oriented languages requires separate builder classes around a class they should create. This is used for abstraction and data encapsulation which would not be easily possible without the Builder.

In Common Lisp dedicated Builder classes are not needed. But dedicated classes are required to allow the multi-methods to do their work. Yet, this is hardly a pattern, just use of existing language features.
