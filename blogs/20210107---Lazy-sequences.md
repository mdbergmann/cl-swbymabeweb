Lazy sequences are sequences whose elements are generated on demand. Many languages have them built in or available as libraries.

If you don't know what this is then here is an example:

```lisp
(take 5 (range :from 100))
(100 101 102 103 104)
```

`take` takes the first 5 elements from the a generator `range` which starts counting at 100. Each 'take' makes the `range` generator compute a new value rather then computing 5 elements up-front.

That's why it is called 'lazy'. The elements of the sequence are computed when needed. In a very simple form a `range` generator can be implemented using a simple 'let over lambda', like this:

```lisp
(defun range (&key (from 0))
  (let ((n from))
    (lambda () (prog1
              n
            (incf n)))))
```

The `range` function returns a lambda which has bound the `n` variable (this is also called 'closure'). When we now call the the lambda function it will return `n` and increment as a last step. (The `prog1` form returns the first element and continues to evaluate the rest)

So we can formulate a `take` function like this:

```lisp
(defun take (n gen)
  (loop :repeat n
        :collect (funcall gen)))
```

`take` has two arguments, the number of elements to 'take' and the generator, which is our lambda from `range`. This is a very simple example but effectively this is how it works.

If you are looking for good libraries for Common Lisp then I can recommend the following two:

1. <a href=https://github.com/cbeo/gtwiwtg class="link">gtwiwtg</a>: a new kid on the block.
2. <a href=http://series.sourceforge.net/ class="link">Series</a>: a well known and solid library.
