Lazy sequences are sequences that are generated on demand. Many languages have them built in or as libraries.

If you don't know what this is then here is an example:

```lisp
(take 5 (range :from 100))
(100 101 102 103 104)
```

`take` takes the first 5 elements from the a generator `range` which starts counting at 100. Each 'take' make the `range` generator compute a new value rather then computing 5 elements up-front.

That's why it is called 'lazy'. The elements of the sequence are computed when needed. In a very simple form a `range` generator can be implemented using a simple 'let over lambda', like this:

```lisp
(defun range (&key (from 0))
  (let ((n from))
    (lambda () (prog1
              n
            (incf n)))))
```

The `range` function returns a lambda which has bound the `n` variable. This is also called 'closure'. When we now call the the lambda function it will return and increment `n` as a last step. (If you don't know, the `prog1` form returns the first element and continues to evaluate the rest)

