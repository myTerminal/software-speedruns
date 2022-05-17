# [Common Lisp](https://lisp-lang.org)

> Common Lisp is the modern, multi-paradigm, high-performance, compiled, ANSI-standardized, most prominent (along with Scheme) descendant of the long-running family of Lisp programming languages. - [common-lisp.net](https://common-lisp.net)

**PS:** This document only contains concepts of Common Lisp that I'm aware of, is in no way complete, and most probably not accurate.

**PPS:** Lisp isn't like the other programming languages, but I still made an attempt to create this "cheat sheet" which might not appear to be the most logical to read. Your contributions are always welcome.

## Comments

    ;; This is a comment on its own line
    (+ 1 2) ; This is an inline comment

## Types

1. 453 ; Number (Integer)
2. 3.14 ; Number (Floating point)
3. "Shepard" ; String
4. 'Something ; Symbol

## Commands vs data

Everything is a list, hence,

    (+ 2 3)

is also a list, but by default, everything (every list) is treated as a command and is considered to be in a "form" that starts with a function name as the first element, followed by one (or more, or no) arguments to the function.

    (+ 2 3) ; Gets evaluated as 5

Anything can be turned into data by "quoting" it by prepending the expression with a `'`.

    '(+ 2 3) ; Remains a list of three elements, and is not executed/evaluated

Alternatively, the `list` function also creates a list from a set of values.

    (list '+ 2 3); Notice how we had to "quote" the '+' operator/function

## Lists and cons

The below could be a list:

    '(Shepard Joker Vakarian)

While the below is a pair or a "cons":

    '(Shepard . Joker)

and it could also be written as:

    (cons 'Shepard 'Joker)

A `cons` can also be chained, turning the second element into a `cons` itself:

    (cons 'Shepard (cons 'Joker 'Vakarian))

but when the "tail" of the `cons` is a "nil",

    (cons 'Shepard  (cons 'Joker (cons 'Vakarian . nil)))

it turns into a list in Lisp

    '(Shepard Joker Vakarian)

It also means that a list in Lisp is pretty much something that we know as a linked-list.

## Printing values

    (princ )

## Variables

Defining global variables

    (defparameter *count* 2) ; Shadows an existing variable named `*count*`
    (defvar *count* 2) ; Does not shadow an existing variable named `*count*`

Defining local variable(s)

    (let ((first-name "John")
          (last-name "Shepard"))
        ...
        )

## Functions

Defining a global function

    (defun add (a b)
        (+ a b))

Defining local function(s)

    (flet ((add (a b)
            (+ a b))
        ...
        )

Functions that need to refer to each other can be defined using `labels`

    (labels ((add (a b)
                (+ a b))
             (avg (a b)
                (/ (add a b) 2)))
        ...
        )

## Operations on values

### Arithmetic operations

    (+ 2 3) ; Returns 5 as the sum of 2 and 3
    (- 3 2) ; Returns 1 as the difference between 3 and 2
    (* 2 3) ; Returns 6 as the product of 2 and 3
    (/ 6 3) ; Returns 2 as the quotient
    (/ 5 3) ; Returns 5/3 as the quotient
    (/ 5.0 3) ; Returns 0.6 as the quotient
    (1+ 3) ; Returns 4 as the increment to 3
    (1- 3) ; Returns 2 as the decrement to 3
    (expt 2 3) ; Returns 8 (2^3)

### Binary operations

    (ash 11 1) ; Turns 11 into 22 by shifting bits once to the left
    (ash 11 -1) ; Turns 11 into 5 by shifting bits once to the right

### Operations on lists

To access

    (car '(Shepard Joker Vakarian)) ; Gives 'Shepard
    (cdr '(Shepard Joker Vakarian)) ; Gives '(Joker Vakarian)
    (car (cdr '(Shepard Joker Vakarian))) ; Gives 'Joker
    (cadr '(Shepard Joker Vakarian)) ; Gives 'Joker
    (cadr '(1 2 3 4 5)) ; Gives 2
    (cddr '(1 2 3 4 5)) ; Gives '(3 4 5)
    (caddr '(1 2 3 4 5)) ; Gives 3
    (cdddr '(1 2 3 4 5)) ; Gives '(4 5)
    (cadddr '(1 2 3 4 5)) ; Gives 4
    (cddddr '(1 2 3 4 5)) ; Gives '(5)

There are definitely many more `c*r`s available out-of-the-box, and more can be implemented.
