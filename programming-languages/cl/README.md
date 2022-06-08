# [Common Lisp](https://lisp-lang.org)

> Common Lisp is the modern, multi-paradigm, high-performance, compiled, ANSI-standardized, most prominent (along with Scheme) descendant of the long-running family of Lisp programming languages. - [common-lisp.net](https://common-lisp.net)

**PS:** This document only contains concepts of Common Lisp that I'm aware of, is in no way complete, and most probably not accurate.

**PPS:** Lisp isn't like the other programming languages, but I still made an attempt to create this "cheat sheet" which might not appear to be the most logical to read. Your contributions are always welcome.

## Comments

    ;; This is a comment on its own line
    (+ 1 2) ; This is an inline comment

## Types

1. `'Something` => Case-insensitive symbol, turns into "SOMETHING"
2. `'|Something|` => Case-sensitive symbol, stays "|Something|"
2. `453` => Number (Integer)
3. `3.14` => Number (Floating point)
4. `"Shepard"` => String
5. `#\a` => Character

## Code vs data

Everything is a list, hence,

    (+ 2 3)

is also a list, but by default, everything (every list) is treated as a command and is considered to be in a "form" that starts with a function name as the first element, followed by one (or more, or no) arguments to the function.

    (+ 2 3) ; Gets evaluated as 5

Anything can be turned into data by "quoting" it by prepending the expression with a `'`.

    '(+ 2 3) ; Remains a list of three elements, and is not executed/evaluated

Alternatively, the `list` function also creates a list from a set of values.

    (list '+ 2 3) ; Notice how we had to "quote" the '+' operator/function

Quasiquoting can be used to embed code within data.

    '(1 (+ 2 3)) ; Returns the data as-is
    `(1 ,(+ 2 3)) ; Returns '(1 5)

Executing data as code:

    (eval '(+ 2 3)) ; Returns '5'

## Lists and cons's

The below could be a list:

    '(Shepard Joker Vakarian)

While the below is a pair or a "cons":

    '(Shepard . Joker)

and it could also be written as:

    (cons 'Shepard 'Joker)

A `cons` can also be chained, turning the second element into a `cons` itself:

    (cons 'Shepard (cons 'Joker 'Vakarian))

but when the "tail" of the `cons` is a "nil",

    (cons 'Shepard  (cons 'Joker (cons 'Vakarian nil)))

it turns into a list in Lisp

    '(Shepard Joker Vakarian)

It also means that a list in Lisp is pretty much something that we know as a linked-list.

## Variables

Defining global variables

    (defparameter *count* 2) ; Shadows an existing variable named `*count*`
    (defvar *count* 2) ; Does not shadow an existing variable named `*count*

Change the value of a variable

    (setf *count* 3) ; Sets the new value to the variable *count*

Defining local variable(s)

    (let ((first-name "John")
          (last-name "Shepard"))
        ...
        )

Defining a set of local variables dependent on one another

    (let* ((a 1)
           (b (1+ a)))
       ...
       )

## Printing values

Simply printing out the value such that they could be read back later

    (print "Shepard") ; Prints "Shepard" with a carriage return

Printing out the value but without a carriage return

    (prin1 "Shepard") ; Prints "Shepard" without a carriage return

Printing out a value but in a literal sense

    (princ "Shepard") ; Prints 'Shepard' without double-quotes
    (princ #\newline) ; Prints a new line character

## Reading values

The below snippet reads a value from the user, stores it in a variable named "name" and then prints it out back.

    (let ((name (read)))
        (print name))

To read an input in the way `princ` treats output:

    (read-line) ; Reads a string from the user without it having any special meaning

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

Referring to functions by names to pass them around to other functions

    (function add) ; Returns a reference to the function 'add'
    #'add ; Returns a reference to the function 'add'

Creating an unnamed function (lambda):

    (lambda (x) (* x 2))

## Operations

### Arithmetic

    (+ 2 3) ; Returns 5 as the sum of 2 and 3
    (- 3 2) ; Returns 1 as the difference between 3 and 2
    (* 2 3) ; Returns 6 as the product of 2 and 3
    (/ 6 3) ; Returns 2 as the quotient
    (/ 5 3) ; Returns 5/3 as the quotient
    (/ 5.0 3) ; Returns 0.6 as the quotient
    (1+ 3) ; Returns 4 as the increment to 3
    (1- 3) ; Returns 2 as the decrement to 3
    (expt 2 3) ; Returns 8 (2^3)

### Comparison

`eq` for symbols

    (eq 'Shepard 'Vakarian) ; Returns nil
    (eq 'Shepard 'Shepard) ; Returns t
    (eq (cons 1 2) (cons 1 2)) ; Returns nil, as the two are separate conses

`eql` for symbols, numbers and characters

    (eql 'Shepard 'Shepard) ; returns t
    (eql 2 2) ; returns t
    (eql #\a #\a) ; returns t

`equal` for isomorphic values (the ones that "look the same")

    (equal 'Shepard 'Shepard) ; Returns t
    (equal 7 7) ; Returns t
    (equal '(1 2 3) '(1 2 3)) ; Returns t
    (equal "Shepard" "Shepard") ; Returns t
    (equal '(1 2 3) (cons 1 (cons 2 cons (3 ())))) ; Returns t

`equalp` for a "fuzzy" comparison

    (equalp "Shepard" "shepard") ; Returns t
    (equalp 0 0.0) ; Returns t

`=` for numbers

    (= 1 2) ; Returns "nil"
    (= 1 1) ; Returns "t"

`string-equal` for strings

    (string-equal "Shepard" "Shepard") ; Returns t
    (string-equal "Shepard" "shepard") ; Returns t

`char-equal` for characters

    (char-equal #\a #\a) ; Returns t
    (char-equal #\a #\A) ; Returns t

### String

Replace text

    (substitute-if #\x #'alphanumericp "cat") ; Replaces all alphanumeric characters in the string "cat" with an "x"

Split strings

    (subseq "Commander" 3 6) ; Gets "man"

### Logical

Regular use

    (or (= 2 3) (= 4 5)) ; Logical OR
    (and (= 2 2) (= 3 4)) ; Logical AND
    (not nil) ; Logical NOT

Shortcut boolean evaluation

    (or (= 2 3) (princ "Nice!"))
    (and (= 2 3) (princ "Great!"))

### Binary

    (ash 11 1) ; Turns 11 into 22 by shifting bits once to the left
    (ash 11 -1) ; Turns 11 into 5 by shifting bits once to the right

### List

To access a part of a list

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

To push an element to the start of a list:

    (defvar *squad* '(Garrus Liara))
    (push 'Shepard *squad*)
    *squad* ; Returns '(Shepard Garrus Liara)

To check whether a list contains a particular value

    (member 3 '(1 2 3 4)) ; Returns the rest of the list from the point of a match

Remove duplicate elements

    (remove-duplicates '(1 2 2 3)) ; Returns a list of unique items
    (remove-duplicates '(1 2 3 3) :test #'equal) ; Determines duplicates based on the test function

To retrieve the first occurrence of a pattern in a list

    (find-if #'evenp '(1 2 3 4)) ; Returns "2" as the first even number in the list

Fetching items from association lists (alists):

    (defvar *squad* '((Shepard Human)
                      (Garrus Turian)
                      (Liara Asari)))
    (assoc 'Garrus *squad*) ; Returns "(Garrus Turian)"

Finding items from lists:

    (find 1 '((a 1) (b 2)) :key #'cadr) ; Returns '(a 1)

Check if at least one element matches the predicate:

    (some #'oddp '(1 2 3)) ; Returns t

Concatenating lists:

    (append '(1 2 3) '(4 5 6)) ; Returns '(1 2 3 4 5 6)

Find difference between two lists:

    (set-difference '(1 2 3) '(2 3)) ; Returns '(1)

Find common elements between two (or more) lists:

    (intersection '(1 2 3) '(2 3 4)) ; Returns '(2 3)

Iterating through lists:

    (mapc #'print '(1 2 3)) ; Prints each of the elements on a new line

Passing elements in a list to a function as individual arguments:

    (apply #'append '((Shepard) (Vakarian)))

Transforming lists:

    (mapcar #'1+ '(1 2 3)) ; Returns '(2 3 4)

Filtering lists:

    (remove-if #'oddp '(1 2 3 4 5)) ; Removes numbers matching the predicate
    (remove-if-not #'oddp '(1 2 3 4 5)) ; Removes numbers NOT matching the predicate

### Misc

    (oddp 1) ; Returns t as "1" is an odd value
    (evenp 3) ; Returns nil as "3" is an odd value
    (random 5) ; Gets a random number between 0 and 4, which is one less than 5
    (zerop 1) ; Gets a nil as 1 isn't a zero
    (concatenate 'string "Commander" "Shepard") ; Gives "Commander Shepard"
    (concatenate 'list '(1 2) '(3 4)) ; Gives '(1 2 3 4)
    (compliment #'oddp) ; Gets you a function equivalent to #'evenp

## False values

1. `nil` - a nil value or an absence of a value
2. `'nil` - a symbol named "nil" that points to `nil`
3. `'()` - an empty list
4. `()` - an empty form

## Code blocks

Combines all contained statements into a single form, and returns the last value

    (progn
        ...
        )

## Conditionals

The classic `if`:

    (if <condition>
        'yes
        'no)

The smart `when` and `unless`:

    (when <condition>
        ...
        )

    (unless <condition>
        ...
        )

The heroic `cond`:

    (cond (<condition1> (
                       ...
                       ))
          (<condition2> (
                       ...
                       ))
          ...
          (t 'nothing))

The convenient `case`:

    (defparameter squad-mate 'Garrus)
    (case squad-mate
        ((Miranda Jacob Jack Zaeed Kasumi) (princ "Human"))
        ((Garrus) (princ "Turian"))
        ((Tali) (princ "Quarian"))
        ((Grunt) (princ "Krogan"))
        ((Mordin) (princ "Salarian"))
        ((Legion) (princ "Geth"))
        (otherwise (princ "Unknown")))

## Looping

Indefinite looping

    (loop (princ "Nice")) ; Keeps looping forever

Looping for a fixed number of times

    (loop repeat 5
        do (print "Hello!")) ; Prints "Hello!" exactly 5 times

Looping with index

    (loop for i from 6 to 10
        do (print i)) ; Prints numbers between 6 and 9, one less than 10

Looping and collecting values

    (loop repeat 10
        collect (random 100)) ; Generates 10 random numbers between 0 and 99

## File I/O

### Writing to a file

Writing to a text file using a custom stream

    (with-open-file (my-stream
                     "~/_store/temp"
                     :direction :output
                     :if-exists :supersede)
        (princ "Hello!" my-stream))

Writing to a text file using `*standard-output*`  stream

    (with-open-file (*standard-output*
                     "~/_store/temp"
                     :direction :output
                     :if-exists :supersede)
        (princ "Hello!"))
