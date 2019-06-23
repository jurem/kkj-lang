# Command reference

Poor man's docs, sorry.

## Integer arithmetics

Builtin
* add: Int Int -> Int
	* Addition: adds two top integers from the stack, and pushes the result back to the stack
* sub: Int Int -> Int
	* Subtraction
* mul: Int Int -> Int
	* Multiplication
* div: Int Int -> Int
	* Division
* mod: Int Int -> Int
	* Modulo
* divMod: Int Int -> Int
	* Division and modulo
* quot: Int Int -> Int
	* Quotient, see Haskell's docs
* rem: Int Int -> Int
	* Reminder
* quotRem: Int Int -> Int
	* Quotient and reminder

Prelude
* neg: Int -> Int
	* Negative of a number
* pred: Int -> Int
	* Predecessor
* succ: Int -> Int
	* Successor

## Strings

Prelude
* yesno: Bool -> Str
	* Takes a Boolean and produces a "yes" or "no" string.

## Boolean operations

Builtin
* true: -> Bool
	* Produces a true Boolean value.
* false: -> Bool
	* Produces a false Boolean value.
* and: Bool Bool -> Bool
	* Logical conjunction
* or: Bool Bool -> Bool
	* Logical disjunction
* not: Bool -> Bool
	* Logical negation

## Type checking and conversion

Builtin
* isInt: T -> Bool
	* Is the top of stack of an Int type?
* isBool: T -> Bool
	* Is the top of stack of a Bool type?
* toInt: T -> Int
	* Convert the top to Int
* toBool: T -> Bool
	* Convert the top to Bool

## Comparisons

Builtin
* cmp: T T -> Int
	* Compares two top values and produce -1,0,1 if <,=,>, respectively.
* isZero: Int -> Bool
	* Is the top zero?
* isNeg: Int -> Bool
	* Is the top negative?
* isPos: Int -> Bool
	* Is the top positive?

Prelude
* isZero: Int -> Bool
	* Is the top zero? (duplicate of builtin)
* isEven: Int -> Bool
	* Is the top even integer?
* isOdd: Int -> Bool
	* Is the top odd integer?
* lt: Int Int -> Bool
	* Compares (<) top two numbers
* le: Int Int -> Bool
	* Compares (<=) top two numbers
* eq: Int Int -> Bool
	* Compares (==) top two numbers
* ne: Int Int -> Bool
	* Compares (!=) top two numbers
* ge: Int Int -> Bool
	* Compares (>=) top two numbers
* gt: Int Int -> Bool
	* Compares (>) top two numbers
* min: Int Int -> Int
	* Minimum of the top two numbers
* max: Int Int -> Int
	* Maximum of the top two numbers
* abs: Int -> Int
	* Absolute of the top number

## Conditionals

Builtint
* choose: Bool S T -> S|T
	* Based on Bool choose either the sub-top or the top value
* intchoose: Int R S T -> R|S|T
	* Based on Int (-1,0,1) choose either the sub-sub-top, the sub-top, or the top value

Prelude
* select: S T Bool -> S|T
	* Similar to choose but with different order
* if: Stack Bool f@(S0 -> S1) g@(T0 -> T1) -> Stack@((S0 -> S1)|(T0 -> T1))
    * Based on Bool applies f or g to the rest of the stack
* iftrue: Stack Bool f@(S0 -> S1) -> Stack@(S0 -> S1)
	* Based on Bool applies f
* iffalse: Stack Bool f@(S0 -> S1) -> Stack@(S0 -> S1)
	* Based on Bool applies f

## Stack manipulation

Builtin
* clear: Stack -> ()
	* Clears the stack
* id: ->
	* Does nothing
* pop: T ->
	* Pops the top element
* dup: T -> T T
	* Duplicates the top element
* dip: S T -> S T S
	* Duplicates the sub-top element
* dep: R S T -> R S T R
	* Duplicates the sub-sub-top element
* swap: S T -> T S
	* Swaps the top two elements
* rot: R S T -> S T R
	* Rotate (right) the top three elements
* roll: P R S T -> R S T P
	* Roll = rotate (right) the top four elements

Prelude
* pop2: S T ->
	* Pops the top two elements
* pop3: R S T ->
	* Pops the top three elements
* pop4: P R S T ->
	* Pops the top four elements
* dup2: S T -> S T S T
	* Duplicates the top two elements
* dup3: R S T -> R S T R S T
	* Duplicates the top three elements
* rotl: R S T -> S T R
	* Rotate left the top three elements
* swapOver: R S T -> S R T
	* Swaps the sub-sub-top and sub-top elements
* rotr: R S T -> T R S
	* Rotate right the top three elements
* mirror: R S T -> T S R
	* Mirror the top three elements (middle is fixed)

## Combinators

Builtin
* apply: Stack (T0 -> T1) -> Stack@(T0 -> T1)
	* Applies the top function to the rest of the stack
* over: Stack (T0 -> T1) S -> Stack@(T0 -> T1) S
	* Applies the sub-top function to the rest of the stack (while preserving the top element)
* quote: S -> (-> S)
	* Quote the top element (creates a function producing the top element)
* compose: (S0 -> S1) (T0 -> T1) -> (S0 -> S1)@(T0 -> T1)
	* Compose the top two function into a new function

Prelude
* cons: S (T0 -> T1) -> ?
	* Prepends the sub-top element to the list (the top)
* quote2: S T -> (-> S T)
	* Quote the top two elements
* quote3: R S T -> (-> R S T)
	* Quote the top three elements
* over2: Stack (T0 -> T1) R S  -> Stack@(T0 -> T1) R S
	* Applies the sub-sub-top function while preserving the top two elements

## Loops

Prelude
* times: Fun Int -> ?
	* Applies Fun Int times.
* while: cond@Fun body@Fun -> ?
	* Applies body while cond is true
* bi:
	* Check the source :(

## Functions as lists

Builtin
* isNull: Fun -> Bool
	* Does the function represent the empty list?
* head: Fun -> T
	* The irst element
* tail: Fun -> T
* first: Fun -> T
* last: Fun -> T
* front: Fun -> T
* back: Fun -> T
* eval: 

## Printing and reading input

Builtin
* print: T ->
* println: T ->
* ln: ->
* printstack: ->
* .: alias to printstack
* readInt: -> Int
* readBool: -> Bool

## Other
* exit: ->
