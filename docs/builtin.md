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
* pred: Int -> Int
* succ: Int -> Int

## Strings

Prelude
* yesno: Bool -> Str

## Boolean operations

Builtin
* true: -> Bool
* false: -> Bool
* and: Bool Bool -> Bool
* or: Bool Bool -> Bool
* not: Bool -> Bool

## Type checking and conversion

Builtin
* isInt: T -> Bool
* isBool: T -> Bool
* toInt: T -> Int
* toBool: T -> Bool

## Comparisons

Builtin
* cmp: T T -> Int
* isZero: Int -> Bool
* isNeg: Int -> Bool
* isPos: Int -> Bool

Prelude
* isZero: Int -> Bool
* isEven: Int -> Bool
* isOdd: Int -> Bool
* lt: Int Int -> Bool
* le: Int Int -> Bool
* eq: Int Int -> Bool
* ne: Int Int -> Bool
* ge: Int Int -> Bool
* gt: Int Int -> Bool
* min: Int Int -> Int
* max: Int Int -> Int
* abs: Int -> Int

## Conditionals

Builtint
* choose: Bool S T -> S|T
* intchoose: Int R S T -> R|S|T

Prelude
* select: S T Bool -> S|T
* if: Stack Bool f@(S0 -> S1) g@(T0 -> T1) -> Stack@((S0 -> S1)|(T0 -> T1))
    * apply f or g to the rest of the stack
* iftrue: Stack Bool (S0 -> S1) -> Stack@(S0 -> S1)
* iffalse: Stack Bool (S0 -> S1) -> Stack@(S0 -> S1)

## Stack manipulation

Builtin
* clear: Stack -> ()
* id: ->
* pop: T ->
* dup: T -> T T
* dip: S T -> S T S
* dep: R S T -> R S T R
* swap: S T -> T S
* rot: R S T -> S T R
* roll: P R S T -> R S T P

Prelude
* pop2: S T ->
* pop3: R S T ->
* pop4: P R S T ->
* dup2: S T -> S T S T
* dup3: R S T -> R S T R S T
* rotl: R S T -> S T R
* swapOver: R S T -> S R T
* rotr: R S T -> T R S
* mirror: R S T -> T S R

## Combinators

Builtin
* apply: Stack (T0 -> T1) -> Stack@(T0 -> T1)
* over: Stack (T0 -> T1) S -> Stack@(T0 -> T1) S
* quote: S -> (-> S)
* compose: Stack (S0 -> S1) (T0 -> T1) -> Stack@(S0 -> S1)@(T0 -> T1)

Prelude
* cons: S (T0 -> T1) -> 
* quote2: S T -> (-> S T)
* quote3: R S T -> (-> R S T)
* over2: Stack (T0 -> T1) R S  -> Stack@(T0 -> T1) R S

## Loops

Prelude
* times: (T0 -> T1) Int -> ?
* while: cond body
* bi:

## Functions as lists

Builtin
* isNull: Fun -> Bool
* head: Fun -> T
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
