---

# Reading Simple

[![Haskell](https://www.haskell.org/static/img/haskell-logo.svg)](https://haskell.org)

---

# Reading Simple Haskell

* Markdown format available [here](https://github.com/soupi/rfc/blob/master/reading_haskell.md).

---

## Haskell

Haskell is a general purpose programming language, and can be used to build:

- Compilers ([PureScript](http://purescript.org), [Elm](https://elm-lang.org), [Agda](https://github.com/agda/agda), [Corrode](https://github.com/jameysharp/corrode/) and many more)
- Build systems ([Shake](http://shakebuild.com/))
- Scripts ([Turtle](http://hackage.haskell.org/package/turtle))
- Web servers and frontend applications ([Yesod](https://www.yesodweb.com/), [Servant](https://haskell-servant.github.io/), [Miso](https://haskell-miso.org/), [Reflex](https://github.com/reflex-frp/reflex-platform) and many more)
- [etc](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md)

Haskell's main compiler is [GHC](https://www.haskell.org/ghc/).

---

## Module Structure

- Compiler extensions - we won't talk about those in this talk
- Comments - `--` for single line comment, `{- -}` for block comments
- Module name
- Exports
- Imports
- **Definitions**

---

## Definitions - Simple Values

- Left-hand side is the name of the value
- `=` is used to declare the expression that is bound to the name on the left side (value definition)

```hs
five = 5
```

---

## Definitions - Functions

- Add argument names after a name
- Call functions without parentheses
- Function call is left associative
- Function call takes precendence over operators

```hs
increment n = n + 1

six = increment five

seven = increment (increment five)

incAndAdd x y = increment x + increment y
```

---

## Definitions - Operators

- You can also define operators

```hs
x +- y = (x + x) - (y + y)
```

---

## Function Calls - Partial Application

- We can supply only some of the arguments to a function
- If we have a function that takes N arguments and we supply K arguments, we'll get a function that takes the remaining (N - K) arguments

```hs
-- takes 3 arguments, so in this case N = 3
sum3 x y z = x + y + z

-- only supplies 2 arguments (K = 2), 0 and 1.
-- so newIncrement is a function that takes (N - K = 1) arguments
newIncrement = sum3 0 1

-- three is the value 3
three = newIncrement 2
```

---

## let/where

- We can name part of the computation using `let` or `where`
- `let [<definition>] in <expression>` is an expression and can be used anywhere
- `where` is special syntax

```hs
sumOf3 x y z =
  let temp = x + y
  in temp + z

-- or:
sumOf3 x y z = temp + z
  where temp = x + y
```

---

## Defining Types

- Concrete types starts with an uppercase letter
- Use `type` to give a new alias to an existing type. They can be used interchangingly.

```hs
type Nickname = String
```

---

## Type Signatures

We can give values a type signature using `::`

```hs
myNickname :: Nickname
myNickname = "suppi"
```

---

## Defining Types - Sum Types

- We can define our own types using the keyword `data`
- Sum types are alternative possible values of a given type
- Similar to enums in other languages
- We use `|` to say "alternatively"
- To calculate how many possible values the new type has, we count and sum all the possible values, therefore "sum type"
- Each option must start with an uppercase letter

```hs
data KnownColor -- the new type's name
  = Red         -- One possible value
  | Blue
  | Green

redColor :: KnownColor
redColor = Red
```

---

## Defining Types - Product Types

- We can also use `data` to define compound data of existing types
- Similar to structs in other languages
- To calculate how many possible values the new type has, we count and multiply the amount of possible values for each type. Therefore "product type"

```hs
data RGB
  = MkRGB Int Int Int
{-
      ^    ^   ^   ^
      |    |   |   |
      |    |   |   +- This is the blue component
      |    |   |
      |    |   +----- This is the green component
      |    |
      |    +--------- This is the red component
      |
      +------------- This is called the value constructor, or "tag"
-}

magenta :: RGB
magenta = MkRGB 255 0 255
```

---

## Defining types - Sum and Product Types

- We can mix sum and product types in one type
- This is often called an algebraic data type, or ADT
- Value constructors (like `Red`, `Blue`, `Green` or `RGB`) create a value of the type
- If they represent a product (like `RGB`), value constructors can be used as regular functions to build values of the type
- This also means they can be partially applied

```hs
data Color
  = Red
  | Blue
  | Green
  | RGB Int Int Int

blue :: Color
blue = Blue

magenta :: Color
magenta = RGB 255 0 255
```

---

## Defining types - Records

- Records allow us to name the fields in a product type
- There is more to records, but we won't talk too much about it here

```hs
data RGB = MkRGB
  { rgbRed   :: Int
  , rgbGreen :: Int
  , rgbBlue  :: Int
  }


red :: RGB
red = MkRGB
  { rgbRed   = 255
  , rgbGreen = 0
  , rgbBlue  = 0
  }
```
---

## The Type of Functions

- We use `->` to denote the type of a function from one type to another type

```hs
increment :: Int -> Int
increment n = n + 1

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

supplyGreenAndBlue :: Int -> Int -> Color
supplyGreenAndBlue = RGB 100
```

---

## The Type of Functions

- `->` is right associative, The function definitions from the previous slide will be parsed like this:

```hs
increment :: Int -> Int
increment n = n + 1

sum3 :: (Int -> (Int -> (Int -> Int)))
sum3 x y z = x + y + z

supplyGreenAndBlue :: (Int -> (Int -> Color))
supplyGreenAndBlue = RGB 100
```

- This is why partial function application works.

---

## Parametric Polymorphism in Type Signatures

- Also known as "generics" in other languages
- Names that starts with an **upper** case letter in types are *concrete types*
- Names that starts with a **lower** case letter in types are *type variables*
- Just as a variable represent some value of a given type, a type variable represents some type
- A type variable represents one type across the type signature (and function definition) in the same way a variable represent a value throughout the scope it's defined in

---

## Parametric Polymorphism in Type Signatures

```hs
-- I only take concrete `Int` values
identityInt :: Int -> Int
identityInt x = x

five :: Int
five = identityInt 5

-- `a` represents any one type
identity :: a -> a
identity x = x

seven :: Int
seven = identity 7

true :: Bool
true = identity True

const :: a -> b -> a
const x y = x
```

---

## Parametric Polymorphism in Type Signatures

```hs
-- will fail because nothing in the type signature suggests that
-- `a` and `b` necessarily represent the same type
identity1 :: a -> b
identity1 x = x

-- will fail because we don't know if `a` is `Int`
identity2 :: a -> Int
identity2 x = x

-- will fail because we don't know if `a` is `Int`
identity3 :: Int -> a
identity3 x = x
```

---

## One More Thing About Functions

- In Haskell functions are first class values
- They can be put in variables, passed and returned from functions, etc
- This is a function that takes two functions and a value, applies the second function to the value and then applies the first function to the result
- AKA function composition

```hs
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

f . g = compose f g
```

---

## One More Thing About Functions

- Remember, `->` in type signatures is right associative
- Doesn't it look like we take two functions and return a third from the type signature?

```hs
compose :: ((b -> c) -> ((a -> b) -> (a -> c)))
compose f g x = f (g x)
```

---

## Definitions - Global type inference

As we saw earlier, Haskell is globally type inferred. We can remove almost all type signatures
and Haskell will choose the most general type signature for us.

---

## Recursive Types and Data Structures

- A recursive data type is a data definition that refers to itself
- This lets us define even more interesting data structures such as linked lists and trees

```hs
data IntList
  = EndOfIntList
  | ValAndNext Int IntList

-- the list [1,2,3]
list123 :: IntList
list123 = ValAndNext 1 (ValAndNext 2 (ValAndNext 3 EndOfList))
```

---

## Recursive Types and Data Structures

- A recursive data type is a data definition that refers to itself
- This lets us define even more interesting data structures such as linked lists and trees

```hs
data IntTree
  = Leaf
  | Node
      IntTree      -- Left subtree
      Int          -- Node value
      IntTree      -- Right subtree

--     2
--    / \
--   1   3
--  /
-- 1
tree1123 :: IntTree
tree1123 =
  Node
    (Node (Node Leaf 1 Leaf) 1 Leaf)
    2
    (Node Leaf 3 Leaf)

```

---

## Defining Types - Type variables

- We can use type variables when defining types
- We can define generic structures
- This way we don't have to restrict our structure to a specific type such as `Int` or `Bool` like in the previous slide

```hs
-- a value of type a or nothing
data Maybe a
  = Just a
  | Nothing

-- a value of type a or a value of type b
data Either a b
  = Left a
  | Right b

-- A linked list of `a`s

-- Note: there's also a built in syntax in Haskell for linked lists

data List a          -- [a]    -- special syntax for a linked list of a generic type `a`
  = Nil              -- []     -- special syntax for the empty list
  | Cons a (List a)  -- x : xs -- special operator for constructing a list
```

---

## Case Expression (Pattern Matching)

- Allows us to write control flows on data types
- Matches from top to bottom

```hs
case <expr> of
  <pattern1> -> <result1>
  <pattern2> -> <result2>
  ...
  <patternN> -> <resultN>
```

---

## Case Expression (Pattern Matching)

- Allows us to write control flows on data types
- Matches from top to bottom

```hs
myIf :: Bool -> a -> a -> a
myIf test trueBranch falseBranch =
  case test of
    True  -> trueBranch
    False -> falseBranch
```

---

## Case Expression (Pattern Matching)

- Allows us to write control flows on data types
- Matches from top to bottom

```hs
factorial :: Int -> Int
factorial num =
  case num of
    0 -> 1
    n -> n * factorial (n - 1)
```

---

## Case Expression (Pattern Matching)

- Allows us to write control flows on data types
- Matches from top to bottom
- The pattern `_` means match anything

```hs
colorName :: Color -> String
colorName color =
  case color of
    Red -> "red"
    Green -> "green"
    Blue -> "blue"
    RGB 255 0 255 -> "magenta"
    RGB _ 255 _ -> "well it has a lot of green in it"
    _ -> "i don't know this color"
```

---

## Do notation

- Do notation is special syntax for writing IO actions in a way that looks imperative
- `<-` is used to bind the result of an IO action to a variable when using do notation
- `let` is used to bind an expression to a name

```hs
main :: IO ()
main = do
  putStrLn "Hello!"
  putStrLn "What is your name?"
  result <- getLine
  putStrLn ("Nice to meet you, " ++ result)
  putStrLn "Here is the result of 1+1: "
  let calculation = factorial 100 -- note that when using do notation we don't need to use `in`
  putStrLn (show calculation)
  putStrLn "Bye!"

```

---

## Example

- [A simple JSON EDSL](https://gist.github.com/soupi/c7c94a45d006bc70f3b896f327ea47a3)
- [Try it in repl.it](https://repl.it/repls/IntrepidVacantGraywolf) to see the result

---

## Want to learn more?

- [Writing Simple Haskell](https://soupi.github.io/rfc/writing_simple_haskell)
- [Install a Haskell compiler and environment](https://haskell.org/downloads)
- [Minimal Haskell IDE based on Emacs](https://github.com/soupi/minimal-haskell-emacs/)
- [Haskell Study Plan](https://github.com/soupi/haskell-study-plan)


---

