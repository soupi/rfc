---

# Writing Simple

[![Haskell](https://www.haskell.org/static/img/haskell-logo.svg)](https://haskell-lang.org)

---

## Writing Simple Haskell

* This is a follow-up to [Reading Simple Haskell](https://soupi.github.io/rfc/reading_simple_haskell/)
* Markdown format available [here](https://github.com/soupi/rfc/blob/master/writing_simple_haskell.md).

---

### What we're going to do

* We're going to build a cli todo app.
* To get the most out of this, follow along using [ghc](https://haskell-lang.org/get-started) or [repl.it](https://repl.it/languages/haskell).

---

### TODO - Requirements

We want to be able to:

- Add a new todo item
- Diaplay all items
- Mark an item as done

But first, let's greet the user.

---

### Hello

```hs
-- Hello.hs
module Main where

main :: IO ()
main = putStrLn "Hello user!"
```

```sh
$ runghc Hello.hs
Hello user!
```

---

### Hello

```hs
module Main where

main :: IO ()
main = putStrLn "Hello user!"
```

- `::` means "type of"
- `=` means equality (the two sides are interchangeable).
- The type of `putStrLn` is `String -> IO ()`
- The type of main is `IO ()`.

---

### Hello

```hs
module Main where

main :: IO ()
main = putStrLn "Hello user!"
```

- The type `IO a` means _This is a description of a subroutine which when run, may perform IO actions and in the end will return a value of type a_
- `main` is the name of the entry point of the program.
- The Haskell runtime will look for `main` and will run it.

---

### What is your name?

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let out = "Nice to meet you, " ++ name ++ "!"
  putStrLn out
```

```sh
$ runghc Hello.hs
"Hello! What is your name?"
suppi
"Nice to meet you, suppi!
```

---

### What is your name?

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let out = "Nice to meet you, " ++ name ++ "!"
  putStrLn out
```

* Haskell is [indentation sensitive](https://en.wikibooks.org/wiki/Haskell/Indentation)
* _Code which is part of some expression should be indented further in than the beginning of that expression_

---

### What is your name?

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let out = "Nice to meet you, " ++ name ++ "!"
  putStrLn out
```

* `do` is a special syntax that lets us sequence IO actions
* the type of `getLine` is `IO String`
* `IO String` means _This is a description of a subroutine which when run, may perform IO operations and in the end will return a value of type `String`_
* `getLine` produces a `String` by taking a line from the standard input

---

### What is your name?

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  let out = "Nice to meet you, " ++ name ++ "!"
  putStrLn out
```

* The type of `getLine` is `IO String`
* The type of `name` is `String`
* `<-` is special syntax that can only appear in `do` notation.
* `<-` means _run the subroutine and bind the value it produces to the name on the left side of `<-`_
* `let <name> = <expr>` means that the `<name>` is interchangeable with `<expr>` for the rest of the `do` block
* In `do` notation, `let` does not need the accompanying `in`

---

### Common Error #1

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  let out = "Nice to meet you, " ++ getLine ++ "!"
  putStrLn out
```

```hs
Hello.hs:6:37: error:
    • Couldn't match expected type ‘[Char]’
                  with actual type ‘IO String’
    • In the first argument of ‘(++)’, namely ‘getLine’
      In the second argument of ‘(++)’, namely ‘getLine ++ "!"’
      In the expression: "Nice to meet you, " ++ getLine ++ "!"
  |
6 |   let out = "Nice to meet you, " ++ getLine ++ "!"
  |                                     ^^^^^^^
```

---

### Common Error #1 - Using `IO String` in place of `String`

* Note: `String` is defined as `type String = [Char]`
* Haskell says it can't match the types `String` which was expected, with `IO String` which is the type of `getLine`
* `IO a` and `a` are different types

---


### Common Error #2

```hs
module Main where

main :: IO ()
main = do
  putStrLn "Hello! What is your name?"
  name <- getLine
  putStrLn "Nice to meet you, " ++ name ++ "!"
```

### Common Error #2

```hs
Hello.hs:7:3: error:
    • Couldn't match expected type ‘[Char]’ with actual type ‘IO ()’
    • In the first argument of ‘(++)’, namely
        ‘putStrLn "Nice to meet you, "’
      In a stmt of a 'do' block:
        putStrLn "Nice to meet you, " ++ name ++ "!"
      In the expression:
        do putStrLn "Hello! What is your name?"
           name <- getLine
           putStrLn "Nice to meet you, " ++ name ++ "!"
  |
7 |   putStrLn "Nice to meet you, " ++ name ++ "!"
  |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

---

### Common Error #2 - Function application precedes operator application

* Parenthesis are needed around the expression string

```hs
putStrLn ("Nice to meet you, " ++ name ++ "!")
```

---

## TODO

We want to be able to:

- Add a new todo item
- Diaplay all items
- Mark an item as done

How should we

* Model the data?
* Store the items?

---

### TODO

* We can store the items in a linked list.

```hs
type Item = String
type Items = [Item]
```

---

### TODO

* We can save the items in a linked list.

```hs
type Item = String
type Items = [Item]
```

* How can we refer to an item?

---

### TODO

* We can save the items in a linked list.

```hs
type Item = String
type Items = [Item]
```

* How can we refer to an item?

By it's index in the list

---

### TODO

* We can save the items in a linked list.

```hs
type Item = String
type Items = [Item]
```

* How can we refer to an item?

By it's index in the list

* Will will the operations we want to do look like?

---

### TODO - Items as a linked list

- Add a new todo item -> Add an item to the start of the list
- Diaplay all items -> reverse and print with indices
- Mark an item as done -> Remove an item by it's reverse-index

* How can we represent these actions?

---

### TODO - Actions as functions

```hs
-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items

-- Returns a string representation of the items
displayItems :: Items -> String

-- Returns a new list of items or an error message if the index is out of bounds
removeItem :: Int -> Items -> Either String Items
```

* We use `Either` to mark a possible failure

---

### TODO - addItem

```hs
-- Returns a new list of Items with the new item in it
addItem :: Item -> Items -> Items
addItem item items = item : items
```

---

### TODO - displayItems

```hs
-- Returns a string representation of the items
displayItems :: Items -> String
displayItems items =
  let
    displayItem index item = show index ++ " - " ++ item
    reversedList = reverse items
    displayedItemsList = zipWith displayItem [1..] reversedList
  in
    unlines displayedItemsList
```

* Use [hoogle](https://hoogle.haskell.org) to search for `zipWith`, `reverse`, and `unlines` to find more about them
* Haskell only evaluates values when it needs to (for example, when they need to be evaluated in order to print something to the user).
* It let's us write functions that work on infinite lists such as `[1..]` and only evaluate what it needs to evaluate.
* You can read more about evaluation in Haskell in [this guide](http://blog.ezyang.com/2011/04/the-haskell-heap/)

---

### TODO - User Interaction

* Let's skip `removeItem` for now and add user interaction

```hs
-- Takes a list of items
-- Interact with the user
-- Return an updated list of items
interactWithUser :: Items -> IO Items
```

---

### TODO - User Interaction

* Let's start by reading a line, treat it as an item, add it to the list, and display the new items

```hs
interactWithUser :: Items -> IO Items
interactWithUser items = do
  putStrLn "Enter an item to add to your todo list:"
  item <- getLine
  let newItems = addItem item items
  putStrLn "Item added.\n"
  putStrLn "The List of items is:"
  putStrLn (displayItems newItems)
  pure newItems
```

* The last line of the `do` notation is the result of the computation
* In this case, it needs to be a value of the type `IO Items`
* But `newItems` has the type `Items`
* So we use `pure` which has the type `a -> IO a`
* `pure` creates a subroutine that produces an `a` without doing any IO

---

### Back to main

```hs
main :: IO ()
main = do
  putStrLn "TODO app"
  let initialList = []
  interactWithUser initialList
  putStrLn "Thanks for using this app."
```

We can now try and run this program.

---

### Execution

```sh
$ runghc Todo.hs
TODO app
Enter an item to add to your todo list:
Make a better app
Item added.

The List of items is:
1 - Make a better app

Thanks for using this app.
```

---

### Iteration and State

* We want to let the user add more than one item to their todo list
* We want to remember the changes to the list

To do this in Haskell, we use recursion.

---

### Iteration and State

* Instead of returning the todo list, we feed it back to `interactWithUser`

```hs
interactWithUser :: Items -> IO ()
interactWithUser items = do
  putStrLn "Enter an item to add to your todo list:"
  item <- getLine
  let newItems = addItem item items
  putStrLn "Item added.\n"
  putStrLn "The List of items is:"
  putStrLn (displayItems newItems)
  interactWithUser newItems
```

---

### Iteration and State

```hs
TODO app
Enter an item to add to your todo list:
Make
Item added.

The List of items is:
1 - Make

Enter an item to add to your todo list:
This
Item added.

The List of items is:
1 - Make
2 - This

Enter an item to add to your todo list:
Stop
Item added.

The List of items is:
1 - Make
2 - This
3 - Stop

Enter an item to add to your todo list:
^C
```

---

### Iteration and State

* This program will run forever
* We can stop it using Ctrl-C, but that's not very nice
* Let's make it possible for the user to use different commands

---

### User Commands - Representation

```hs
data Command
  = Quit
  | DisplayItems
  | AddItem String
```

* We create a new ADT to model the possible user commands

---

### User Commands - Parsing

* And parse a user command to our data type
* This may fail

```hs
parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["items"] -> Right DisplayItems
  "add" : "-" : item -> Right (AddItem (unwords item))
  _ -> Left "Unknown command."
```

---

### User Commands - Change iteraction

We change interactWithUser to accomodate for our new functionality

```hs
interactWithUser :: Items -> IO ()
interactWithUser items = do
  putStrLn "Commands: quit, items, add - <item to add>"
  line <- getLine
  case parseCommand line of
    Right DisplayItems -> do
      putStrLn "The List of items is:"
      putStrLn (displayItems items)
      interactWithUser items

    Right (AddItem item) -> do
      let newItems = addItem item items
      putStrLn "Item added."
      interactWithUser newItems

    Right Quit -> do
      putStrLn "Bye!"
      pure ()

    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items
```

---

### TODO - interact

```sh
TODO app
Commands: quit, items, add - <item to add>
add - Add a remove item command
Item added.
Commands: quit, items, add - <item to add>
add - Maybe also display the list of commands only once    
Item added.
Commands: quit, items, add - <item to add>
items
The List of items is:
1 - Add a remove item command
2 - Maybe also display the list of commands only once

Commands: quit, items, add - <item to add>
quit
Bye!
Thanks for using this app.
```

---

### Let's add a help command

```hs
data Command
  ...
  | Help

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ...
  ["help"] -> Right Help
  _ -> Left "Unknown command."

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine
  case parseCommand line of
    Right Help -> do
      putStrLn "Commands: help, quit, items, add - <item to add>"
      interactWithUser items
    ...
```

* the pattern `_` serves as a "catch all" so we need to add the pattern for `["help"]` before it.


---

### TODO - removeItem

```hs
-- Returns a new list of items or an error message if the index is out of bounds
removeItem :: Int -> Items -> Either String Items
removeItem reverseIndex allItems =
    impl (length allItems - reverseIndex) allItems
  where
    impl index items =
      case (index, items) of
        (0, item : rest) ->
          Right rest
        (n, []) ->
          Left "Index out of bounds."
        (n, item : rest) ->
          case impl (n - 1) rest of
            Right newItems ->
              Right (item : newItems)
            Left errMsg ->
              Left ("Error: " ++ errMsg)
```

---

### TODO - add commands for marking an Item as done

```hs
data Command
  ...
  | Done Int

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ...
  ["done", idxStr] ->
    if all (\c -> elem c "0123456789") idxStr
      then Right (Done (read idxStr))
      else Left "Invalid index."
  _ -> Left "Unknown command."

interactWithUser :: Items -> IO ()
interactWithUser items = do
  line <- getLine
  case parseCommand line of
    Right Help -> do
      putStrLn "Commands: help, quit, items, add - <item to add>, done <item index>"
      interactWithUser items
    Right (Done index) -> do
      let result = removeItem index items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right newItems -> do
          putStrLn "Item done."
          interactWithUser newItems

    ...
```

---

## TODO - Done

```sh
TODO app
help
Commands: help, quit, items, add - <item to add>, done <item index>
add - Greet user
Item added.
add - Model data and user interaction
Item added.
add - Implement data modification
Item added.
add - Implement state and iteration using recursion
Item added.
add - Parse user input
Item added.
add - Interact with the user
Item added.
items
The List of items is:
1 - Greet user
2 - Model data and user interaction
3 - Implement data modification
4 - Implement state and iteration using recursion
5 - Parse user input
6 - Interact with the user

done 1
Item done.
items 
The List of items is:
1 - Model data and user interaction
2 - Implement data modification
3 - Implement state and iteration using recursion
4 - Parse user input
5 - Interact with the user

done 1
Item done.
done 1
Item done.
done 1
Item done.
items
The List of items is:
1 - Parse user input
2 - Interact with the user

done 1
Item done.
done 1
Item done.
items
The List of items is:

quit
Bye!
Thanks for using this app.
```

---

## TODO - Done

The final app's source code can be viewed [here](https://github.com/soupi/rfc/blob/master/Todo.hs).

---

## This code is still a bit messy. Can we do better?

#### Yes!

* Haskell provides us with all kinds of features to simplify the code and reduce code duplication!

* But This is a story for another time.

---

## Curious? Want to learn more?

* [Get started with Haskell](https://haskell-lang.org/get-started)
* [Haskell Programming From First Principles](http://haskellbook.com)
* [Haskell wikibook](https://en.wikibooks.org/wiki/Haskell)

---




