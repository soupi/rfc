module Todo where

main :: IO ()
main = do
  putStrLn "TODO app"
  interactWithUser []
  putStrLn "Thanks for using this app."


type Item = String
type Items = [Item]


data Command
  = Quit
  | DisplayItems
  | AddItem String
  | Done Int
  | Help

parseCommand :: String -> Either String Command
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["items"] -> Right DisplayItems
  ["help"] -> Right Help
  "add" : "-" : item -> Right (AddItem (unwords item))
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

    Right (Done index) -> do
      let result = removeItem index items
      case result of
        Left errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          interactWithUser items
        Right newItems -> do
          putStrLn "Item done."
          interactWithUser newItems

    Left errMsg -> do
      putStrLn ("Error: " ++ errMsg)
      interactWithUser items


addItem :: Item -> Items -> Items
addItem item items = item : items

displayItems :: Items -> String
displayItems items =
  let
    displayItem index item = show index ++ " - " ++ item
    reversedList = reverse items
    displayedItemsList = zipWith displayItem [1..] reversedList
  in
    unlines displayedItemsList

removeItem :: Int -> Items -> Either String Items
removeItem idxToRemove allItems
  | (idxToRemove < 1) || (idxToRemove > (length allItems)) = Left "Index out of bounds"
  | otherwise = Right $ foldr ff [] (zip [1..] allItems)
  where
    ff :: (Int, Item) -> Items -> Items
    ff (idx, item) acc = if idx /= idxToRemove then (item : acc) else acc
