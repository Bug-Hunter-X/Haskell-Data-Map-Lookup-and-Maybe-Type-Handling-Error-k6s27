The solution involves using pattern matching to safely handle the `Maybe` type.  If the key exists, extract the value; otherwise, provide a default value or handle the absence of a value appropriately.

```haskell
import qualified Data.Map as Map

main :: IO ()
main = do
  let myMap = Map.fromList [(1, "a"), (2, "b"), (3, "c")]

  case Map.lookup 2 myMap of
    Just val -> print val  -- Prints "b"
    Nothing -> print "Key not found"

  case Map.lookup 4 myMap of
    Just val -> print val
    Nothing -> print "Key not found" -- Prints "Key not found"

  let result = case Map.lookup 2 myMap of
                Just val -> val
                Nothing -> ""
  print result --Prints "b"
```

Alternatively, the `fromMaybe` function from `Data.Maybe` can be used to provide a default value:

```haskell
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  let myMap = Map.fromList [(1, "a"), (2, "b"), (3, "c")]
  let value = fromMaybe "" (Map.lookup 2 myMap)
  print value  -- Prints "b"
  let otherValue = fromMaybe "" (Map.lookup 4 myMap)
  print otherValue -- Prints ""
```