### Overview

`synchronized` is a simple Haskell function that guards an arbitrary IO action with an integer indicating the maximum number of threads that may execute the action concurrently.

```haskell
synchronized :: Q Exp
```

It uses Template Haskell to grab the source location of the call-site, which acts as the _monitor_ of the given action. The "true" type of `synchronized` is:

```haskell
synchronized :: Int -> IO a -> IO a
```

### Example Usage

Create a replacement for `putStrLn` that only allows one thread to call it at a time, preventing overlapping output:

```haskell
import Control.Concurrent.Synchronized (synchronized)

import qualified Prelude (putStrLn)

putStrLn :: String -> IO ()
putStrLn str = $synchronized 1 (Prelude.putStrLn str)
```

Create a replacement for `mapConcurrently` that takes an additional `Int` argument indicating the maximum number of threads to run concurrently:

```haskell
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.Synchronized (synchronized)

mapConcurrentlyN :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyN n f xs = mapConcurrently (\x -> $synchronized n (f x)) xs
```

(Note that the above example will still immediately spawn a thread for each element in `xs`, even though only `n` will run at a time).
