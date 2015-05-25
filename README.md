# Desugarer

Desugars do-notation into infix applications of `(>>=)` and `return`.
Just a fun adventure in SYB-land. Reads from stdin.

Suppose the file SumFile.hs contains the following snippet (from one
of my first Haskell programs a few years back!):

    -- SumFile.hs
    sumFile :: IO ()
    sumFile = do
                args <- getArgs
                if (not $ null args)
                then do
                    contents <- readFile $ head args
                    let ints = map read (lines contents) :: [Int]
                    print $ foldr (+) 0 ints
                else return ()

It should walk the tree and pretty-print the desugared output.

    $ desugar < SumFile.hs
    sumFile :: IO ()
    sumFile
      = getArgs >>=
          \ args ->
            if (not $ null args) then
              readFile $ head args >>=
                \ contents ->
                  let ints = map read (lines contents) :: [Int] in
                    print $ foldr (+) 0 ints
              else return ()

No support for `-XArrows` - I have no idea how they work and don't know
how to desugar them properly.