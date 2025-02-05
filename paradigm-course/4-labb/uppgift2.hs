loop :: Integer -> IO ()
loop secret = do
  putStrLn "Enter a number between 1-99!"
  x <- getLine
  let number = read x
  if not (elem number [1 .. 99])
    then do
      putStrLn "You must enter a number between 1-99!"
      loop secret
    else
      if number < secret
        then do
          putStrLn "Too low!"
          loop secret
        else
          if number > secret
            then do
              putStrLn "Too high!"
              loop secret
            else putStrLn "Success!"

main :: IO ()
main = do
  let secret = 63
  loop secret

main_ :: IO ()
main_ = loop_ 63

loop_ :: Integer -> IO ()
loop_ secret =
  putStrLn "Give me a guess"
    >>= ( \() ->
            getLine
              >>= ( \x ->
                      if x == show secret
                        then
                          putStrLn "Success!"
                        else
                          loop secret
                  )
        )
