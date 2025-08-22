module Main where

import Reader

main :: IO ()
main = do
  putStrLn "Running the reader!"
  r <- runReader (100 :: Int) $ do
    val0 <- ask
    Reader $ putStrLn $ "Passed value: " <> show val0
    local (* 500) $ do
      val1 <- ask
      Reader $ putStrLn $ "Within local: " <> show val1
      pure $ val1 `div` val0
  putStrLn $ "Final result: " <> show r

