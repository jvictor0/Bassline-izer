module Utils where

bot = bot

filterMUntil :: (Monad m) => Int -> (a -> m Bool) -> [a] -> m [a]
filterMUntil 0 _ _ = return []
filterMUntil _ _ [] = return []
filterMUntil n f (a:as) = do
  b <- f a
  if b then do
    rest <- filterMUntil (n-1) f as
    return $ a:rest
    else filterMUntil n f as
    
         
headReverse [] = []
headReverse ls =(head ls):(reverse $ tail ls)

(f `on` g) x y = f (g x) (g y)

class UnShow u where
  unShow :: String -> u
  
