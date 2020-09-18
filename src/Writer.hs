module Writer
  ( helloAdditionWriter,
    Writer (..),
    listen
  )
where

helloAdditionWriter :: Int -> Int -> Writer [String] Int
helloAdditionWriter x y = do
  z <- loggedAddition x y
  tell ["Hello, performed the addition giving " ++ show z]
  return z

loggedAddition :: Int -> Int -> Writer [String] Int
loggedAddition x y = Writer (x + y, ["Added " ++ show x ++ " and " ++ show y])

newtype Writer log a = Writer {runWriter :: (a, log)}

instance Functor (Writer log) where
  fmap f (Writer (x, l)) = Writer (f x, l)

instance Monoid log => Applicative (Writer log) where
  pure x = Writer (x, mempty)
  (<*>) (Writer (f, l)) (Writer (x, l')) = Writer (f x, l <> l')

instance Monoid log => Monad (Writer log) where
  return = pure
  (>>=) (Writer (x, l)) f = let Writer (y, l') = f x in Writer (y, l <> l')

tell :: Monoid log => log -> Writer log ()
tell line = Writer ((), line)

listen :: Writer log a -> Writer log (a, log)
listen (Writer (x, l)) = Writer ((x, l), l)
