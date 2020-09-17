{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module State
  ( reverseWithState,
    State (..),
    append3ReversedWithState,
  )
where

reverseWithCount :: Int -> [a] -> (Int, [a])
reverseWithCount count list = (count + 1, reverse list)

append3ReversedWithCount :: Int -> [a] -> [a] -> [a] -> (Int, [a])
append3ReversedWithCount count list1 list2 list3 =
  let (c1, r1) = reverseWithCount count list1
      (c2, r2) = reverseWithCount c1 list2
      (c3, r3) = reverseWithCount c2 list3
   in (c3 + 1, r1 ++ r2 ++ r3)

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f (State runState) =
    State
      ( \s ->
          let (s', a) = runState s
           in (s', f a)
      )

instance Applicative (State s) where
  pure x = State (,x)
  (<*>)  (State runStateFn)  (State runState) =
    State
      ( \s ->
          let (s', f) = runStateFn s
              (s'', x) = runState s'
           in (s'', f x)
      )

instance Monad (State s) where
  return = pure

  (>>=) (State stateFn) nextFn =
    State
      ( \s ->
          let (s', x) = stateFn s
              State stateFn' = nextFn x
           in stateFn' s'
      )

sget :: State s s
sget = State (\s -> (s, s))

sput :: s -> State s ()
sput s = State $ const (s, ())

smod :: (s -> s) -> State s ()
smod stateFn =
  sget >>= (sput . stateFn)

reverseWithState :: [a] -> State Int [a]
reverseWithState list = do
  smod (+ 1)
  return $ reverse list

append3WithState :: [a] -> [a] -> [a] -> State Int [a]
append3WithState listA listB listC = do
  smod (+ 1)
  return $ listA ++ listB ++ listC

append3ReversedWithState :: [a] -> [a] -> [a] -> State Int [a]
append3ReversedWithState listA listB listC = do
  rlistA <- reverseWithState listA
  rlistB <- reverseWithState listB
  rlistC <- reverseWithState listC
  append3WithState rlistA rlistB rlistC
