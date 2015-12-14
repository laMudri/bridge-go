{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where
  import Data.Monoid
  import Data.List
  import Data.Set (Set)
  import qualified Data.Set as S
  import Prelude.Unicode
  import qualified Data.Set.Unicode as S
  import Control.Applicative
  import Data.Maybe

  type Moku = Maybe Player
  type Index = Int
  data Board = Board Index (Point → Moku)
  newtype Player = Player Bool deriving (Eq,Ord)
  type Point = (Index,Index)
  type ErrorMessage = String
  data Result = Win Player Board
              | NoResult Player Board (Set Chain)
              | Illegal Player Board ErrorMessage
    deriving Show
  data Chain = Chain { player :: Player, points :: Set Point }
    deriving (Eq,Ord,Show)

  instance Show Player where
    show (Player False) = "Black"
    show (Player True) = "White"
  instance Show Board where
    show (Board s f) =
      ' ' : take s ds ++ '\n' : unlines (zipWith (:) ds [[g (f (x,y))
                                                         | x ← [0..pred s]]
                                                        | y ← [0..pred s]])
      where
        g (Just (Player False)) = '●'
        g (Just (Player True)) = '○'
        g Nothing = '┼'
        ds = cycle "0123456789"

  instance Eq Board where
    Board s0 f0 == Board s1 f1 = s0 ≡ s1 ∧ all (\p → f0 p ≡ f1 p) (coords s0)
  instance Ord Board where
    Board s0 f0 `compare` Board s1 f1 =
      s0 `compare` s1 <> mconcat (map (\p → f0 p `compare` f1 p) (coords s0))

  coords ∷ Index → [(Index,Index)]
  coords s = [(x,y) | x ← [0..pred s], y ← [0..pred s]]

  play ∷ Index → [Point] → Result
  play s = playFrom S.empty (Player False) (Board s (const Nothing)) S.empty
  playFrom ∷ Set Board → Player → Board → Set Chain → [Point] → Result
  playFrom bs p b cs [] = NoResult p b cs
  playFrom bs p b cs (m:ms) = case playAt bs p b cs m of
                                r@(Win _ _) → r
                                NoResult p' b' cs' →
                                  playFrom (S.insert b bs) p' b' cs' ms
                                r@(Illegal _ _ _) → r
  playAt ∷ Set Board → Player → Board → Set Chain → Point → Result
  playAt bs p b@(Board s f) cs m@(x,y)
    | 0 ≤ x ∧ x < s ∧ 0 ≤ y ∧ y < s =
        case f m of
          Nothing → let (b',cs') = clear p $ clear (otherPlayer p)
                                           $ putStone p m (b,cs) in
            case find (winningChain b') $ S.elems cs' of
              Just (Chain wp _) → Win wp b'
              Nothing → if b' S.∈ bs ∨ b' ≡ b
                           then Illegal p b "Kou"
                           else NoResult (otherPlayer p) b' cs'
          Just _ → Illegal p b $ "Stone already at " ++ show m
    | otherwise = Illegal p b "Out of range"
  putStone ∷ Player → Point → (Board,Set Chain) → (Board,Set Chain)
  putStone p m (Board s f,cs) = (Board s f',cs')
    where
      f' m' = if m' ≡ m then Just p else f m'
      cs' = S.insert newChain $ foldr S.delete cs sideChains
      newChain = Chain p $ mconcat $ S.singleton m : map points sideChains
      sideChains = mapMaybe chainAtSide edges
      chainAtSide ∷ Side → Maybe Chain
      chainAtSide s = find (\c → p ≡ player c ∧ step s m `isPointInChain` c)
                           (S.elems cs)
  liberties ∷ Board → Point → Set Point
  liberties b = (\case Just c → chainLiberties b c
                       Nothing → S.empty) ∘ chainFromPoint b
  chainLiberties ∷ Board → Chain → Set Point
  chainLiberties (Board s f) (Chain p ms) =
    mconcat $ S.elems $ S.mapMonotonic immediateLiberties ms
    where
      immediateLiberties =
        S.filter (\n → f n ≡ Nothing ∧
                       external s ((Just p ≡) ∘ f) n) ∘ neighbours
      neighbours (x,y) =
        g (x ≡ 0) (pred x,y) $
        g (succ x ≡ s) (succ x,y) $
        g (y ≡ 0) (x,pred y) $
        g (succ y ≡ s) (x,succ y) S.empty
        where
          g t o = if t then id else S.insert o
  chainFromPoint ∷ Board → Point → Maybe Chain
  chainFromPoint (Board size f) m = fmap (`Chain` points) (f m)
    where
      p = f m
      points = case find (isPointInSpace m)
                       $ S.elems (falseSpaces size ((p ≢) ∘ f)) of
                 Just (Space ps _) → ps
                 Nothing → error "Eh?"
  winningChain ∷ Board → Chain → Bool
  winningChain (Board s _) (Chain _ ps) =
    all (\i -> setAny (\(x,_) -> i ≡ x) ps) [0..pred s] ∨
    all (\i -> setAny (\(_,y) -> i ≡ y) ps) [0..pred s]
    where setAny p = S.foldr ((∨) . p) False

  otherPlayer ∷ Player → Player
  otherPlayer (Player p) = Player (not p)

  clear ∷ Player → (Board,Set Chain) → (Board,Set Chain)
  clear p (b@(Board s f),cs) = (Board s f',liveChains S.∪ otherChains)
    where
      (theseChains,otherChains) = S.partition ((p ≡) ∘ player) cs
      f' m = if any (m `isPointInChain`) (S.elems deadChains) then Nothing
                                                              else f m
      (deadChains,liveChains) =
        S.partition ((0 ≡) ∘ S.size ∘ chainLiberties b) theseChains

  isPointInChain ∷ Point → Chain → Bool
  isPointInChain p (Chain _ ps) = p S.∈ ps
  isPointInSpace ∷ Point → Space → Bool
  isPointInSpace p (Space ps _) = p S.∈ ps

  data Side = LeftS | TopS | RightS | BottomS deriving (Eq,Ord,Show)
  data Space = Space (Set Point) (Set Side)
               deriving (Show,Eq,Ord)
  instance Monoid Space where
    mempty = Space mempty mempty
    Space ps0 ss0 `mappend` Space ps1 ss1 = Space (ps0 S.∪ ps1) (ss0 S.∪ ss1)

  edges = [LeftS,TopS,RightS,BottomS]

  atEdge ∷ Index → Side → Point → Bool
  atEdge _ LeftS (x,_) = x ≡ 0
  atEdge _ TopS (_,y) = y ≡ 0
  atEdge size RightS (x,_) = x ≡ pred size
  atEdge size BottomS (_,y) = y ≡ pred size

  pointSpace ∷ Index → (Point → Bool) → Point → Space
  pointSpace size f p =
    if g p
       then Space mempty mempty
       else Space (S.singleton p)
                  (mconcat (map (\s → if atEdge size s p then S.singleton s
                                                         else mempty) edges))
    where g q@(x,y) = 0 ≤ x ∧ x < size ∧ 0 ≤ y ∧ y < size ∧ f q

  externalSpace ∷ Space → Bool
  externalSpace (Space _ ss) = (f LeftS ∧ f RightS) ∨ (f TopS ∧ f BottomS)
    where f = (S.∈ ss)

  falseSpaces ∷ Index → (Point → Bool) → Set Space
  falseSpaces size f = go mempty (Just (0,0))
    where
      go ∷ Set Space → Maybe Point → Set Space
      go ss Nothing = ss
      go ss (Just p)
         | f p = go ss (next size p)
         | otherwise = go ss'' (next size p)
         where
           psp = pointSpace size f
           spaceLeft = find (\(Space ps _) → step LeftS p S.∈ ps)
                            (S.elems ss)
           spaceTop = find (\(Space ps _) → step TopS p S.∈ ps)
                           (S.elems ss)
           interspace = case spaceLeft of
                          Just s → psp p <> s
                          Nothing → psp p
           ss' = case spaceLeft of
                   Just s → S.insert interspace (S.delete s ss)
                   Nothing → S.insert interspace ss
           ss'' = case spaceTop of
                    Just s → S.insert (interspace <> s) $ S.delete interspace
                                                        $ S.delete s ss'
                    Nothing → ss'

  step ∷ Side → Point → Point
  step LeftS (x,y) = (pred x,y)
  step TopS (x,y) = (x,pred y)
  step RightS (x,y) = (succ x,y)
  step BottomS (x,y) = (x,succ y)

  next ∷ Index → Point → Maybe Point
  next size (x,y)
       | x' ≡ size = if succ y ≡ size then Nothing else Just (0,succ y)
       | otherwise = Just (x',y)
       where x' = succ x

  external ∷ Index → (Point → Bool) → Point → Bool
  external size f q = not (f q) ∧ case find (\(Space ps _) → q S.∈ ps)
                                            (S.elems (falseSpaces size f)) of
                                    Just s → externalSpace s
                                    Nothing → error "Eh?"

  playIO ∷ Set Board → Player → Board → Set Chain → IO ()
  playIO bs p b cs = do
    putStr $ show p ++ " to play: "
    l ← getLine
    if null l then return ()
    else do
      m ← readIO $ '(' : l ++ ")"
      case playAt bs p b cs m of
        Win p' b' → print b' >> putStrLn (show p' ++ " wins!")
        NoResult p' b' cs' → print b' >> playIO (S.insert b bs) p' b' cs'
        Illegal _ _ msg → putStrLn ("Error: " ++ msg) >> playIO bs p b cs

  main ∷ IO ()
  main = do
    putStr "Size: "
    size ← readLn
    print (Board size (const Nothing))
    playIO S.empty (Player False) (Board size (const Nothing)) S.empty
