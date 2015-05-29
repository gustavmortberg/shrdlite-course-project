import Data.List
import Control.Monad

data Obj = A | B | C | D | E | F | G | H | I | J | K | L | M | Floor
  deriving (Eq,Show)
type Loc = Obj

type Conj a = [a]
type DNF a  = [Conj a]

data Rel = Injective | NonInjective
    deriving Eq


conjunction :: DNF a -> Bool
conjunction = any ((>1) . length)

disjunction :: DNF a -> Bool
disjunction = (>1) . length

buildStates :: Rel -> DNF Obj -> DNF Loc -> DNF (Obj,Loc)
buildStates rel dnfObjs dnfLocs
    | conjunction dnfObjs || conjunction dnfLocs = handleConjs rel dnfObjs dnfLocs
    | otherwise = [ [(o,l)] | o <- concat dnfObjs, l <- concat dnfLocs, o /= l ]

handleConjs :: Rel -> DNF Obj -> DNF Loc -> DNF (Obj,Loc)
handleConjs NonInjective dnfObjs dnfLocs
    | disjunction dnfLocs = let locs = concat dnfLocs
                                objs = head dnfObjs in
                       filter (all $ uncurry (/=)) [ zip objs pLocs | pLocs <- replicateM (length objs) locs ]
    | otherwise           = [ [ (o,l) | o <- cObjs, l <- head dnfLocs, o /= l] | cObjs <- dnfObjs]

handleConjs Injective dnfObjs dnfLocs
    | disjunction dnfLocs &&
      length locs >= length objs = filter (all $ uncurry (/=)) [ zip objs pLocs | pLocs <- permutations locs ]
    | otherwise                  = []
  where objs = head dnfObjs
        locs = concat dnfLocs

{-
[[(E,H)]
,[(E,G)]
,[(A,H)]
,[(A,G)]
,[(L,H)]
,[(L,G)]
,[(I,H)]
,[(I,G)]
,[(H,H)]
,[(H,G)]
,[(J,H)]
,[(J,G)]
,[(K,H)]
,[(K,G)]
,[(G,H)]
,[(G,G)]
,[(C,H)]
,[(C,G)]
,[(B,H)]
,[(B,G)]
,[(D,H)]
,[(D,G)]
,[(M,H)]
,[(M,G)]
,[(F,H)]
,[(F,G)]
,[(Floor,H)]
,[(Floor,G)]]

-}