module MyInteger

import Setoid

%access public export
%default total

||| x from Z = y from N - z from N
data MyInteger = SubInt Nat Nat

implementation Num MyInteger where
    ||| ((a - b)) + ((c - d)) = (((a + c) - (b + d)))
    (+) (SubInt a b) (SubInt c d) = SubInt (a + c) (b + d)

    ||| ((a - b)) * ((c - d)) = (((a * c + b * d) - (a * d - b * c)))
    (*) (SubInt a b) (SubInt c d) = SubInt (a * c + b * d) (a * d + b * c)

    fromInteger n =
        if n > 0
           then SubInt (fromInteger n) Z
           else SubInt Z (fromInteger $ abs n)

implementation Neg MyInteger where
    negate (SubInt a b) = SubInt b a
    (SubInt a b) - (SubInt c d) = SubInt (a + d) (b + c)

infixl 5 *^
(*^) : MyInteger -> Nat -> MyInteger
(*^) (SubInt a b) k = SubInt (a * k) (b * k)


data IntEq : MyInteger -> MyInteger -> Type where
    ||| a - b == c - d  <=>  a + d == c + b
    IntRefl : (eq : a + d = c + b) -> IntEq (SubInt a b) (SubInt c d)

intRefl : Reflx IntEq
intRefl (SubInt a b) = IntRefl $ Refl {x = a + b}

intSym : Sym IntEq
intSym (SubInt a b) (SubInt c d) (IntRefl eq) = IntRefl $ sym eq

reflPlus : {a : Nat} -> {b : Nat} -> {c : Nat} -> {d : Nat} ->
    (a = b) -> (c = d) -> (a + c = b + d)
reflPlus eq1 eq2 = rewrite eq1 in rewrite eq2 in Refl

intTrans : Trans IntEq
intTrans (SubInt a b) (SubInt c d) (SubInt e f) (IntRefl eq1) (IntRefl eq2) = IntRefl f2
  where
    eq3  : (a + d) + (c + f) = (c + b) + (e + d)
    step11 : (a + d) + (c + f) = (c + f) + (a + d)
    step12 : (c + f) + (a + d) = ((c + f) + a) + d
    step13 : (c + b) + (e + d) = ((c + b) + e) + d
    step14 : ((c + f) + a) + d = ((c + b) + e) + d
    step15  : (c + f) + a = (c + b) + e
    step21 : c + (f + a) = (c + b) + e
    step22 : (f + a) + c = (c + b) + e
    step23 : (f + a) + c = c + (b + e)
    step24 : (f + a) + c = (b + e) + c
    step25  : f + a = b + e
    f1 : a + f = b + e
    f2 : a + f = e + b

    eq3  = reflPlus eq1 eq2
    step11 = plusCommutative (a + d) (c + f)
    step12 = plusAssociative (c + f) a d
    step13 = plusAssociative (c + b) e d
    step14 = trans (sym step12) $ trans (sym step11) $ trans eq3 step13
    step15  = plusRightCancel ((c + f) + a) ((c + b) + e) d step14
    step21 = trans (plusAssociative c f a) step15
    step22 = trans (plusCommutative (f + a) c) step21
    step23 = trans step22 $ sym $ plusAssociative c b e
    step24 = trans step23 $ plusCommutative c (b + e)
    step25  = plusRightCancel (f + a) (b + e) c step24
    f1 = trans (plusCommutative a f) step25
    f2 = trans f1 $ plusCommutative b e

IntegerSetoid : Setoid
IntegerSetoid = MkSetoid MyInteger IntEq $ EqProof IntEq intRefl intSym intTrans