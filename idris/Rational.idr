module Rational

import Setoid
import MyInteger

%access public export
%default total

||| x from R = a from Z / (b + 1) , b from N
data Rational = Rat MyInteger Nat

implementation Num Rational where
    -- a/(1 + b) + c/(1 + d) = (a * (1 + d) + c * (1 + b) / ((1 + b) * (1 + d))
    (+) (Rat a b) (Rat c d) = Rat ((a *^ S d) + (c *^ S b)) (b + d + b * d)
    -- a/(1 + b) * c/(1 + d) = a * c / ((1 + b) * (1 + d))
    (*) (Rat a b) (Rat c d) = Rat (a * c) (b + d + b * d)

    fromInteger a = Rat (fromInteger a) Z

implementation Neg Rational where
    negate (Rat a b) = Rat (negate a) b
    a - b = a + negate b

||| Equality on Rational
data RatEq : Rational -> Rational -> Type where
    ||| a/(1 + b) == c/(1 + d)  <=>  a * (1 + d) == b * (1 + c)
    RatRefl : (eq : a *^ (S d) `IntEq` c *^ (S b)) -> RatEq (Rat a b) (Rat c d)

ratReflx : Reflx RatEq
ratReflx (Rat a b) = RatRefl (intRefl (a *^ (S b)))

ratSym : Sym RatEq
ratSym (Rat a b) (Rat c d) (RatRefl eq) = RatRefl $ intSym (a *^ (S d)) (c *^ (S b)) eq



{-
   Given:
    (a1 * S d) + (c2 * S b) = (c1 * S b) + (a2 * S d) &&
    (c1 * S f) + (e2 * S d) = (e1 * S d) + (c2 * S f)

1. multiply (1) by Sf and (2) by Sb
    (a1 * S d) * S f + (c2 * S b) * S f = (c1 * S b) * S f + (a2 * S d) * S f &&
    (c1 * S f) * S b + (e2 * S d) * S b = (e1 * S d) * S b + (c2 * S f) * S b

2. sum up
    (a1 * S d) * S f + (c2 * S b) * S f + (c1 * S f) * S b + (e2 * S d) * S b =
    (c1 * S b) * S f + (a2 * S d) * S f + (e1 * S d) * S b + (c2 * S f) * S b

3. remove terms with c1 and c2
    (a1 * S d) * S f + (e2 * S d) * S b = (a2 * S d) * S f + (e1 * S d) * S b

4. get out S d
    S d * (a1 * S f + e2 * S b) = S d * (a2 * S f + e1 * S b)

5. cut
    a1 * S f + e2 * S b = e1 * S b + a2 * S f
-}
ratTrans : Trans RatEq
ratTrans (Rat (SubInt a1 a2) b) (Rat (SubInt c1 c2) d) (Rat (SubInt e1 e2) f)
        (RatRefl (IntRefl eq1)) (RatRefl (IntRefl eq2)) = RatRefl $ IntRefl step5
  where
    step1' : (u1 * S y)       + (v2 * S x)       = (v1 * S x)       + (u2 * S y)       ->
             (u1 * S y) * S z + (v2 * S x) * S z = (v1 * S x) * S z + (u2 * S y) * S z
    step1' prf {u1 = u1} {u2 = u2} {v1 = v1} {v2 = v2} {x = x} {y = y} {z = z} =
        rewrite sym $ multDistributesOverPlusLeft (u1 * S y) (v2 * S x) (S z) in
        rewrite sym $ multDistributesOverPlusLeft (v1 * S x) (u2 * S y) (S z) in
        cong {f = (* S z)} prf

    step1'eq1 : (a1 * S d) * S f + (c2 * S b) * S f = (c1 * S b) * S f + (a2 * S d) * S f
    step1'eq1 = step1' eq1
    step1'eq2 : (c1 * S f) * S b + (e2 * S d) * S b = (e1 * S d) * S b + (c2 * S f) * S b
    step1'eq2 = step1' eq2

    step2' : {u : Nat} -> {v : Nat} -> {x : Nat} -> {y : Nat} ->
             (u = v) -> (x = y) -> (u + x = v + y)
    step2' prf1 prf2 = rewrite prf1 in rewrite prf2 in Refl

    step2 : ((a1 * S d) * S f + (c2 * S b) * S f) + ((c1 * S f) * S b + (e2 * S d) * S b) =
            ((c1 * S b) * S f + (a2 * S d) * S f) + ((e1 * S d) * S b + (c2 * S f) * S b)
    step2 = step2' step1'eq1 step1'eq2

    step3 : (a1 * S d) * S f + (e2 * S d) * S b =
            (a2 * S d) * S f + (e1 * S d) * S b
    step3 = rewrite plusCommutative (a1 * S d * S f) (e2 * S d * S b) in int5
      where
        int1Left : ((a1 * S d) * S f + (c2 * S b) * S f) + ((c1 * S f) * S b + (e2 * S d) * S b) =
                   ((c1 * S f) * S b + (e2 * S d) * S b) + ((a1 * S d) * S f + (c2 * S f) * S b)
        int1Left =
            rewrite sym $ multAssociative c2 (S f) (S b) in
            rewrite sym $ multAssociative c2 (S b) (S f) in
            rewrite sym $ multCommutative (S f) (S b) in
            rewrite plusCommutative ((a1 * S d) * S f + c2 * (S (b + f * S b))) ((c1 * S f) * S b + (e2 * S d) * S b) in
            Refl

        int1 : ((c1 * S f) * S b + (e2 * S d) * S b) + (a1 * S d) * S f + (c2 * S f) * S b =
               ((c1 * S b) * S f + (a2 * S d) * S f) + (e1 * S d) * S b + (c2 * S f) * S b
        int1 =
            rewrite sym $ plusAssociative ((c1 * S f) * S b + (e2 * S d) * S b) ((a1 * S d) * S f) ((c2 * S f) * S b) in
            rewrite sym $ plusAssociative ((c1 * S b) * S f + (a2 * S d) * S f) ((e1 * S d) * S b) ((c2 * S f) * S b) in
            rewrite sym $ int1Left in
            step2

        int2 : (c1 * S f) * S b + (e2 * S d) * S b + (a1 * S d) * S f =
               (c1 * S b) * S f + (a2 * S d) * S f + (e1 * S d) * S b
        int2 = plusRightCancel
            ((c1 * S f) * S b + (e2 * S d) * S b + (a1 * S d) * S f)
            ((c1 * S b) * S f + (a2 * S d) * S f + (e1 * S d) * S b)
            (c2 * S f * S b)
            int1

        int3 : (c1 * S f) * S b + ((e2 * S d) * S b + (a1 * S d) * S f) =
               (c1 * S b) * S f + ((a2 * S d) * S f + (e1 * S d) * S b)
        int3 =
            rewrite plusAssociative ((c1 * S f) * S b) ((e2 * S d) * S b) ((a1 * S d) * S f) in
            rewrite plusAssociative ((c1 * S b) * S f) ((a2 * S d) * S f) ((e1 * S d) * S b) in
            int2

        int4Left : (c1 * S b) * S f + ((e2 * S d) * S b + (a1 * S d) * S f) =
                   (c1 * S f) * S b + ((e2 * S d) * S b + (a1 * S d) * S f)
        int4Left =
            rewrite sym $ multAssociative c1 (S b) (S f) in
            rewrite multCommutative (S b) (S f) in
            rewrite multAssociative c1 (S f) (S b) in
            Refl

        int4 : (c1 * S b) * S f + ((e2 * S d) * S b + (a1 * S d) * S f) =
               (c1 * S b) * S f + ((a2 * S d) * S f + (e1 * S d) * S b)
        int4 = rewrite int4Left in int3

        int5 : (e2 * S d) * S b + (a1 * S d) * S f =
               (a2 * S d) * S f + (e1 * S d) * S b
        int5 = plusLeftCancel
            (c1 * S b * S f)
            ((e2 * S d) * S b + (a1 * S d) * S f)
            ((a2 * S d) * S f + (e1 * S d) * S b)
            int4

    step4 : S d * (a1 * S f + e2 * S b) = S d * (a2 * S f + e1 * S b)
    step4 =
        rewrite multDistributesOverPlusRight (S d) (a1 * S f) (e2 * S b) in
        rewrite multDistributesOverPlusRight (S d) (a2 * S f) (e1 * S b) in
        rewrite lemma (S d) a1 (S f) in
        rewrite lemma (S d) a2 (S f) in
        rewrite lemma (S d) e1 (S b) in
        rewrite lemma (S d) e2 (S b) in
        step3
      where
        lemma : (x : Nat) -> (y : Nat) -> (z : Nat) -> x * (y * z) = y * x * z
        lemma x y z =
            rewrite multAssociative x y z in
            rewrite multCommutative x y in
            Refl

    step5 : a1 * S f + e2 * S b = e1 * S b + a2 * S f
    step5 =
        rewrite plusCommutative (e1 * S b) (a2 * S f) in
        multLeftCancel _ _ _ step4
      where
        addensumIsZero : (x : Nat) -> (y : Nat) -> x + y = 0 -> x = 0
        addensumIsZero x Z prf = int
          where
            int : 0 + x = 0
            int = rewrite plusCommutative 0 x in prf
        addensumIsZero x (S k) prf = void $ SIsNotZ int
          where
            int : S k + x = 0
            int = rewrite plusCommutative (S k) x in prf

        multLeftCancel : (x : Nat) -> (y : Nat) -> (z : Nat) -> S x * y = S x * z -> y = z
        multLeftCancel x Z z prf = sym $ addensumIsZero _ _ (sym int)
          where
            int : 0 * x = z + (x * z)
            int = rewrite multCommutative 0 x in prf
        multLeftCancel x y Z prf = addensumIsZero _ _ int
          where
            int : y + (x * y) = 0 * x
            int = rewrite multCommutative 0 x in prf
        multLeftCancel x (S k) (S j) prf =
            let rec = multLeftCancel x k j in
            rewrite rec (plusLeftCancel _ _ _ int2) in
            Refl
          where
            prf' : k + x * S k = j + x * S j
            prf' = succInjective _ _ prf

            int1 : k + (x + x * k) = j + (x + x * j)
            int1 =
                rewrite sym $ multRightSuccPlus x k in
                rewrite sym $ multRightSuccPlus x j in
                prf'

            int2 : x + (k + x * k) = x + (j + x * j)
            int2 =
                rewrite plusAssociative x k (x * k) in
                rewrite plusAssociative x j (x * j) in
                rewrite plusCommutative x k in
                rewrite plusCommutative x j in
                rewrite sym $ plusAssociative k x (x * k) in
                rewrite sym $ plusAssociative j x (x * j) in
                int1


||| Setoid of Rat
RatSetoid : Setoid
RatSetoid = MkSetoid Rat RatEq $ EqProof RatEq ratReflx ratSym ratTrans