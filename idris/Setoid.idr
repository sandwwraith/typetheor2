module Setoid

%access public export
%default total

||| Reflexivity
Reflx : {A : Type} -> (R : A -> A -> Type) -> Type
Reflx {A} R = (x : A) -> R x x

||| Symmetry
Sym : {A : Type} -> (R : A -> A -> Type) -> Type
Sym {A} R = (x : A) -> (y : A) -> R x y -> R y x

||| Transitivity
Trans : {A : Type} -> (R : A -> A -> Type) -> Type
Trans {A} R = (x : A) -> (y : A) -> (z : A) -> R x y -> R y z -> R x z

||| Equivalence relation
data IsEquivalence : {A : Type} -> (R : A -> A -> Type) -> Type where
    EqProof : {A : Type} -> (R : A -> A -> Type) -> Reflx {A} R -> Sym {A} R -> Trans {A} R -> IsEquivalence {A} R

||| Setoid is a set with equivalence relation
record Setoid where
    constructor MkSetoid
    ||| Type of setoid
    Carrier : Type
    ||| Equivalence relation
    Equiv : Carrier -> Carrier -> Type
    ||| Proof that Equiv is equivalence relation
    EquivProof : IsEquivalence Equiv
