> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE TypeFamilies           #-}
> {-# LANGUAGE TypeOperators          #-}


Recall the example from the lecture, where we used GADTs to build lists whose length is encoded in their type and therefore known at compile-time. 

We called these lists vectors and used their construction to define safe head and indexing functions. Calling these safe functions with invalid inputs (e.g head on an empty vector) leads to compile-time type errors instead of runtime value errors.

In today's Lab we will extend this example from vectors to matrices and our goal will be to define a type-safe matrix multiplication function.

1) For convenience we will redefine the type Nat' with shorter data constructors. 

1.1) Create a type Nat with data constructors Z (for Zero) and S (for Succ). 
1.2) Do we need GADT syntax? 
1.3) Explicitly define type synonyms One, Two and Four that represent their value in type Nat.
1.4) Implement the Show Typeclass for type Nat.
1.5) [Bonus] ]Implement Addition (#+), Multiplication (#*) and Exponentiation (#^) for type Nat


> data Nat' where
>   Zero :: Nat'
>   Succ :: Nat' -> Nat'

2) Again, for convenience we will redefine the type Vec' with more ergonomic data constructors. 

2.1) Create a type Vec where the Cons constructor is an infix operator (:-) with right associativity and precedence 5. 
2.2) Implement the Show Typeclass for Vec
2.3) Implement the (@+) infix operator as vector addition.


> data Vec' :: Nat' -> * -> * where
>   VNil' :: Vec' 'Zero a
>   VCons :: a -> Vec' n a -> Vec' ('Succ n) a


> -- data Vec :: Nat -> * -> * where

> -- (@+) :: (Num a) => Vec k a -> Vec k a -> Vec k a


3) We can write a total function (a function that does not fail for any value of its input type) for operations like head, tail and indexing that can lead to runtime errors when used with plain lists.

3.1) Write a total function safeTail for Vec that acts like the tail function on nonempty lists.


> -- safeHead :: Vec ('S n) a -> a
> -- safeHead (x :- _) = x


4) Let's define a type Mat for matrices based on Vec. 

4.1) Define a type Mat as a vector of (column) vectors. The first Nat corresponds to the number of rows, the second Nat to the number of columns. Make sure that entries of matrices of type Mat have to implement the Num Typeclass.
4.2) Implement the Show Typeclass for type Mat.


> -- data Mat :: Nat -> Nat -> * -> * where


5) We used GADTs to define a family of Singleton Types SNat for type Nat. Every Singleton Nat is inhabitated by exactly one Nat (e.g. SZ is inhabitated by Z, SS SZ is inhabitated by SZ). This one-to-one correspondance between types and values can be encode values (e.g. an index of type Nat) as types (of type SNat). We used this mechanism to define a safe indexing function.

5.1) Define a safe indexing operator (!!!) for type Mat.


> -- type family (m::Nat) :< (n::Nat) :: Bool
> -- type instance m      :< 'Z     = 'False
> -- type instance 'Z     :< ('S n) = 'True
> -- type instance ('S m) :< ('S n) = m :< n


> --data SNat :: Nat -> * where
> --   SZ :: SNat 'Z
> --   SS :: SNat n -> SNat ('S n)


> -- nth :: (m:<n) ~ 'True => SNat m -> Vec n a -> a
> -- nth SZ (a :- _)        = a
> -- nth (SS sm') (_ :- as) = nth sm' as


6) As a preparation for the matrix multiplication, let's add some more functionality to our vectors.

6.1) Implement the Typeclass Foldable for Vec
6.2) Implement the Typeclass Functor for Vec


7) Finally we tackle matrix multiplication.

7.1) Implement the infix operator (#) as the dot product between two vectors.
7.2) Implement the infix operator (#@) as the matrix-vector product. Hint: Decompose the matrix-vector product into two steps: scaling and adding columns.
7.3) Use the matrix vector product to implement the infix operator (#@@) as matrix-matrix multiplication.
7.4) Create examples to check that matrix-matrix multiplications of invalid dimensions are rejected by the type-checker.


> -- (#) :: (Num a) => Vec n a -> Vec n a -> a


> -- (#@) :: (Num a) => Mat (S m) (S k) a -> Vec (S k) a -> Vec (S m) a


> -- colSum :: (Num a) => Vec k (Vec m a) -> Vec m a

> -- scale :: (Num a) => Vec k (Vec m a) -> Vec k a -> Vec k (Vec m a) 

> -- (#@@) :: (Num a) => Mat (S m) (S k) a -> Mat (S k) (S n) a -> Mat (S m) (S n) a
