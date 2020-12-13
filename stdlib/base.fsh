#| Strict Fixpoint Combinator (Z Combinator)
#| Often used for inline recursion
#| ```
#| fib x = if x < 2 then 1 else fib (x - 1) + fib (x - 2)
#| <=>
#| fib = fix \f -> \x -> if x < 2 then 1 else f (x - 1) + f (x - 2)
#| ```
fix f = (\x -> f (\v -> x x v)) (\x -> f (\v -> x x v));

(||) x y = if x then x else y;
infixl 2 ||

(&&) x y = if x then y else x;
infixl 3 &&

(+) = _addNum;
infixl 6 +

(-) = _subNum;
infixl 6 -

(*) = _mulNum;
infixl 7 *

(/) = _divNum;
infixl 7 /

(//) = _divIntNum;
infixl 7 //

(%) = _modNum;
infixl 7 %

(<) = _ltNum;
infixl 4 <

(==) = _eq;
infixl 4 ==

(<=) x y = x < y || x == y;
infixl 4 <=
