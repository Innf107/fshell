#!/bin/env fshell
import stdlib/base;

# TODO: Overloads

# empty :: [a] -> Bool
empty = _isnillist;

# head :: [a] -> a | not . empty
head = _headlist;

# tail :: [a] -> [a] | not . empty
tail = _taillist;

# (:) :: a -> [a] -> [a]
(:) = _conslist;
infixr 5 :

# length :: [a] -> Num
length xs = if empty xs
    then 0
    else 1 + length (tail xs);

# (<>) :: [a] -> [a] -> [a]
infixr 6 <>
(<>) xs ys = if (empty xs)
    then ys
    else head xs : (tail xs <> ys);

# map :: (a -> b) -> [a] -> [b]
map f xs = if empty xs
    then xs
    else (f (head xs)) : (map f (tail xs));

# foreach :: [a] -> (a -> b) -> [b]
foreach = flip map;

# foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z xs = if empty xs
    then z
    else f (foldr f (head xs) z) (tail xs);

# foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z xs = if empty xs
    then z
    else foldl f (f z (head xs)) (tail xs);

# reverse :: [a] -> [a]
reverse = foldl (flip (_conslist)) [];  # TODO: replace _conslist by (:) when operator sections are implemented

# drop :: Num -> [a] -> [a]
drop n list = if empty list || n <= 0
    then list
    else drop (n - 1) (tail list);

take n list = if n <= 0 || empty list
    then []
    else head list : (take (n - 1) (tail list));

# isPrefix :: [a] -> [a] -> Bool
isPrefix prefix list = if empty prefix
    then True
    else if empty list
        then False
        else if head prefix == head list
            then isPrefix (tail prefix) (tail list)
            else False;


# replace :: [a] -> [a] -> [a] -> [a]
replace from to list = if empty list || empty from
    then list
    else if isPrefix from list
        then to <> drop (length from) list
        else head list : replace from to (tail list);

# init :: [a] -> [a]
init xs = take (length xs - 1) xs;
