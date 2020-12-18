#!/bin/env fshell

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

# map :: (a -> b) -> [a] -> [b]
map f xs = if empty xs
    then xs
    else (f (head xs)) : (map f (tail xs));

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

