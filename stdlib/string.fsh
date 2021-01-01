#!/bin/env fshell
import stdlib/base;
import stdlib/list;

# TODO: Overloads
(++) = _stringappend;
infixr 6 ++

# replaceStr :: String -> String -> String -> String
replaceStr from to str = _stringreplace from to str;

# stripNL :: String -> String
stripNL s = _mkString (init (_stringchars s));

