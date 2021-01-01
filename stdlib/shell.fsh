#!/bin/env fshell
import stdlib/base;

(|) = _pipe;
infixr 2 |

# Used to find out wether a program succeeded or failed.
#
# This is useful when converting output from regular shell programs (i.e. test) to
# fshell booleans
#
# Example:
# ```
# $ success? (test "Something")
# True
# $ success? (test "")
# False
# ```
# Note: Other shells often have their own test buildins. fshell does not so
# it inherits the behaviour of /usr/bin/test and thus, behaviour may be different than
# in other shells.
success? prog = {prog} >> status == 0;

# TODO: `"-d"` -> `-d` (String -> Flag)
# TODO: handle wrong types
cd path = if (success? (test "-d" path)) then _cd path >> True else False;

# TODO: `"--color"` -> `--color`
ls = ls "--color";
