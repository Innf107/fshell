import stdlib/stdlib; #TODO: Should be implicit
# TODO: replace 000 with ()

test class f = if empty (f 000)
    then _performProg (echo ("Tests passed for class " ++ class))
    else _performProg (echo -n ("~~~~~~~~~~Tests failed for class " ++ class ++ "~~~~~~~~~" ++ foldl (\a -> \x -> a ++ "    " ++ x ++ "\n") "\n" (f 000) ++ "\n"));

assertTrue msg v = if v 000 then [] else ["assertTrue failed for '" ++ msg ++ "'"];

assertFalse msg v = if not (v 000) then [] else ["assertFalse failed for '" ++ msg ++ "'"];

(:!) x y = x <> y;
infixr 1 :!

