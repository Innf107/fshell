import stdlib/stdlib; # TODO: Should be implicit
import test/fshell/test;

testAll _ =
    _performProg (echo "[PARSER]")
    >>
    test "integers" (\_ ->
        assertTrue "num? 0" (\_ -> num? 0)
     :! assertTrue "num? 123456789" (\_ -> num? 123456789)
     :! assertTrue "num? (123456789)" (\_ -> num? (123456789))
     :! assertFalse "num? \"I\"" (\_ -> num? "I")
     :! assertFalse "num? \"\"" (\_ -> num? "")
    )
    >> test "floats" (\_ ->
        assertTrue "num? 0.0" (\_ -> num? "0.0")
     :! assertFalse "num? .1" (\_ -> num? ".1") # Maybe this should be allowed?
     :! assertTrue "A" (\_ -> False)
    );

