#!/bin/env fshell
import stdlib/base;
import stdlib/string;

resetColor = "\ESC[0m\STX";

prefix = "";

userColor = "\ESC[38;2;255;0;0m\STX";

hostColor = "\ESC[38;2;255;0;0m\STX";

userHostSep = "\ESC[38;2;255;0;0m\STX@";

hostPwdSep = ":";

pwdColor = "\ESC[38;2;0;255;255m\STX";

postfix = resetColor ++ "$ ";

simplifyPath = replaceStr HOME "~";

prompt x = prefix ++ userColor ++ USER ++ resetColor ++ userHostSep ++ hostColor ++ stripNL {hostname}
    ++ resetColor ++ hostPwdSep ++ pwdColor ++ simplifyPath PWD ++ resetColor ++ postfix;
