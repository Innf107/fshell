#!/usr/bin/env bash

set -e

if [ -z "$FSHELLPATH" ]; then
  FSHELLPATH=~/.fshell
fi

echo "[FSHELL INSTALL] Building and installing the executable..."
stack install
echo "[FSHELL INSTALL] Build successfull"

echo "[FSHELL INSTALL] Trying to create the \$FSHELLPATH directory at $FSHELLPATH"
mkdir -p $FSHELLPATH
echo "[FSHELL INSTALL] Successfully created the \$FSHELLPATH directory at $FSHELLPATH"

echo "[FSHELL INSTALL] Copying stdlib to \$FSHELLPATH/stdlib ($FSHELLPATH/stdlib)"
cp -r stdlib "$FSHELLPATH/stdlib"
echo "[FSHELL INSTALL] Successfully copied stdlib"

if fshell --help > /dev/null; then
  echo "[FSHELL INSTALL] Install successfull!"
else
    echo [FSHELL INSTALL] Install failed. Either something went wrong or the location of your executable \
    \(probably ~/.local/bin/fshell\) is not on your path.
fi;
