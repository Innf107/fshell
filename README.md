# fshell - a functional shell

## Installation
Prerequisites
- git
- [haskell stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
```
$ git clone https://github.com/Innf107/fshell
$ cd fshell
$ stack install
```
The `fshell` executable will be automatically installed into `$HOME/.local/bin/fshell`. 
Make sure this directory is on your `PATH` or copy the executable to a custom location that is on your `PATH`.

## Running fshell
fshell can simply be run by opening a terminal and running the `fshell` command as follows.
```
$ fshell
```
For more information and other ways to run the fshell (like running a script file) type
```
$ fshell --help
```

## Quick Start

On the surface, fshell looks and feels like most other shells (i.e. bash, fish, zsh,...).

```
$ echo Hi
Hi
$ echo $PWD | xargs printf "Current working directory: %s\n"
Current working directory: /home/prophet 
```

Once you get to scripting, that perception should have changed.
```
square x = x * x;

echo (square 5)

filesInCurrentDirectory = {ls -A}

foreach (unlines filesInCurrentDirectory) (\x -> {printf ("File: " ++ $x)})
``` 
Note how scripts use a different [parse mode]() than usage at the command prompt. If you wanted to execute the first statement at the command prompt, you would have to write it as `square x = $x * $x`.

## Language Reference
TBD

### Basic Syntax
TBD

### Parse Modes
TBD

## Features Under Consideration
TBD

## To Be Documented
TBD
