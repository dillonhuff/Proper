# Proper

Propositional logic theorem prover in Haskell

Proper is also available as a hackage. The Hackage
contains a library for propositional logic as well
as executables for parsing and checking theorems
in propositional logic. See the example files in
the top level directory for details.

# Installation

At the command line:

```bash
bash-3.2$ git clone https://github.com/dillonhuff/Proper.git
Cloning into 'Proper'...
remote: Counting objects: 317, done.        
remote: Total 317 (delta 0), reused 0 (delta 0), pack-reused 317        
Receiving objects: 100% (317/317), 40.66 KiB | 0 bytes/s, done.
Resolving deltas: 100% (161/161), done.
Checking connectivity... done.
bash-3.2$ cd Proper/
bash-3.2$ ls
DemorganThm.txt		ExampleTheoremFile.txt	LICENSE			README.md		src
EmptyThm.txt		FalseThm.txt		Proper.cabal		Setup.hs		test
bash-3.2$ cabal install
Warning: The package list for 'hackage.haskell.org' is 158.7 days old.
Run 'cabal update' to get the latest list of available packages.
Resolving dependencies...
Configuring Proper-0.5.2.2...
Building Proper-0.5.2.2...
Installed Proper-0.5.2.2
Updating documentation index
/Users/dillon/Library/Haskell/share/doc/x86_64-osx-ghc-7.10.3/index.html
bash-3.2$ ls
DemorganThm.txt		ExampleTheoremFile.txt	LICENSE			README.md		dist			test
EmptyThm.txt		FalseThm.txt		Proper.cabal		Setup.hs		src
bash-3.2$ ./dist/build/Proper-tests/Proper-tests
Cases: 11  Tried: 11  Errors: 0  Failures: 0
Cases: 9  Tried: 9  Errors: 0  Failures: 0
Cases: 14  Tried: 14  Errors: 0  Failures: 0
Cases: 7  Tried: 7  Errors: 0  Failures: 0
Cases: 3  Tried: 3  Errors: 0  Failures: 0
Cases: 8  Tried: 8  Errors: 0  Failures: 0
Cases: 12  Tried: 12  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
Cases: 4  Tried: 4  Errors: 0  Failures: 0
bash-3.2$ ./dist/build/Proper/Proper FalseThm.txt 
THEOREM
("a" <-> "b")
(~("c") | (~("d") & "a"))

|=

("a" | "b")

is False
bash-3.2$ ./dist/build/Proper/Proper DemorganThm.txt 
THEOREM
("a" <-> "f")
(~("c") -> ("a" & "l"))
~(~(~("c")))

|=

(~(("l" | ~("f"))) <-> (~("l") & "f"))

is True
```

# Using Proper as a library from within a Haskell program

After installing Proper, use put the following code in Main.hs:

```haskell
import Proper.Formula

main :: IO ()
main =
  let a = val "a"
      thm = theorem [neg $ neg a] a in
   do
     putStrLn $ show thm
     putStrLn $ "is true ? " ++ (show $ checkTheorem thm)

```

This code should print out:

```bash
bash-3.2$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> :l Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*Main> main
THEOREM
~(~("a"))

|=

"a"
is true ? True
```