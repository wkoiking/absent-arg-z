# Oops!  Entered absent arg Arg: z

## Summary

GHC 9.x.x (including 9.4.4) causes below error while GHC 8.10.7 do not.

~~~
test-exe.exe: internal error: Oops!  Entered absent arg Arg: z
Type: [(TrackID, VentilationSectionID)]
In module `SP6.Data.Layout'
    (GHC version 9.4.4 for x86_64_unknown_mingw32)
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
~~~

Steps to reproduce

~~~
% git clone https://github.com/wkoiking/absent-arg-z.git
% cd absent-arg-z
% stack test
~~~

Without optimization, this error seems to be avoided. i.e., below will not cause the error.

~~~
% stack clean
% stack test --fast
~~~

## Expected behavior

The run time error should not occur.

## Environment

GHC version used: GHC 9.4.4

Optional:

* Operating System: Windows 10 Home (10.0.19044 build 19044)
* System Architecture: x86_64
