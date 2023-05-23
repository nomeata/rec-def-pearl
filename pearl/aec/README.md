# ICFP 2023 Artifact

Name: Functional Pearl: More fixpoints!

## Artifact Instructions

### Dependencies

The main artifact is a Haskell library in the form of a Cabal package. It has
no dependencies besides the GHC library; only the test suite has extra
dependencies. The virtual image has GHC and these dependencies installed; if
you want to use a different system, install `ghc` and `cabal` as usual (on
Debian, run `sudo apt install ghc cabal-install`).

For the following steps I assume you are in the home directory of the virtual
machine, or if you are not using the virtual machine, that you have installed
ghc and cabal, unpacked the artifact tarball, and entered the directory
therein.

In the virtual machine, the shell and GHCI history is populated with the
commands below, so just press ↑ if you want to avoid typing.

### Building

To see if the artifacts builds, run

    cd rec-def-0.2.1;
    cabal build

The expected output is

    artifact@artifact:~/rec-def-0.2.1$ cabal build
    Config file path source is default config file.
    Config file /home/artifact/.cabal/config not found.
    Writing default configuration to /home/artifact/.cabal/config
    Resolving dependencies...
    Build profile: -w ghc-8.8.4 -O1
    In order, the following will be built (use -v for more details):
     - rec-def-0.2.1 (lib) (first run)
    Configuring library for rec-def-0.2.1..
    Preprocessing library for rec-def-0.2.1..
    Building library for rec-def-0.2.1..
    [ 1 of 12] Compiling Data.POrder      ( Data/POrder.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/POrder.o )
    [ 2 of 12] Compiling Data.Propagator.Class ( Data/Propagator/Class.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Propagator/Class.o )
    [ 3 of 12] Compiling Data.Propagator.Naive ( Data/Propagator/Naive.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Propagator/Naive.o )
    [ 4 of 12] Compiling Data.Propagator.P2 ( Data/Propagator/P2.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Propagator/P2.o )
    [ 5 of 12] Compiling System.IO.RecThunk ( System/IO/RecThunk.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/System/IO/RecThunk.o )
    [ 6 of 12] Compiling Data.Propagator.Purify ( Data/Propagator/Purify.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Propagator/Purify.o )
    [ 7 of 12] Compiling Data.Recursive.Internal ( Data/Recursive/Internal.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/Internal.o )
    [ 8 of 12] Compiling Data.Recursive.Set ( Data/Recursive/Set.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/Set.o )
    [ 9 of 12] Compiling Data.Recursive.Map ( Data/Recursive/Map.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/Map.o )
    [10 of 12] Compiling Data.Recursive.DualBool ( Data/Recursive/DualBool.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/DualBool.o )
    [11 of 12] Compiling Data.Recursive.Bool ( Data/Recursive/Bool.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/Bool.o )
    [12 of 12] Compiling Data.Recursive.Examples ( Data/Recursive/Examples.hs, /home/artifact/rec-def-0.2.1/dist-newstyle/build/x86_64-linux/ghc-8.8.4/rec-def-0.2.1/build/Data/Recursive/Examples.o )


### Running the test suite

The artifact comes with a test suite. To run this, use

    cd rec-def-0.2.1
    cabal build --write-ghc-environment-files=always
    cabal test

Unless you are using the provided virtal image, this download and build additional dependencies, which will take a few minutes.

The first build step is needed to work around issues runing doctest tests with cabal. Try that step if you encounter this error:

    Test suite doctest: RUNNING...
    doctest: <command line>: cannot satisfy -package QuickCheck
        (use -v for more information)

The sources for the three test suites are in `rec-def-0.2.1/tests/`:

 * `dejafu`: Tests for deadlocks or non-determinism due to concurrency. Takes a few minutes.
 * `doctest`: Tests all the doctests in the documentation
 * `spaceleak`: A test that the space leak mitigation (section 4.3.) works

### Accessing the documentation

The library is comprehensively documented. The documentation is included in the
source tarball under `docs/index.html`.

The module `Data.Recursive.Examples` has some small examples.


### Interactive experimentation

You can use GHC’s REPL to play around with `rec-def` and try some of the
examples from the paper. To do so, run

    cd rec-def-0.2.1
    ghci Data/Recursive/Examples.hs

You should now see

    artifact@artifact:~/rec-def-0.2.1$ ghci Data/Recursive/Examples.hs
    GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
    Loaded package environment from /home/artifact/rec-def-0.2.1/.ghc.environment.x86_64-linux-8.8.4
    [ 1 of 11] Compiling Data.POrder      ( Data/POrder.hs, interpreted )
    [ 2 of 11] Compiling Data.Propagator.Class ( Data/Propagator/Class.hs, interpreted )
    [ 3 of 11] Compiling Data.Propagator.Naive ( Data/Propagator/Naive.hs, interpreted )
    [ 4 of 11] Compiling Data.Propagator.P2 ( Data/Propagator/P2.hs, interpreted )
    [ 5 of 11] Compiling System.IO.RecThunk ( System/IO/RecThunk.hs, interpreted )
    [ 6 of 11] Compiling Data.Propagator.Purify ( Data/Propagator/Purify.hs, interpreted )
    [ 7 of 11] Compiling Data.Recursive.Internal ( Data/Recursive/Internal.hs, interpreted )
    [ 8 of 11] Compiling Data.Recursive.Set ( Data/Recursive/Set.hs, interpreted )
    [ 9 of 11] Compiling Data.Recursive.DualBool ( Data/Recursive/DualBool.hs, interpreted )
    [10 of 11] Compiling Data.Recursive.Bool ( Data/Recursive/Bool.hs, interpreted )
    [11 of 11] Compiling Data.Recursive.Examples ( Data/Recursive/Examples.hs, interpreted )
    Ok, 11 modules loaded.
    *Data.Recursive.Examples>

Conveniently, by loading the example file, its qualified imports are now in scope, in particular
`RS`, `RB` and `RDB`. You can now run the examples from the paper, for example
from Section 2.2:

    *Data.Recursive.Examples> let x = RS.insert 42 x in RS.get x
    fromList [42]

You can also load additional files to experiment with more complex code.

The file `trans.hs` conatins the example from the introduction, with the naive
`rTrans1` function and the `rec-def`-using function `rTrans2`. Running
`rTrans2` on the second example graph will not terminate, and you'll have to
abort using Ctrl-C:

    ghci> :load ../trans.hs
    …
    Ok, 9 modules loaded.
   ghci> rTrans1 example1
   fromList [(1,[1,3]),(2,[1,2,3]),(3,[3])]
   ghci> rTrans1 example2
   fromList [(1,^CInterrupted.
   ghci> rTrans2 example1
   fromList [(1,[1,3]),(2,[1,2,3]),(3,[3])]
   ghci> rTrans2 example2
   fromList [(1,[1,2,3]),(2,[1,2,3]),(3,[3])]


The file `program-anal.hs` contains the example from Section 3 with some
example input. Again, you can see how the original code (`canThrow`) fails to
process `example4` (and you have to abort it using Ctrl-C), but the variant
using `rec-def` (`canThrow2`) succeeds:

    ghci> :load ../program-anal.hs
    …
    Ok, 9 modules loaded.
    *Main> canThrow example4
    ^CInterrupted.
    *Main> canThrow2 example4
    True

The files `dominators.hs` and `minesweeper.hs` contain further example without
comments; they accompany blog posts that you can google for, if you want (but
this will deanonymize the authors).

### Building the artifact VM

This section is mostly for the author's future reference, and for anyone
curious about it, and document how the artifact vitual image was created.

This creations has been scripted using the `build-aec-image.sh` script, which
installs packages and copies file into the base image provided by the AEC. Then
the `prepare-aec-image.sh` script is run inside the image. This also creates
the documentation, which is then copied out again.

The `build-aec-image.sh` also creates the artifact source tarball.

_Everything below has been copied from the base image README._

## QEMU Instructions

The ICFP 2023 Artifact Evaluation Process is using a Debian QEMU image as a
base for artifacts. The Artifact Evaluation Committee (AEC) will verify that
this image works on their own machines before distributing it to authors.
Authors are encouraged to extend the provided image instead of creating their
own. If it is not practical for authors to use the provided image then please
contact the AEC co-chairs before submission.

QEMU is a hosted virtual machine monitor that can emulate a host processor
via dynamic binary translation. On common host platforms QEMU can also use
a host provided virtualization layer, which is faster than dynamic binary
translation.

QEMU homepage: https://www.qemu.org/

### Installation

#### OSX
``brew install qemu``

#### Debian and Ubuntu Linux
``apt-get install qemu-kvm``

On x86 laptops and server machines you may need to enable the
"Intel Virtualization Technology" setting in your BIOS, as some manufacturers
leave this disabled by default. See Debugging.md for details.


#### Arch Linux

``pacman -Sy qemu``

See the [Arch wiki](https://wiki.archlinux.org/title/QEMU) for more info.

See Debugging.md if you have problems logging into the artifact via SSH.


#### Windows 10

Download and install QEMU via the links at

https://www.qemu.org/download/#windows.

Ensure that `qemu-system-x86_64.exe` is in your path.

Start Bar -> Search -> "Windows Features"
          -> enable "Hyper-V" and "Windows Hypervisor Platform".

Restart your computer.

#### Windows 8

See Debugging.md for Windows 8 install instructions.

### Startup

The base artifact provides a `start.sh` script to start the VM on unix-like
systems and `start.bat` for Windows. Running this script will open a graphical
console on the host machine, and create a virtualized network interface.
On Linux you may need to run with `sudo` to start the VM. If the VM does not
start then check `Debugging.md`

Once the VM has started you can login to the guest system from the host.
Whenever you are asked for a password, the answer is `password`. The default
username is `artifact`.

```
$ ssh -p 5555 artifact@localhost
```

You can also copy files to and from the host using scp.

```
$ scp -P 5555 artifact@localhost:somefile .
```

### Shutdown

To shutdown the guest system cleanly, login to it via ssh and use

```
$ sudo shutdown now
```

### Artifact Preparation

Authors should install software dependencies into the VM image as needed,
preferably via the standard Debian package manager. For example, to install
GHC and cabal-install, login to the host and type:

```
$ sudo apt update
$ sudo apt install ghc
$ sudo apt install cabal-install
```

If you really need a GUI then you can install X as follows, but we prefer
console-only artifacts whenever possible.

```
$ sudo apt install xorg
$ sudo apt install xfce4   # or some other window manager
$ startx
```

See Debugging.md for advice on resolving other potential problems.

If your artifact needs lots of memory you may need to increase the value
of the `QEMU_MEM_MB` variable in the `start.sh` script.

When preparing your artifact, please also follow the [Submission
Guidelines](https://icfp23.sigplan.org/track/icfp-2023-artifact-evaluation#Submission-Guidelines).
