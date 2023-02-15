# hyggec - the Didactic Compiler for Hygge

This is the source code `hyggec`, the didactic compiler for the Hygge
programming language.

Hygge and `hyggec` have been designed and developed as learning tools for the
course *02247 Compiler Construction* at DTU Compute - Technical University of
Denmark.

`hyggec` is released under the terms of the [MIT license](LICENSE.md).

## Software Requirements

  * .NET 6.0 (for compiling and running `hyggec`)
    - On Ubuntu and Debian GNU/Linux: `apt install dotnet6`
    - On MacOS: `brew install dotnet@6`
    - On Windows: <https://dotnet.microsoft.com/en-us/download>
 
  * Java Runtime Environment, preferably version 17 (for running the included
    copy of [RARS, the RISC-V Assembler and Runtime Simulator](https://github.com/TheThirdOne/rars))
    - On Ubuntu and Debian GNU/Linux: `apt install openjdk-17-jre`
    - On MacOS: `brew install openjdk@17`
    - On Windows: <https://www.oracle.com/java/technologies/javase/jdk17-archive-downloads.html>

**NOTE:** you will need to have both the `dotnet` and `java` programs in your
[executable PATH](https://janelbrandon.medium.com/understanding-the-path-variable-6eae0936e976) (their installation scripts should
take care of it).

## Quick Start

After installing the required software above, open a terminal in the root
directory of the `hyggec` source tree, and try:

```
./hyggec test
```

This command automatically builds `hyggec` and runs its test suite. If you don't
see any error, then `hyggec` was built correctly and passed all its tests.  You
should now be able to use and modify it.

To see the usage options, you can execute:

```
./hyggec help
```

You will see a list of various commands.  To get usage options for a specific
command (for example, `compile`):

```
./hyggec compile --help
```

Here's something you can try:

```
./hyggec interpret --typecheck --verbose examples/hygge0-spec-example.hyg
```

## Building `hyggec` from the Command Line

Every time you invoke the script `./hyggec`, the compiler will be rebuilt if its
source code was modified since the last execution.

You can also (re)build the `hyggec` executable by running:

```
dotnet build
```

To clean up the results of a build, you can run:

```
dotnet clean
```

## Recommended Visual Studio Code Extensions

These Visual Studio Code extensions are very helpful when working on `hyggec`:

  * [Ionide for F#](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp)
  * [FSharp fsl and fsy](https://marketplace.visualstudio.com/items?itemName=mnxn.fsharp-fsl-fsy)
