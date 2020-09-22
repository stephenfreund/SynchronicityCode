---
title:  'Installing Anchor From Source'
author:
- Cormac Flanagan (UC, Santa Cruz)
- Stephen Freund (Williams College)
---

These instructions install the version of Anchor presented in our
paper.  We have used them on computers running either Mac OSX 10.15 or
Ubuntu 16.  You may need to make some adjustments, depending on your
operating system and/or local environment.

1. Install JDK 8 --- [https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html](https://www.oracle.com/java/technologies/javase/javase-jdk8-downloads.html)

2. Install scala 2.12.8 --- [https://www.scala-lang.org/download/2.12.8.html](https://www.scala-lang.org/download/2.12.8.html)

3. Install Mono --- [https://www.mono-project.com/download/stable/](https://www.mono-project.com/download/stable/)

4. Get our code from this artifact (or github if a more recent version is available).

5. Get Boogie:
   ```diff
   > cd Synchronicity/workspace/Synchronicity/
   > git clone https://github.com/boogie-org/boogie.git
   > cd boogie/
   > git checkout 3b7fc31f4ef3f8efc70c812e374c01384509b7f2
   > wget https://dist.nuget.org/win-x86-commandline/latest/nuget.exe
   > mono ./nuget.exe restore Source/Boogie.sln
   > xbuild Source/Boogie.sln
   ```

6. Get Z3:

   * Download from [https://github.com/Z3Prover/z3/releases/tag/z3-4.8.6](https://github.com/Z3Prover/z3/releases/tag/z3-4.8.6)
   * Install it in `Synchronicity/workspace/Synchronicity/z3/`

7. Link Z3 to where Boogie expects it:

    ```diff
    > cd Synchronicity/workspace/Synchronicity/boogie/
    > ln -s `pwd`/../z3/bin/z3 Binaries/z3
    ```

8. Ensure Boogie and Z3 work:

    ```diff
    > cd Synchronicity/workspace/Synchronicity/
    > ./bpl
    > ./z3/bin/z3
    ```

   If you get errors about z3 being corrupted on OS X, go to the
   Security System Preferences and say it's ok to run that program.

9. Test Anchor:

    ```diff
    > cd Synchronicity/workspace/Synchronicity/
    > make
    > source msetup
    > anchor tests/ok1.anchor
    ```

10. Run unit Tests (about 15 minutes):

    ```diff
    > cd Synchronicity/workspace/Synchronicity/
    > make tests
    ```

11. Run Benchmark Tests:

    ```diff
    > cd Synchronicity/workspace/Synchronicity/
    > # verify each a single time
    > ./anchor-benchmarks
    ```
