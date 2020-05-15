freq
====

Counts `[a-zA-Z]+` words in input and outputs frequency.

Uses Scala 2.13.x, [Cats-Effect][ce] and [FS2][fs2] streams for processing.

[ce]: https://github.com/typelevel/cats-effect
[fs2]: https://github.com/functional-streams-for-scala/fs2/

Running CLI
-----------
1. Run SBT: 
   ```bash
   $> sbt
   ```
    
2. From SBT console run app to get CLI help:
   ```bash
   freq(master)> run --help
   ```
   
3. From SBT console:
   ```bash
   freq(master)> run pg.txt out.txt
   ```
