# c12e-learning-fp

### Sessions:
* Monday, Mar 21, 2016: typeclasses
    
    files: typeclasses.scala 

* Wednesday, Mar 23, 2016: variance and ADTs

    files: variance.scala, and IList.scala (to show an ADT)

### Notes

* SBT
  * SBT Commands:
    * compile: compiles code in the directory the sbt file is in. (Not sure if it recurses through any other directories ... maybe src/main ...)
    * run: finds all the mains within scope and lets us select which one we want to run
  * Putting a `~` infront of an sbt command will attach a file system trigger to any one the the sbt commands and make the command restart if any of the files in the sbt directory change.
