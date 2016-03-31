import com.c12e.sbt.Build._


lazy val root =
  app("new_cogscale_proj")
    .project

// Boot strapper, whole point is to import scalaz, thus we use the builtin sequence instead of importing it.
// When you chage build file, reload
// We can compile - we want more proofs / logic at compile time

// scalaz github, these values will be there
// ~ <command>
// ~compile

// ~compile makes it watch filesystem.
// could have also ~run to rerun everytime a file changes
// Using fs as a trigger - do this
libraryDependencies ++=
  Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.7",
    "org.scalaz" %% "scalaz-concurrent" % "7.1.7"
  )
