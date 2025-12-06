import Path.FileMap
import java.nio.file.{ Files, Paths }
import java.nio.file.attribute.PosixFilePermissions
import Zip.createZipFile

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "2.16",
  scalaVersion := "2.13.18",
  javacOptions  ++= Seq("-source", "8", "-target",  "8"),
  scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature" ),
)

lazy val stage = taskKey[Unit]("Create distribution zip file")
lazy val sourceOther  = settingKey[File]("Other source file included in the package")

lazy val coltwi = (project in file("."))
  .settings(
    commonSettings,
    name        := "coltwi",
    description := "A scala implementation of the solo AI for Colonial Twilight",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    ),
    sourceOther := sourceDirectory.value / "other",
    Compile / resourceGenerators += Def.task {
      val versFile = (Compile / resourceManaged).value / "version"
      IO.write(versFile, version.value)
      Seq(versFile)
      }.taskValue,

    // Task to create the distribution zip file
    Compile / stage  := {
      val log = streams.value.log
      (loader / Compile / packageBin).value  // Depends on the loader package being built
      (Compile / packageBin).value  // Depends on the package being built
      def rebaseTo(directory: File)(origfile: File): Option[File] = {
        val mapper: FileMap = Path.flat(directory)
        mapper(origfile)
      }
      val pkgDir     = target.value / s"coltwi-${version.value}"
      val lib        = pkgDir / "lib"
      val loader_jar = (loader / Compile / packageBin / artifactPath).value
      val zipfile    = file(s"${pkgDir.getAbsolutePath}.zip")
      val jars       = (Compile / fullClasspathAsJars).value.files
      val others     = (sourceOther.value * "*").get
      val assets     = (others pair rebaseTo(pkgDir)) ++ (jars pair rebaseTo(lib))
      // Order zipfile entries alphabetically with directories first
      val zipOrdering: Ordering[File] = new Ordering[File] {
        def compare(x: File, y: File) = 
          (x.isDirectory, y.isDirectory) match {
            case (true, false) => -1
            case (false, true) => 1
            case _  => x.toString.compare(y.toString)
          }
      }
      log.info(s"Staging to $pkgDir ...")
      IO.delete(pkgDir)
      IO.createDirectory(lib)
      IO.copyFile(loader_jar, lib / loader_jar.getName)
      IO.copy(assets, CopyOptions().withOverwrite(true))
      IO.setPermissions(pkgDir / "coltwi", "rwxr-xr-x") // Make bash script executable
      val zipEntries = (pkgDir ** "*")
        .get
        .filter(f => f.getName != ".DS_Store") // Skip any pesky .DS_Store files created by Mac OS
        .sorted(zipOrdering)
        .map(f => (f, IO.relativize(target.value, f).get))
      createZipFile(zipEntries, zipfile)
    }
  )
  
  lazy val loader = (project in file("loader"))
    .settings(
      commonSettings,
      name        := "Loader",
      description := "Bootstrap loader",
      // Make loader.jar generic without version number so the fitl scripts can find it.
      (Compile / packageBin / artifactPath) := (Compile / target).value / "loader.jar"
    )
