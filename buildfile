require "buildr"
require "scala"

GROUP = "scala"
VERSION_NUMBER = "1.0.0"
NEXT_VERSION = "1.0.1"
COPYRIGHT = "Eric Torreborre"
repositories.remote << "http://www.ibiblio.org/maven2/"
#repositories.release_to << "http://www.ibiblio.org/maven2/"

LIBRARIES = ["junit:junit:jar:3.8.1", "scala:utils:jar:1.0.0", "C:/tools/develop/scala/lib/scala-library.jar"]

desc "The Specs project provides a framework for behaviour-driven development in Scala"
define "specs" do
  project.version = VERSION_NUMBER
  project.group = GROUP
  manifest["Implementation-Vendor"] = COPYRIGHT

  scalac.with(LIBRARIES)
  scalac_test.with(LIBRARIES)
  test.with(LIBRARIES).include("*Suite$")
  test.with(LIBRARIES).exclude("*sample*", "*RunnerSuite$")
  package(:jar)
end
