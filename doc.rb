CLASSPATH = 'c:\projects\scala-utils\target\classes;C:\projects\scalacheck\target\classes;C:\projects\specs\target\classes;./lib/junit-3.8.1.jar'
Dir["./src/main/scala/scala/specs/**/*.scala"].each do |f|
  system("scaladoc -d ./api2 -linksource -doctitle Specs -windowtitle Specs -top Specs -cp " + CLASSPATH + " " + f + " > ./api2/doc.log")
end