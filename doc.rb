CLASSPATH = 'c:\projects\scala-utils\target\classes;C:\projects\scalacheck\target\classes;C:\projects\specs\target\classes;./lib/junit-3.8.1.jar'
open("specsdoc.bat", "w") do |f|
  result = "scaladoc -d ./api2 -linksource -doctitle Specs -windowtitle Specs -top Specs -cp " + CLASSPATH + " "
  Dir["./src/main/scala/scala/specs/**"].each do |p|
    result += p + "/*.scala "
  end
  f.puts(result)
end