import sbt._

class Project(info: ProjectInfo) extends ProguardProject(info) {
  val scalaSwing = "org.scala-lang" % "scala-swing" % "2.8.1"

  override def mainClass = Some("com.jcraft.dstream_client.DStreamClient")

/*
  def keepMainClass = """
-keepclasseswithmembers public class * {
    public static void main(java.lang.String[]);
}
-keepclassmembernames class * {
    void setImage(java.awt.Image);
}
-keepclassmembernames class * {
    int getOrientation();
}

"""
  override def proguardOptions = List(
    "-dontoptimize",
    "-dontobfuscate",
    proguardKeepLimitedSerializability,
    proguardKeepAllScala,
    "-keep class com.jcraft.dstream_client.*",
    "-keep interface scala.ScalaObject"
  )

  override def proguardDefaultArgs =
    "-dontwarn" :: 
    "-dontoptimize" :: 
    "-dontobfuscate" :: 
    keepMainClass  ::  
    proguardOptions
*/
}

