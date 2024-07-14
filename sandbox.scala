// Main.scala
//> using toolkit typelevel:0.1.27
import cats.effect.*
import io.circe.Decoder
import fs2.Stream
import fs2.io.file.*
import org.http4s.ember.client.*
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.circe.*
import cats.syntax.all.*

extension (s: String)
  def reverseI: String = s.reverse
  def removeSpaces: String = s.replaceAll("\\s+", "")
  def lowerCase: String = s.toLowerCase

def palindrome(s: String): Boolean =
  s.removeSpaces.lowerCase === s.removeSpaces.reverseI.lowerCase

// step 1 test the initial implementation
// ❯ scala-cli repl sandbox.scala
// Compiling project (Scala 3.4.1, JVM (17))
// Compiled project (Scala 3.4.1, JVM (17))
// Welcome to Scala 3.4.1 (17.0.5, Java Java HotSpot(TM) 64-Bit Server VM).
// Type in expressions for evaluation. Or try :help.

// scala> palindrome("never odd or even".removeSpaces.reverseI)
// val res3: Boolean = true
object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("Enter a string to check if it is a palindrome")
      readLine <- IO.readLine
      result <- IO.println(palindrome(readLine))
    yield result
