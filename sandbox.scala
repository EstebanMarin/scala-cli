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
import scala.annotation.tailrec

// Exercise 1 Palindrome
extension (s: String)
  def reverseI: String =
    @tailrec
    def reverseRec(str: String, acc: String): String =
      if (str.isEmpty) acc
      else reverseRec(str.tail, str.head + acc)
    reverseRec(s, "")
  def removeSpaces: String = s.replaceAll("\\s+", "")
  def lowerCase: String = s.toLowerCase

def palindrome(s: String): Boolean =
  s.removeSpaces.lowerCase === s.removeSpaces.reverseI.lowerCase

// ❯ scala-cli repl sandbox.scala
// Compiling project (Scala 3.4.1, JVM (17))
// Compiled project (Scala 3.4.1, JVM (17))
// Welcome to Scala 3.4.1 (17.0.5, Java Java HotSpot(TM) 64-Bit Server VM).
// Type in expressions for evaluation. Or try :help.

// scala> palindrome("never odd or even".removeSpaces.reverseI)
// val res3: Boolean = true

// Exercise 2
// First Factorial

def firstFactorial(num: Int): Int =
  // making it tail recursive to avoid stack overflow
  // notice the overflow when using Int
  // best way to describe this type class is using Numbers Typeclass
  //   scala> firstFactorial(24)
  // val res1: Int = -775946240
  @tailrec
  def factorialHelper(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else factorialHelper(n - 1, n * acc)
  factorialHelper(num, 1)

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("Enter a string to check if it is a palindrome")
      readLine <- IO.readLine
      result <- IO.println(palindrome(readLine))

    // readFF <- IO.readLine
    // result <- IO.println(firstFactorial(readFF.toInt))
    yield ()
