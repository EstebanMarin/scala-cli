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

def runPalindrome(s: String) =
  IO(palindrome(s))
object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("Enter problem number")
      _ <- IO.println("1. Palindrome")
      _ <- IO.println("2. First Factorial")
      _ <- IO.println("3. Min Window Sbustring")
      readLine <- IO.readLine
      _ <- readLine match
        case "1" =>
          IO.println(
            "Enter a string to check if it is a palindrome"
          ) *> IO.readLine.flatMap { s =>
            runPalindrome(s)
          }

        case "2" =>
          IO.println("Enter a number to get the first factorial")
            *> IO.readLine.flatMap { s =>
              // should avoid type casting
              IO.println(firstFactorial(s.toInt))
            }
        case _ =>
          IO.println("Min Window Substring") *>
            IO.unit
    yield ()
