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
  // tail recursive reverse homegrown implementation
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

// scala> palindrome("never odd or even")
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

// Exercise 3
// Min Window Substring
type BetterDataDescription = (String, String)
def minWindowSubString(entry: BetterDataDescription): Set[String] =
  val (main, subString) = entry
  val mainLength = main.length
  val subStringLength = subString.length
  val subStringDomainSearch: Set[String] =
    // create a unique permutation of the substring to clearly identify the domain of search
    // scala> "aed".permutations.permutations.toSet
    // val res6: List[String] = List(aed, ade, ead, eda, dae, dea)
    println(subString.permutations.toSet)
    subString.permutations.toSet
  val mainDomainSearch: Set[String] =
    println(main.sliding(subStringLength).toSet)
    main.sliding(subStringLength).toSet

// ❯ scala-cli sandbox.scala
// Compiling project (Scala 3.4.1, JVM (17))
// Warning: there was 1 deprecation warning; re-run with -deprecation for details
// Compiled project (Scala 3.4.1, JVM (17))
// Enter problem number
// 1. Palindrome
// 2. First Factorial
// 3. Min Window Sbustring
// 3
// Min Window Substring
// Enter the main string
// aaabaaddae
// Enter the substring
// aed
// HashSet(ade, eda, dae, ead, dea, aed)
// HashSet(aab, dda, aba, dae, aad, baa, aaa, add)
// Min Window Substring: HashSet(dae)

  mainDomainSearch.intersect(subStringDomainSearch)

object Main extends IOApp.Simple:
  def run: IO[Unit] =
    for
      _ <- IO.println("Enter problem number")
      _ <- IO.println("1. Palindrome")
      _ <- IO.println("2. First Factorial")
      _ <- IO.println("3. Min Window Substring")
      readLine <- IO.readLine
      _ <- readLine match
        case "1" =>
          IO.println(
            "Enter a string to check if it is a palindrome"
          ) *> IO.readLine.flatMap { s =>
            IO(palindrome(s))
              .flatMap { result =>
                IO.println(s"Is Palindrome: $result")
              }
          }

        case "2" =>
          IO.println("Enter a number to get the first factorial")
            *> IO.readLine.flatMap { s =>
              // should avoid type casting
              // provide type class with smart constructors
              IO(firstFactorial(s.toInt)).flatMap(result =>
                IO.println(s"First Factorial: $result")
              )
            }
        case _ =>
          IO.println("Min Window Substring") *> {
            for
              _ <- IO.println("Enter the main string")
              main <- IO.readLine
              _ <- IO.println("Enter the substring")
              subString <- IO.readLine
              _ <- IO.println(
                s"Min Window Substring: ${minWindowSubString((main, subString))}"
              )
            yield ()
          }
    yield ()
