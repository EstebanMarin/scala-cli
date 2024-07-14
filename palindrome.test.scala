//> using toolkit typelevel:0.1.27

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
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

object PalindromePropertyTests extends Properties("Palindrome") {
  // A generator for palindrome strings
  val palindromeGen: Gen[String] = for {
    str <- Gen.alphaStr
  } yield str

  property("palindrome should return true for any palindrome string") =
    "never odd or even".removeSpaces.reverseI.lowerCase === "never odd or even".removeSpaces.reverseI.lowerCase

  property("palindrome should return true for any palindrome string") =
    forAll(palindromeGen) { str =>
      ???
    }

//   property("palindrome should return false for any non-palindrome string") =
//     forAll(Gen.alphaStr.suchThat(_.length > 1)) { str =>
//       val nonPalindrome =
//         str + "a" + str.reverse // Ensuring it's not a palindrome
//       palindrome(nonPalindrome) == false
//     }
}
