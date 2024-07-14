//> using toolkit typelevel:0.1.27
//> org.scalatest::scalatest::3.2.17

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

object PalindromeProperty extends Properties("Palindrome") {
  // A generator for random strings
  val stringGenerator: Gen[String] = for {
    str <- Gen.alphaStr
  } yield str

  // A generator for palindrome strings
  val palindromeGen: Gen[String] = for {
    str <- Gen.alphaStr
    isPalindrome <- Gen.oneOf(true, false)
    palindrome = if (isPalindrome) str + str.reverse else str
  } yield palindrome

  property("palindrome should return true for any palindrome string") =
    "never odd or even".removeSpaces.reverseI.lowerCase === "never odd or even".removeSpaces.reverseI.lowerCase

  property("palindrome should return true for any palindrome string") =
    forAll(palindromeGen) { str =>
      palindrome(str) == true
    }

}
