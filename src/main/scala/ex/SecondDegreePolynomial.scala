// Esercizio svolto da solo.
package ex

import scala.annotation.targetName

// Express a second degree polynomial
// Structure: secondDegree * X^2 + firstDegree * X + constant
trait SecondDegreePolynomial:
  def constant: Double
  def firstDegree: Double
  def secondDegree: Double
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial
  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial

class SecondDegreePolynomialImpl(override val secondDegree: Double, override val firstDegree: Double, 
                                  override val constant: Double) extends SecondDegreePolynomial:
    override def +(polynomial: SecondDegreePolynomial) = SecondDegreePolynomial(secondDegree + polynomial.secondDegree, 
                                                                                firstDegree + polynomial.firstDegree,
                                                                                constant + polynomial.constant)
    override def -(polynomial: SecondDegreePolynomial) = SecondDegreePolynomial(secondDegree - polynomial.secondDegree, 
                                                                                firstDegree - polynomial.firstDegree,
                                                                                constant - polynomial.constant)
    
object SecondDegreePolynomial:
  def apply(secondDegree: Double, firstDegree: Double, constant: Double): SecondDegreePolynomial = SecondDegreePolynomialImpl(secondDegree, firstDegree, constant)

case class SecondDegreePolynomialImplWithCaseClass(secondDegree: Double, firstDegree: Double, constant: Double) extends SecondDegreePolynomial:
    def +(polynomial: SecondDegreePolynomial) = SecondDegreePolynomial(secondDegree + polynomial.secondDegree, 
                                                                                firstDegree + polynomial.firstDegree,
                                                                                constant + polynomial.constant)
    def -(polynomial: SecondDegreePolynomial) = SecondDegreePolynomial(secondDegree - polynomial.secondDegree, 
                                                                                firstDegree - polynomial.firstDegree,
                                                                                constant - polynomial.constant)
@main def checkComplex(): Unit =
  val simplePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)
  val sum = simplePolynomial + anotherPolynomial
  println((sum, sum.secondDegree, sum.firstDegree, sum.constant)) // 1.0 * X^2 + 1.0 * X + 3.0
  val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
  println((multipleOperations, multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant)) // 2.0 * X^2 + 1.0 * X + 2.0

  val fuzzyPolynomialWithoutCaseClass = SecondDegreePolynomial(1.0, 0, 3)
  val copyOfFuzzyPolynomialWithoutCaseClass = SecondDegreePolynomial(1.0, 0, 3)
  println("Check equals() doesn't work without case class: " + copyOfFuzzyPolynomialWithoutCaseClass.equals(fuzzyPolynomialWithoutCaseClass))
  println("Check toString() doesn't work without case class: " + fuzzyPolynomialWithoutCaseClass)

  val fuzzyPolynomialWithCaseClass = SecondDegreePolynomialImplWithCaseClass(1.0, 0, 3)
  val copyOfFuzzyPolynomialWithCaseClass = SecondDegreePolynomialImplWithCaseClass(1.0, 0, 3)
  println("Check equals() works with case class: " + copyOfFuzzyPolynomialWithCaseClass.equals(fuzzyPolynomialWithCaseClass))
  println("Check toString() works with case class: " + fuzzyPolynomialWithCaseClass)
  


