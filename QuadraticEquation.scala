abstract class Numeric[T] {
  def add(a: T, b: T): T
  def sub(a: T, b: T): T
  def mul(a: T, b: T): T
  def div(a: T, b: T): T
  def sqrt(a: T): T
  def mulInt(a: Int, b: T): T
  def > (a: T, b: Int) : Boolean
}

object Numeric {
  implicit object FloatNumeric extends Numeric[Float] {
    def add(a: Float, b: Float): Float = a + b
    def sub(a: Float, b: Float): Float = a - b
    def mul(a: Float, b: Float): Float = a * b
    def div(a: Float, b: Float): Float = a / b
    def sqrt(a: Float): Float = Math.sqrt(a).toFloat
    def mulInt(a: Int, b: Float): Float = a * b
    def > (a: Float, b: Int): Boolean = a > b
  }

  implicit object DoubleNumeric extends Numeric[Double] {
    def add(a: Double, b: Double): Double = a + b
    def sub(a: Double, b: Double): Double = a - b
    def mul(a: Double, b: Double): Double = a * b
    def div(a: Double, b: Double): Double = a / b
    def sqrt(a: Double): Double = Math.sqrt(a)
    def mulInt(a: Int, b: Double): Double = a * b
    def > (a: Double, b: Int): Boolean = a > b
  }

  implicit object IntNumeric extends Numeric[Int] {
    def add(a: Int, b: Int): Int = a + b
    def sub(a: Int, b: Int): Int = a - b
    def mul(a: Int, b: Int): Int = a * b
    def div(a: Int, b: Int): Int = a / b
    def sqrt(a: Int): Int = Math.sqrt(a).toInt
    def mulInt(a: Int, b: Int): Int = a * b
    def > (a: Int, b: Int): Boolean = a > b
  }

  implicit object LongNumeric extends Numeric[Long] {
    def add(a: Long, b: Long): Long = a + b
    def sub(a: Long, b: Long): Long = a - b
    def mul(a: Long, b: Long): Long = a * b
    def div(a: Long, b: Long): Long = a / b
    def sqrt(a: Long): Long = Math.sqrt(a).toLong
    def mulInt(a: Int, b: Long): Long = a * b
    def > (a: Long, b: Int): Boolean = a > b
  }

  implicit object ComplexNumeric extends Numeric[Complex[Double]] {
    def add (a: Complex[Double], b: Complex[Double]): Complex[Double] = new Complex(a.re + b.re, a.im + b.im)
    def sub (a: Complex[Double], b: Complex[Double]): Complex[Double] = new Complex(a.re - b.re, a.im - b.im)
    def mul (a: Complex[Double], b: Complex[Double]): Complex[Double] = new Complex(a.re*b.re - a.im*b.im, a.im*b.re + a.re*b.im)
    def div (a: Complex[Double], b: Complex[Double]): Complex[Double] = {
      new Complex((a.re*b.re + a.im*b.im)/(b.re*b.re+b.im*b.im),
                  (a.im*b.re - a.re*b.im)/(b.re*b.re+b.im*b.im))
    }
    def sqrt(a: Complex[Double]): Complex[Double] = {
      if (a.re == 0 && a.im == 0) return new Complex(0,0)
      val dx = Math.abs(a.re)
      val dy = Math.abs(a.im)
      val dr = if (dx >= dy) dy/dx else dx/dy
      val dw = if (dx >= dy) Math.sqrt(dx) * Math.sqrt(0.5 * (1.0 + Math.sqrt(1 + dr * dr)))
                else Math.sqrt(dy) * Math.sqrt(0.5 * (dr + Math.sqrt(1 + dr * dr)))
      if (a.re >= 0) {
        new Complex(dw, a.im / (2 * dw))
      } else {
        val im = if (a.im > 0) dw else -dw
        new Complex(a.im / 2 * im, im)
      }
    }
    def mulInt(a: Int, b: Complex[Double]): Complex[Double] = new Complex(a*b.re, a*b.im)
    def > (a: Complex[Double], b: Int): Boolean = a.re > b
  }
}

class Complex[T] (r: T, i: T) (implicit num: Numeric[T]) {
  val re = r
  val im = i
  override def toString: String = {
    if (num.>(im, 0)) re + "+" + im + "i"
    else re + "" + im + "i"
  }
}

class QuadraticEquation[T] (val a: T, val b: T, val c: T) {
  def solve(implicit num : Numeric[T]): List[T] = {
    val discr = num.sub(num.mul(b, b), num.mul(num.mulInt(4,a),c))
    val x1 = num.div(num.add(num.mulInt(-1,b), num.sqrt(discr)), num.mulInt(2,a))
    val x2 = num.div(num.sub(num.mulInt(-1,b), num.sqrt(discr)), num.mulInt(2,a))
    List(x1, x2)
  }
}

object Main {
  def main(args: Array[String]) : Unit = {
    val eqFloat = new QuadraticEquation[Float](3, 5, -3)
    println("Float: " + eqFloat.solve)

    val eqDouble = new QuadraticEquation[Double](3, 5, -3)
    println("Double: " + eqDouble.solve)

    val eqInt = new QuadraticEquation[Int](3, 5, -3)
    println("Int: " + eqInt.solve)

    val eqLong = new QuadraticEquation[Long](3, 5, -3)
    println("Long: " + eqLong.solve)

    val eqComplex = new QuadraticEquation[Complex[Double]](new Complex(5,7), new Complex(-3,6), new Complex(5,5))
    println("Complex[Double]: " + eqComplex.solve)
  }
}