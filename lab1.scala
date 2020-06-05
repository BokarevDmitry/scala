object Main extends App{
  val head: List[Int] => Int = {
    case Nil => sys.error("nil head")
    case x :: xs => x
  }

  val tail: List[Int] => List[Int] = {
    case Nil => sys.error("nil tail")
    case x :: xs => xs
  }

  val length: List[Int] => Int = {
    case Nil => 0
    case x :: xs => 1 + length(xs)
  }

  val take: (Int, List[Int]) => List[Int] = {
    case (n, Nil) => Nil
    case (0, list) => Nil
    case (n, list) if n>length(list) => Nil
    case (n, list) => head(list) :: take(n-1, tail(list))
  }

  val reverse: List[Int] => List[Int] = {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  val takeHelp: (Int, List[Int]) => List[List[Int]] = {
    case (a, Nil) => Nil
    case (a, list) if a>length(list) => Nil
    case (a, list) => List(take(a, list)) ::: takeHelp(a, tail(list))
  }

  val takeAll: (Int, List[Int]) => List[List[Int]] = {
    case (a, Nil) => Nil
    case (a, list) if a>length(list) => Nil
    case (a, list) => takeHelp(a, list) ::: takeAll(a+1, list)
  }

  val isPalindrom: List[Int] => Boolean = {
    list => list == reverse(list)
  }

  val filterPalindromes: List[List[Int]] => List[List[Int]] = {
    case Nil => Nil
    case (x :: xs) if isPalindrom(x) => x :: filterPalindromes(xs)
    case (x :: xs) => filterPalindromes(xs)
  }

  val palindromes: List[Int] => List[List[Int]] = {
    case Nil => Nil
    case list => filterPalindromes(takeAll(2, list))
  }

  println(palindromes(List(5,2,5,5,2,5)))
  println(palindromes(List(1,2,3,3,2,1)))
  println(palindromes(List(1,2,3,4,2,4)))
}