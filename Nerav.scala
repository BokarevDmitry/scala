class Nerav (aCoefs: List[Int], bCoef: Int) {
  val a = aCoefs
  val b = bCoef
}

class System (listArg: List[Nerav]) {
  val list = listArg
  def + (q: System) = new System(list ::: q.list)
  def / (i: Int) = new System (list.map(x => new Nerav(x.a.slice(0,i-1) ::: (0 :: x.a.slice(i, x.a.length)), x.b)))
  def check (x: List[Int]): Boolean = {
    val zipped =
      list
      .map(p => List(
        p.a
          .zip(x)
          .map(q => q._1*q._2)
          .sum,
        p.b
      ))
    //println(zipped)
    //println(zipped.filter(i => i.head == i.last))
    zipped.equals(zipped.filter(i => i.head == i.last))
  }

  override def toString: String = {
    this.list.map(x => x.a + " " + x.b).toString()
  }
}



object Main {
  def main(args: Array[String]) : Unit = {
    val ner = new Nerav(List(1,2,3), 10)
    val system1 = new System(List(ner))

    val ner2 = new Nerav(List(1,3,-5), -43)
    val system2 = new System(List(ner2))

    println("Система 1: " + system1)
    println("Система 2: " + system2)


    val sum = system1 + system2
    println("Система Cумма: " + sum)

    val posZero = 3
    val system3 = sum / posZero
    println("Система с нулём на " + posZero + " месте: " + system3)

    val x = List(2,-5,6)
    print("Проверка системы Сумма с вектором ответов х = {2, -5, 6}: ")
    println(sum check x)

    val x1 = List(1,1,1)
    print("Проверка системы Сумма с вектором ответов х = {1, -1, 1}: ")
    println(sum check x1)

  }
}