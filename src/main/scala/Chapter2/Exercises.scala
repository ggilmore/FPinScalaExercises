package Chapter2

object Exercises extends App{
  def nthFib(n:Int):Int = {
    require(n >= 0)
    def loop(twoBefore:Int, oneBefore:Int, currentIndex:Int):Int = {
      if (currentIndex == 0) {
        println(twoBefore)
        twoBefore
      }
      else if (currentIndex == 1) {
        println(twoBefore)
        println(oneBefore)
        oneBefore
      }
      else {
        println(twoBefore)
        loop(oneBefore, twoBefore + oneBefore, currentIndex - 1)
      }
    }
    loop(0 , 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(restOfList:Array[A]):Boolean = {
      if (restOfList.length <= 1) true
      else{
          val head = restOfList.head
          val next = restOfList.tail.head
          if (ordered(head, next)) loop(restOfList.tail)
          else false
      }
    }
    loop(as)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b:B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }

  nthFib(9)

}
