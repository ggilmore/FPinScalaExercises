package Chapter3

/**
 * Created by gmgilmore on 5/10/15.
 */

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{ // `List` companion object. Contains functions for creating and working with lists.
def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((acc, element) => acc + element)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)((acc, element) => acc * element)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_,x)=> x
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop(restOfList: List[A], index: Int): List[A] = restOfList match {
      case Nil => Nil
      case Cons(_, tail) => {
        if (index == n) restOfList
        else loop(tail, index + 1)
      }
    }
    loop(l, n)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    def loop(restOfList: List[A]): List[A] = restOfList match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) loop(t) else restOfList
    }
    loop(l)
  }

  def init[A](l: List[A]): List[A] = {
    def loop(restOfList: List[A]): List[A] = {
      restOfList match {
        case Nil => Nil
        case Cons(h, t) => Cons(h, loop(t))
      }
    }
    loop(l)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, count) => count +1 )

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def loop(result:B, restOfList: List[A]):B = restOfList match {
      case Nil => result
      case Cons(h, t) => loop(f(result,h),t)
    }
    loop(z, l)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B]){case (elem, acc) => Cons(f(elem), acc)}

  def reverse[A](l: List[A]): List[A] = foldLeft(l: List[A], Nil:List[A]){case (acc, elem) => Cons(elem, acc)}

  def append2[A](list1: List[A], list2: List[A]) = foldRight(list1, list2){case (element, acc) => Cons(element, acc)}

  def flatten[A](listOfLists: List[List[A]]): List[A] = foldRight(listOfLists, Nil: List[A]){case (list, acc) =>
    append2(list, acc)
  }

  def addOne(list: List[Int]) : List[Int] = foldRight(list, Nil: List[Int])((elem, acc) => Cons(elem +1, acc))

  def dToString(list: List[Double]): String = foldLeft(list, ""){case (str, elem) => str + s"${elem.toString} "}

  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, Nil: List[A]){case(elem, acc) =>
    if (f(elem)) Cons(elem, acc)
    else acc
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil: List[B]){case (elem, acc) =>
    append2(f(elem), acc)}

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) Cons(a, Nil) else Nil)

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addElements(t1, t2))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(restOfList: List[A], restOfSubsequence: List[A]): Boolean = {
      restOfSubsequence match {
        case Nil => true
        case Cons(subHead, subTail) => {
          restOfList match {
            case Nil => false
            case Cons(supHead, supTail) =>{
              if (supHead == subHead) loop(supTail, subTail)
              else loop(supTail, sub)
            }
          }
        }
      }

    }
    loop(sup, sub)
  }

}

object Runner extends App{
  println(List.hasSubsequence(List(1,2,3,4,5,6),List(3,4,5)))
}


