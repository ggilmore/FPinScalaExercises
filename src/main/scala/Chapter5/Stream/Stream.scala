package Chapter5.Stream

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case Empty => None
    case s=> Some((s, s drop 1))
  }

  def scanRight[Z](base: Z)(f: (A,  => Z) => Z): Stream[Z] = this.foldRight(cons(base, empty)){case (a, Cons(h, t)) => cons(f(a, h()), cons(h(), t()))}

  def startsWith[B>:A](s: Stream[B]): Boolean = (this zipAll s) takeWhile (_._2.isDefined)  forAll {case (a,b) => a ==b}

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, bs)){
    case (Empty, Empty) => None
    case (Cons(h,t), Empty)=> Some(((Some(h()), None), (t(), empty)))
    case (Empty, Cons(h,t))=> Some(((None, Some(h())), (empty, t())))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
  }


  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().take(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty){ (x,y) =>
    if (p(x)) cons(x, y)
    else empty
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(elem, next) => Some((elem(), next()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)(p(_) && _)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())w
  }

  def headOption_1: Option[A] = this.foldRight[Option[A]](None)((a, opt) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = this.foldRight[Stream[B]](empty)((a,b) => cons(f(a), b))

  def map_unfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def filter(f: A => Boolean): Stream[A] = this.foldRight[Stream[A]](empty){ (a,b) =>
    if (f(a)) cons(a, b)
    else b
  }

  def append[B>:A](s: Stream[B]): Stream[B] = this.foldRight(s)((a, stream) => cons(a, stream))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](empty){case (a, stream) =>
    f(a) append stream
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
  def toList: List[A] = {
    def build(current: Stream[A], list: List[A]): List[A] = current match {
      case Empty => list.reverse
      case Cons(h,t) => build(t(), h()::list)
    }
    build(this, List())
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def constant_unfold[A](a: A): Stream[A] = unfold(a)(Some(_, a))

  val ones: Stream[Int] = Stream.cons(1, ones)

  val ones_unfold: Stream[Int] = unfold(1)(one => Some((one, one)))

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def from_unfold(n: Int): Stream[Int] = unfold(n)(num => Some((num, num+1)))

  def zipWithUnfold[A,B,C](as: Stream[A], bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((as, bs)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def fibs: Stream[Int] = {
    def loop(one: Int, two: Int): Stream[Int] =
      cons(one, loop(two, one + two))
    loop(0, 1)
  }

  def fibs_unfold = unfold((0, 1)){case (f0, f1) => Some((f0, (f1, f0 + f1)))}


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(a: A, next: S) => cons(a, unfold(next)(f))
    case None => empty
  }
}