package Chapter3

/**
 * Created by gmgilmore on 5/11/15.
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => size(l) + size (r)
    case Leaf(v) => 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(combine: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => combine(fold(l)(f)(combine), fold(r)(f)(combine))
  }

  def sizeAsFold[A](tree: Tree[A]): Int = fold(tree)(x => 1)((b1, b2) => b1 + b2)

  def maximumAsFold(tree: Tree[Int]): Int  = fold(tree)(x => x)((b1,b2) => b1 max b2)

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((b1, b2) => 1 + b1 + b2)

  def mapAsFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l,r))


}

object TreeRunner extends App {
  println(Tree.depth2(Branch(Leaf(4), Branch(Branch(Leaf(1), Leaf(2)), Leaf(6)))))
}