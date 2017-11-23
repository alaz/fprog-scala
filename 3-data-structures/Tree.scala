sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// exercise 3.25: size = the total number of leaves + branches
def size[A](tree: Tree[A]): Int =
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

// exercise 3.26: maximum = the maximum element
def maximum(tree: Tree[Int]): Int =
  tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

// exercise 3.27: depth
def depth[A](tree: Tree[A]): Int =
  tree match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

// exercise 3.28: map
def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
  tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

// exercise 3.29: fold; reimplement size, maximum, depth, map
def fold[A,B](tree: Tree[A])(leaf: A => B)(branch: (B,B) => B): B =
  tree match {
    case Leaf(x) => leaf(x)
    case Branch(l,r) => branch(fold(l)(leaf)(branch), fold(r)(leaf)(branch))
  }

def size_1[A](tree: Tree[A]): Int =
  fold(tree) {_ => 1 } { (l,r) => 1+l+r }
def maximum_1(tree: Tree[Int]) =
  fold(tree) { x => x } { (l,r) => l max r }
def depth_1[A](tree: Tree[A]): Int =
  fold(tree) { _ => 1 } { (l,r) => 1 + (l max r) }
def map_1[A,B](tree: Tree[A])(f: A => B): Tree[B] =
  fold[A,Tree[B]](tree) { x => Leaf(f(x)) } { (l,r) => Branch(l,r) }
