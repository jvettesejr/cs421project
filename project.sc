import scala.annotation.tailrec

/** mytake */
def mytake[A](n: Int, l: List[A]): List[A] = (n, l) match {
  case (0, _) | (_, Nil) => Nil
  case (_, h :: t) => h :: mytake(n - 1, t)
}

mytake(4, List(2,4,56))
mytake(3, List())
mytake(1, List("hello", "world"))

/** mydrop */
def mydrop[A](n: Int, l: List[A]): List[A] = (n, l) match {
  case (0, _) | (_, Nil) => l
  case (_, _ :: t) => mydrop(n-1, t)
}

mydrop(3, List(2,4,56,7))
mydrop(3, List())
mydrop(1, List("hello", "world"))

/** rev */
def rev[A](l: List[A]): List[A] = {
  @tailrec
  def reverse(l: List[A], d:List[A]): List[A] = l match {
    case Nil => d
    case h :: t => reverse(t, h :: d)
  }
  reverse(l, Nil)
}

rev(List(1,2,3))
rev(List())
rev(List("hello", "world"))

/** app */
def app[A](l1: List[A], l2: List[A]): List[A] = l1 match {
  case Nil => l2
  case h1 :: t1 => h1 :: app(t1, l2)
}

app(List(), List(1,2,3))
app(List(1,2,3), List())
app(List(4,5), List(1,2,3))

/** inclist */
def inclist(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case h :: t => (h+1) :: inclist(t)
}

inclist(List(1,2,3,4))
inclist(List(-2,4,5,1))
inclist(List())

/** sumlist */
def sumlist(l: List[Int]): Int = l match {
  case Nil => 0
  case h :: t => h + sumlist(t)
}

sumlist(List())
sumlist(List(1,2,3))
sumlist(List(-3,2,5))

/** myzip */
def myzip[A, B](l1: List[A], l2: List[B]): List[(A, B)] = (l1, l2) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (h1 :: t1, h2 :: t2) => (h1, h2) :: myzip(t1, t2)
}

myzip(List(1,2,3), List())
myzip(List(), List(1,2,3))
myzip(List(1,2,3), List("hello", "world"))

/** addpairs */
def addpairs(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
case (_, Nil) => Nil
case (Nil, _) => Nil
case (h1 :: t1, h2 :: t2) => (h1+h2) :: addpairs(t1,t2)
}

addpairs(List(1,2,3), List())
addpairs(List(1,2,3), List(4,5,6))

/** ones */
lazy val ones: Stream[Int] = 1 #:: ones

ones take 1 foreach println
ones take 10 foreach println

/** nats */
lazy val nats: Stream[Int] = 0 #:: 1 #:: nats.zip(nats.tail).map {x => x._1 + 1}

nats take 1 foreach println
nats take 10 foreach println

/** fib */
lazy val fib: Stream[Int] =
  0 #:: 1 #:: fib.zip(fib.tail).map {x => x._1 + x._2}

fib take 5 foreach println
fib take 15 foreach println

/** add */
def add(i: Int, l: List[Int]): List[Int] = l match {
  case Nil => i :: Nil
  case h :: t => if (i == h) {
    h :: t
  }
  else if (i < h) {
    i :: h :: t
  }
  else {
    h :: add(i, t)
  }
}

add(3, List())
add(3, List(1,2))
add(3, List(1,3,5))


/** union */
def union(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
  case (xs, Nil) => xs
  case (Nil, ys) => ys
  case (h1 :: t1, h2 :: t2) => if (h1 == h2) {
    h1 :: union(t1, t2)
  }
  else if (h1 < h2) {
    h1 :: union(t1, (h2 :: t2))
  }
  else {
    h2 :: union((h1 :: t1), t2)
  }
}

union(List(), List())
union(List(1,2,3), List())
union(List(), List(1,2,3))
union(List(1,2,3), List(1,2,3))


/** intersect */
def intersect(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (h1 :: t1, h2 :: t2) => if (h1 == h2) {
    h1 :: intersect(t1, t2)
  }
  else if (h1 < h2) {
    intersect(t1, (h2 :: t2))
  }
  else {
    intersect((h1 :: t1), t2)
  }
}
intersect(List(), List(1,2,3))
intersect(List(1,2,3), List())
intersect(List(1,2,3), List(3,4,5))

/** inclist2 */
def inclist2(l: List[Int]): List[Int] = l.map(a => a+1)

inclist2(List(1,2,3,4))
inclist2(List(-2,4,5,1))
inclist2(List())

/** sumlist2 */
def sumlist2(l: List[Int]): Int = l.foldRight(0)(_ + _)

sumlist2(List())
sumlist2(List(1,2,3))
sumlist2(List(-3,2,5))
