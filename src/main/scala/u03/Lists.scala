package u03

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    //Task 1_a
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (Cons(_, t), 1) => t
      case (Cons(_, t), n) => drop(t, n - 1)

    //Task 1_b
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Cons(h, Nil()), right) => Cons(h, right)
      case (Cons(_, _), Nil()) => left
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1, append(t1, Cons(h2, t2)))

    //Task 1_c
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case (Cons(h, Nil()), f) => f(h)
      case (Cons(h, t), f) => append(f(h), flatMap(t)(f))


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
