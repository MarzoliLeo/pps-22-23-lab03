package u03

import u03.Lists.List

object Assignment {

  //Task 1_a, svolto da solo.
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil(), _) => Nil()
    case (Cons(_, t), 1) => t
    case (Cons(_, t), n) => drop(t, n - 1)

  //Task 1_b, svolto da solo.
  def append[A](left: List[A], right: List[A]): List[A] = left match {
    case Nil() => right
    case Cons(h, t) => Cons(h, append(t, right))
  }

  //Task 1_c, svolto da solo.
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
    case (Cons(h, Nil()), f) => f(h)
    case (Cons(h, t), f) => append(f(h), flatMap(t)(f))

  //Task 1_d, svolto da solo.
  def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] = {
    flatMap(l1)(a => if (pred(a)) Cons(a, Nil()) else Nil())
  }

  def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = {
    flatMap(l)(a => Cons(mapper(a), Nil()))
  }

  //Task 2, svolto da solo.
  import u02.Optionals.Option.*
  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None()
    case Cons(h, Nil()) => Some(h)
    case Cons(h, t) =>
      val maxTail = max(t)
      maxTail match
        case None() => Some(h)
        case Some(maxVal) => if (maxVal > h) maxTail else Some(h)

  //Task 3, svolto da solo.
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    import List.*
    def listTeachersCourses(l: List[Person]): List[String] =
      flatMap(l)(_ match
        case Teacher(_, courses) => Cons(courses, Nil())
        case _ => Nil()
      )

  //Task 4, svolto da solo.
  def foldLeft[A, B](list: List[A], initialValue: B)(f: (B, A) => B): B = list match
    case Nil() => initialValue
    case Cons(head, tail) => foldLeft(tail, f(initialValue, head))(f)

  def foldRight[A, B](list: List[A], initialValue: B)(f: (A, B) => B): B = list match
    case Nil() => initialValue
    case Cons(head, tail) => f(head, foldRight(tail, initialValue)(f))

  //Task 5, svolto da solo.
  def drop[A](n: Int, stream: Stream[A]): Stream[A] = (n, stream) match
    case (m, _) if m <= 0 => stream
    case (_, Empty()) => Empty()
    case (m, Cons(_, tail)) => drop(m - 1, tail())

  //Task 6, svolto da solo.
  def constant[A](k: A): Stream[A] = Stream.cons(k, constant(k))

  //Task 7, svolto da solo.
  def fibs: Stream[Int] =
    def loop(a: Int, b: Int): Stream[Int] = Stream.cons(a, loop(b, a + b))
    loop(0, 1)
}
