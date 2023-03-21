package u03

import u02.Modules.Person.{Teacher, name}
import u02.Optionals.*
import u02.Optionals.Option.*


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
    def append[A](left: List[A], right: List[A]): List[A] = left match {
      case Nil() => right
      case Cons(h, t) => Cons(h, append(t, right))
    }

    //Task 1_c
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case (Cons(h, Nil()), f) => f(h)
      case (Cons(h, t), f) => append(f(h), flatMap(t)(f))

    //Task 1_d
    def filterWithFlatMap[A](l1: List[A])(pred: A => Boolean): List[A] = { flatMap(l1)(a => if (pred(a)) Cons(a, Nil()) else Nil()) }
    def mapWithFlatMap[A, B](l: List[A])(mapper: A => B): List[B] = { flatMap(l)(a => Cons(mapper(a), Nil())) }

    //Task 2
    import u02.Optionals.Option.*
    def max(l: List[Int]): Option[Int] = l match {
      case Nil() => None()
      case Cons(h, Nil()) => Some(h)
      case Cons(h, t) => {
        val maxTail = max(t)
        maxTail match {
          case None() => Some(h)
          case Some(maxVal) => if (maxVal > h) maxTail else Some(h)
        }
      }
    }

    //Task 3
    /*import u02.Modules.*
    def getTeacherCourses(persons: List[Person]): List[String] =
      val teachers = persons.filter(_.isInstanceOf[Teacher]).map(_.asInstanceOf[Teacher])
      val courses = teachers.map(_.course)
      courses*/

    //Task 4
    def foldLeft[A, B](list: List[A], initialValue: B)(f: (B, A) => B): B = list match
      case Nil() => initialValue
      case (head, tail) => foldLeft(tail, f(initialValue, head))(f)

    def foldRight[A, B](list: List[A], initialValue: B)(f: (A, B) => B): B = list match
      case Nil() => initialValue
      case (head, tail) => f(head, foldRight(tail, initialValue)(f))


  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
