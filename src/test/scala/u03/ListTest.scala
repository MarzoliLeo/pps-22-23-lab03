package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*


class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  //Test task 2_a
  @Test
  def testDropList() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1)) // Cons (20 , Cons (30 , Nil ()))
    assertEquals(Cons(30, Nil()), drop(l, 2)) // Cons (30 , Nil ())
    assertEquals(Nil(), drop(l, 5)) // Nil ()

  //Test task 2_b
  @Test
  def testAppendList() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

  //Test task 1_c
  @Test
  def testFlatMapList() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    // Cons(11, Cons(21, Cons(31, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    // Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))

  //Test task 1_d
  @Test
  def testFilterWithFlatMap() =
    val lfilter = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
    assertEquals(Cons(2, Cons(4, Nil())), filterWithFlatMap(lfilter)(_ % 2 == 0))

  @Test
  def testMapWithFlatMap() =
    val lmap = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
    assertEquals(Cons(2, Cons(4, Cons(6, Cons(8, Nil())))), mapWithFlatMap(lmap)(_ * 2))

  //Test task 2
  @Test
  def testMax() =
    import u02.Optionals.Option.*
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil()))))) // Some (25)
    assertEquals(None(), max(Nil())) // None ()
    
  //Test Task 3
  import u03.Lists.List.Person.*
  val student = Student("Leo", 22)
  val teacher = Teacher("Pluto", "Dama")
  
  @Test def testIfNoTeachers() =
    assertEquals(Nil(), listTeachersCourses(Cons(student, Nil())))

  @Test def testFindTeacherCourses() =
    assertEquals(Cons("Dama", Cons("Dama", Cons("Dama", Nil()))), listTeachersCourses(Cons(teacher, Cons(teacher, Cons(teacher, Cons(student, Nil()))))))

  //Test task 4
  @Test
  def testFoldLeft() =
    assertEquals(60, foldLeft(l, 0)(_ + _))

  @Test
  def testFoldRight() =
    assertEquals(-60, foldRight(l, 0)(_ - _))

  //Test task 5.
  @Test def testDropStream() =
    import u03.Streams.Stream.*
    val s = take(iterate(0)(_ + 1))(10)
    assertEquals(Cons (6 , Cons (7 , Cons (8 , Cons (9 , Nil ())))), toList(drop(6,s))) // = > Cons (6 , Cons (7 , Cons (8 , Cons (9 , Nil ()))))

  //Test task 6.
  @Test def testConstantStream() =
    import u03.Streams.Stream.*
    assertEquals(Cons ("x", Cons ("x", Cons ("x", Cons ("x", Cons ("x", Nil ()))))), toList (take ( constant ("x") ) (5) ))
    // = > Cons (x, Cons (x, Cons (x, Cons (x, Cons (x, Nil ())))))

  //Test task 7.
  @Test def testFibs() =
    import u03.Streams.Stream.*
    type fibs = Stream[Int]
    val fibs2 = fibs
    assertEquals ( Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Cons (5 , Cons (8 , Cons (13 , Nil ())))))))), toList ( take ( fibs2 ) (8) ))
    // = > Cons (0 , Cons (1 , Cons (1 , Cons (2 , Cons (3 , Cons (5 , Cons (8 , Cons (13 , Nil ()))))))))
