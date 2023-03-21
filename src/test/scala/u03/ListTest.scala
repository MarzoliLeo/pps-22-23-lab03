package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  //Task 2_a
  @Test
  def testDropList() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1)) // Cons (20 , Cons (30 , Nil ()))
    assertEquals(Cons(30, Nil()), drop(l, 2)) // Cons (30 , Nil ())
    assertEquals(Nil(), drop(l, 5)) // Nil ()

  //Task 2_b
  @Test
  def testAppendList() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

  @Test
  def testFlatMapList() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(lst)(v => Cons(v + 1, Nil())))
    // Cons(11, Cons(21, Cons(31, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil()))))
  // Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))