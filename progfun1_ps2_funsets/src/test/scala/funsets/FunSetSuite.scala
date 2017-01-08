package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    def positiveSet(x: Int) = true

    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersec contains mutual elements of two set") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s12_s23 = intersect(s12, s23)
      assert(contains(s12_s23, 2), "Set should contain 2")
      assert(!contains(s12_s23, 1), "Set should not contain 1")
      assert(!contains(s12_s23, 3), "Set should not contain 3")
    }
  }

  test("diff contains element that is in set1 but not in set2") {
    new TestSets {
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val s12_s23 = diff(s12, s23)
      assert(contains(s12_s23, 1), "Intersection should contain 1")
      assert(!contains(s12_s23, 2), "Intersection should not contain 2 ")
      assert(!contains(s12_s23, 3), "Intersection should not contain 3 ")
    }
  }

  test("Filter contains element that is in the set and pass the filter") {
    new TestSets {
      def negFilter(x: Int) = x < 0

      val nega_int_set = filter(positiveSet, negFilter)
      assert(contains(nega_int_set, -1), "Negative integer set should contains -1")
      assert(contains(nega_int_set, -100), "Negative integer set should contains -100")
      assert(!contains(nega_int_set, 0), "Negative integer set should not contains 0")
      assert(contains(nega_int_set, Int.MinValue))
      assert(!contains(nega_int_set, Int.MaxValue))
      assert(!contains(nega_int_set, 1))
    }
  }

  test("forall returns whether all bounded integers within `s` satisfy `p`") {
    new TestSets {
      val nega_int_set = filter(positiveSet, x => x < 0)
      assert(forall(nega_int_set, x => x < 0), "all int in negative int set must be negative")
      assert(forall(nega_int_set, x => x < 10), "all int in negative int set must be smaller than 10")
      assert(!forall(nega_int_set, x => x > -500), "all int in negative int set must not be > -500")
      assert(!forall(nega_int_set, x => x > -1000 && x < -10), "all int in negative int set must not be in range [-1000,-10]")
    }
  }

  test("exists returns whether exists an bounded integers within `s` satisfy `p`") {
    new TestSets {
      val nega_int_set = filter(positiveSet, x => x < 0)
      assert(exists(nega_int_set, x => x < 0), "all int in negative int set must exist one negative num")
      assert(exists(nega_int_set, x => x < 10), "all int in negative int set must exist one num smaller than 10")
      assert(exists(nega_int_set, x => x > -500), "all int in negative int set must exist one num  > -500")
      assert(exists(nega_int_set, x => x > -678 && x < -676), "all int in negative int set must not be in range (-678,-676)")
    }
  }

  test("map return a set transformed by applying `f` to each element of `s`") {
    def even_num_set(x: Int) = x > 0 && x % 2 == 0

    def even_num_squared = map(even_num_set, x => x * x)

    assert(contains(even_num_squared, 16), " set of squared even number should contain 16")
    assert(contains(even_num_squared, 4), " set of squared even number should contain 4")
    assert(contains(even_num_squared, 36), " set of squared even number should contain 36")
    assert(!contains(even_num_squared, 9), " set of squared even number should not contain 9")
    assert(!contains(even_num_squared, 0), " set of squared even number should not contain 0")
    assert(!contains(even_num_squared, -10), " set of squared even number should not contain -10")
  }

}
