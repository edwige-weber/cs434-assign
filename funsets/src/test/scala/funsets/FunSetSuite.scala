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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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
   *   val s1 = singletonSet(1)
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
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val sPositiveNumbers = (x:Int) => x >= 0
    val sBetween1and5 = (x:Int) => x > 1 && x<5
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
      assert(contains(s1, 1), true)
    }
  }

  test("union contains all elements with singletons") {
    new TestSets {
      val sUnionSingletons = union(s1, s2)
      assert(contains(sUnionSingletons, 1), true)
      assert(contains(sUnionSingletons, 2), true)
      assert(!contains(sUnionSingletons, 3), true)
    }
  }
  test("union contains all elements with singleton and range") {
    new TestSets {
      val sUnionSingletonRange = union(s1, sBetween1and5)
      assert(contains(sUnionSingletonRange, 1), true)
      assert(contains(sUnionSingletonRange, 2), true)
      assert(contains(sUnionSingletonRange, 3), true)
      assert(contains(sUnionSingletonRange, 4), true)
      assert(!contains(sUnionSingletonRange, 5), true)
    }
  }
  test("union contains all elements with ranges") {
    new TestSets {
      val sUnionRanges = union(sPositiveNumbers, sBetween1and5)
      assert(contains(sUnionRanges, 1), true)
      assert(contains(sUnionRanges, 2), true)
      assert(contains(sUnionRanges, 3), true)
      assert(contains(sUnionRanges, 4), true)
      assert(contains(sUnionRanges, 5), true)
    }
  }

  test("intersection contains no element") {
    new TestSets {
      val setNull = intersect(s1, s2)
      assert(!contains(setNull, 1), true)
    }
  }
  test("intersection contains singleton") {
    new TestSets {
      val set2 = intersect(s1, sPositiveNumbers)
      assert(contains(set2, 2), true)
      assert(!contains(set2, 3), true)
    }
  }
  test("intersection contains all smaller range") {
    new TestSets {
      val setRange = intersect(sBetween1and5, sPositiveNumbers)
      assert(!contains(setRange, 1), true)
      assert(contains(setRange, 2), true)
      assert(contains(setRange, 3), true)
      assert(contains(setRange, 4), true)
      assert(!contains(setRange, 5), true)
    }
  }

  test("difference contains singleton") {
    new TestSets {
      val setSingleton = diff(s1, s2)
      assert(contains(setSingleton, 1), true)
      assert(!contains(setSingleton, 2), true)
    }
  }
  test("difference contains no element") {
    new TestSets {
      val setNull = diff(s1, sPositiveNumbers)
      assert(!contains(setNull, 0), true)
      assert(!contains(setNull, 1), true)
      assert(!contains(setNull, 2), true)
    }
  }
  test("difference contains all range but one") {
    new TestSets {
      val setRangeButOne = diff(sPositiveNumbers, s1)
      assert(contains(setRangeButOne, 0), true)
      assert(!contains(setRangeButOne, 1), true)
      assert(contains(setRangeButOne, 2), true)
    }
  }
  test("difference contains all range except umbers in small range") {
    new TestSets {
      val setRange = diff(sPositiveNumbers, sBetween1and5)
      assert(contains(setRange, 0), true)
      assert(contains(setRange, 1), true)
      assert(!contains(setRange, 2), true)
      assert(!contains(setRange, 3), true)
      assert(!contains(setRange, 4), true)
      assert(contains(setRange, 5), true)
    }
  }

  test("filter odd numbers from range") {
    new TestSets {
      val setFilterOddNumbers = filter(sPositiveNumbers, (x: Int) => x % 2 == 0)
      assert(contains(setFilterOddNumbers, 0), true)
      assert(!contains(setFilterOddNumbers, 1), true)
      assert(contains(setFilterOddNumbers, 2), true)
      assert(!contains(setFilterOddNumbers, 3), true)
      assert(contains(setFilterOddNumbers, 4), true)
    }
  }

  test("forall in singleton") {
    new TestSets {
      assert(forall(s1, (x: Int) => x % 2 != 0), true)
      assert(!forall(s2, (x: Int) => x % 2 != 0), true)
    }
  }
  test("forall in range") {
    new TestSets {
      assert(forall(sBetween1and5, (x: Int) => x <10), true)
      assert(!forall(sPositiveNumbers, (x: Int) => x <10), true)
    }
  }

  test("exists in singleton") {
    new TestSets {
      assert(exists(s1, (x: Int) => x % 2 != 0), true)
      assert(!exists(s2, (x: Int) => x % 2 != 0), true)
    }
  }
  test("exists in range ") {
    new TestSets {
      assert(exists(sBetween1and5, (x: Int) => x <10), true)
      assert(!exists(sBetween1and5, (x: Int) => x >10), true)
    }
  }

  test("map singleton") {
    new TestSets {
      assert(contains(map(s1, (x: Int) => x+1), 2), true)
      assert(!contains(map(s1, (x: Int) => x+1), 1), true)
    }
  }
  test("map range") {
    new TestSets {
      assert(contains(map(sBetween1and5, (x: Int) => -x), -2), true)
      assert(contains(map(sBetween1and5, (x: Int) => -x), -3), true)
      assert(contains(map(sBetween1and5, (x: Int) => -x), -4), true)
      assert(!contains(map(sBetween1and5, (x: Int) => -x), 2), true)
      assert(!contains(map(sBetween1and5, (x: Int) => -x), 3), true)
      assert(!contains(map(sBetween1and5, (x: Int) => -x), 4), true)
    }
  }
}
