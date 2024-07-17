// Scala Exercises
// Uncomment each exercise, then write the appropriate code to remove the errors
// and pass the tests. Consider writing your own tests as well.

/*
// 1. Complete the following skeleton to define a function that adds up a list of Ints:

def total(nums: List[Int]): Int = nums match {
  case Nil => // TODO
  case head :: tail => // TODO
}

test("total of empty list") {
  total(Nil) should equal(0)
}

test("total of sample list") {
  total(3 :: 1 :: 4 :: 1 :: 6 :: Nil) should equal(15)
}

test("total of sample list, short form") {
  total(List(3, 1, 4, 1, 6)) should equal(15)
}
*/

/*
// 2. Write a function doubleAll that takes a list of Ints and returns a list
// where each item has been doubled.

def doubleAll(nums: List[Int]): List[Int] = // TODO
    
test("doubleAll of empty list") {
  doubleAll(List()) should equal(List())
}

test("doubleAll of sample list") {
  doubleAll(List(3, 1, 4, 1, 6)) should equal(List(6, 2, 8, 2, 12))
}
*/

/*
// 3. Write a function named filterOdd that takes a list of Ints and returns a
// list of only the odd items from the list.

test("filterOdd of empty list") {
  filterOdd(List()) should equal(List())
}

test("filterOdd of sample list") {
  filterOdd(List(3, 1, 4, 1, 6)) should equal(List(3, 1, 1))
}
*/

/*
// 4. Write a function with signature isLowerBound(n: Int, nums: List[Int]): Boolean
// that returns true if n is less than or equal to all of the numbers in nums.

test("isLowerBound of empty list") {
  isLowerBound(1, List()) should equal(true)
}

test("isLowerBound of sample list") {
  isLowerBound(1, List(3, 1, 4, 1, 6)) should equal(true)
  isLowerBound(2, List(3, 1, 4, 1, 6)) should equal(false)
}
*/

/*
// 5. Write a function with signature insert(n: Int, nums: List[Int]): List[Int]
// that returns the list nums with n inserted into the correct sorted position
// (assuming nums was sorted).

test("insert into empty list") {
  insert(3, List()) should equal(List(3))
}

test("insert into sample list") {
  insert(3, List(1, 4, 6)) should equal(List(1, 3, 4, 6))
}
*/

/*
// 6. Use the insert function to write an insertion sort.

test("insSort of empty list") {
  insSort(List()) should equal(List())
}

test("insSort of sample list") {
  insSort(List(3, 1, 4, 1, 6)) should equal(List(1, 1, 3, 4, 6))
}
*/

/*
// 7. Write a function with signature splitMin(nums: List[Int]): (Int, List[Int])
// that returns a pair consisting of the smallest item from nums and a list of
// all of the remaining values from nums.
// If splitMin is called on an empty list, you should call the error function to
// throw an exception: error("splitMin of an empty list").

test("splitMin of empty list") {
  val ex = evaluating { splitMin(List()) } should produce [RuntimeException]
  ex.getMessage should equal("splitMin of an empty list")
}

test("splitMin of sample list") {
  splitMin(List(3, 1, 4, 1, 6)) should (
    equal((1, List(3, 4, 1, 6))) or
    equal((1, List(3, 1, 4, 6)))
  )
}
*/

/*
// 8. Use the splitMin function to write a selection sort.

test("selSort of empty list") {
  selSort(List()) should equal(List())
}

test("selSort of sample list") {
  selSort(List(3, 1, 4, 1, 6)) should equal(List(1, 1, 3, 4, 6))
}
*/

/*
// 9. Write a "higher-order" function (that is, a function that takes another
// function as an argument) with signature
// applyAll(nums: List[Int], fun: Int => Int): List[Int] that returns a list
// where each of the items from nums has had fun applied to it.

// For testing: applyAll(nums, double) should be the same as doubleAll(nums) 
def double(n: Int): Int = 2 * n

test("applyAll of empty list") {
  applyAll(List(), double) should equal(List())
}

test("applyAll of sample list") {
  applyAll(List(3, 1, 4, 1, 6), double) should equal(List(6, 2, 8, 2, 12))
}
*/

/*
// 10. Write a higher-order function with signature
// filterPred(nums: List[Int], pred: Int => Boolean): List[Int] that returns a list
// of only those items from nums for which pred (the "predicate") returns true.

// For testing: filterPred(nums, odd) should be the same as filterOdd(nums)
def odd(n: Int): Boolean = n % 2 == 1

test("filterPred of empty list") {
  filterPred(List(), odd) should equal(List())
}

test("filterPred of sample list") {
  filterPred(List(3, 1, 4, 1, 6), odd) should equal(List(3, 1, 1))
}
*/
