import scala.util.Try

def list_equality(l1: List[Any], l2: List[Any]) =
    l1.size == l2.size && l1.containsSlice(l2)

/*
squared_numbers
    that removes all non-numbers from a polymorphic list and returns the resulting list of squared numbers
    e.g., squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil)
    should return List(1, 10000, 9.8596, List(100), (25,49)). Note that it recursively applies to substructures.
*/
def tryToInt(s: String) = Try(s.toInt).toOption
def square(x: Byte) = Try(x * x).toOption

def squared_numbers(l: List[Any]): List[Any] =
    // todo
    def square(x: Any): Option[Any] =
        x match {
            case x: (Byte) => Some(x*x)
            case x: (Short) => Some(x*x)
            case x: (Int) => Some(x*x)
            case x: (Long) => Some(x*x)
            case x: (Float) => Some(x*x)
            case x: (Double) => Some(x*x)
            case x: (Seq[Any]) => square_list(x)
            case x: Tuple => square_tuple(x)
            case _ => None
        }
    def square_list(x: Seq[Any]) =
        Some(squared_numbers(x.toList))
        
    def square_tuple(x: Tuple) =
        val tuple_list = squared_numbers(x.toList)
        (tuple_list.size) match {
            case 1 => Some((tuple_list(0)))
            case 2 => Some((tuple_list(0), tuple_list(1)))
            case 3 => Some((tuple_list(0), tuple_list(1), tuple_list(2)))
            case 4 => Some((tuple_list(0), tuple_list(1), tuple_list(2), tuple_list(3)))
            case 5 => Some((tuple_list(0), tuple_list(1), tuple_list(2), tuple_list(3), tuple_list(4)))
            case 6 => Some((tuple_list(0), tuple_list(1), tuple_list(2), tuple_list(3), tuple_list(4), tuple_list(5)))
            case _ => None
        }
        
    l match {
        case Nil => List()
        case h :: t => {
            square(h) match {
                case Some(v) => v :: squared_numbers(t)
                case None => squared_numbers(t)
            }
        }
    }

def squared_numbers_test() =
    assert(
        squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil)
        == List(1, 10000, 9.8596, List(100), (25,49))
    )

/*
intersect
    that given two generic lists
    returns a new list that is the intersection of the two lists
    (e.g., intersect(List(1,2,3,4,5), List(4,5,6,7,8)) should return List(4,5)).
*/
def intersect(l1: List[Any], l2: List[Any]): List[Any] =
    def intersect_acc(l1: List[Any], l2: List[Any], acc: List[Any]): List[Any] =
        l1 match {
            case Nil => acc
            case h :: t => (if l2.contains(h) then intersect_acc(t, l2, h :: acc) else intersect_acc(t, l2, acc))
        }
    
    intersect_acc(l1, l2, Nil).reverse

def intersect_test() =
    assert(list_equality(intersect(List(1,2,3,4,5), List(4,5,6,7,8)), List(4,5)) == true)
    assert(list_equality(intersect(List(1,2,3), List(4,5,6,7,8)), List(4,5)) == false)
    assert(list_equality(intersect(List(1,2,3), List(4,5,6,7,8)), List()) == true)

/*
symmetric_difference
    that given two lists
    returns a new list that is the symmetric difference of the two lists.
    For example symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)) should return List(1,2,3,6,7,8).
*/
def symmetric_difference(l1: List[Any], l2: List[Any]): List[Any] =
    def symmetric_difference_acc(l1: List[Any], l2: List[Any], acc: List[Any]): List[Any] =
        l1 match {
            case Nil => acc
            case h :: t => (if !l2.contains(h) then symmetric_difference_acc(t, l2, h :: acc) else symmetric_difference_acc(t, l2, acc))
        }
    
    symmetric_difference_acc(l1, l2, Nil).reverse ::: symmetric_difference_acc(l2, l1, Nil).reverse

def symmetric_difference_test() =
    assert(list_equality(symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)), List(1,2,3,6,7,8)) == true)
    assert(list_equality(symmetric_difference(List(1,2,3), List(4,5,6,7,8)), List(1,2,3,4,5,6,7,8)) == true)
    assert(list_equality(symmetric_difference(List(1,2,3), List(1,2,3)), List()) == true)



@main def m() = {
    squared_numbers_test()
    intersect_test()
    symmetric_difference_test()
    Console.println("All correct")
}