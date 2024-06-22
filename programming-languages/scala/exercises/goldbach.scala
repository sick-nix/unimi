/*
Goldbach's conjecture is one of the oldest unsolved problems in number theory and in all of mathematics. It states:
Every even integer greater than 2 is a Goldbach number, i.e., a number that can be expressed as the sum of two primes.
Expressing a given even number as a sum of two primes is called a Goldbach partition of the number. For example,
04 = 2 +  2           6 = 3 +  3           8 = 3 +  5
10 = 7 +  3          12 = 5 +  7          14 = 3 + 11
16 = 5 + 11          18 = 7 + 11          20 = 7 + 13

Write the following functions:
    goldbach(n) that returns a Goldbach partition for n
    goldbach_list(n,m) that returns a list of Goldbach partitions for the even numbers in the range (n,m).
 */

def list_equality(l1: List[Any], l2: List[Any]) =
  l1.size == l2.size && l1.containsSlice(l2)

def factors(n: Integer): List[Integer] =
  def f(n: Integer, i: Integer): List[Integer] =
    if (n == 1 || i > n) then List(1)
    else if n % i == 0 then i :: f(n / i, i)
    else f(n, i + 1)
  f(n, 2).sorted

def is_prime(n: Integer) = list_equality(factors(n), List(1, n))

def goldbach(n: Integer) =
  if is_prime(n) then List()
  else
    def goldbach_acc(m: Integer): List[Integer] =
      if m == 0 then List()
      else if is_prime(m) && is_prime(n - m) then List(m, n - m)
      else goldbach_acc(m - 1)
    goldbach_acc(n - 1).sorted

def goldbach_test() =
  assert(goldbach(4) == List(2, 2))
  assert(goldbach(6) == List(3, 3))
  assert(goldbach(8) == List(3, 5))
  assert(goldbach(10) == List(3, 7))
  assert(goldbach(12) == List(5, 7))
  assert(goldbach(14) == List(3, 11))
  assert(goldbach(16) == List(3, 13))
  assert(goldbach(18) == List(5, 13))
  assert(goldbach(20) == List(3, 17))

def goldbach_list(n: Integer, m: Integer) =
  def goldbach_inner(
      k: Integer,
      map: Map[Integer, List[Integer]]
  ): Map[Integer, List[Integer]] =
    val diff = m - k
    if diff == -1 then map
    else if diff % 2 != 0 then goldbach_inner(k + 1, map)
    else goldbach_inner(k + 1, map + (k -> goldbach(k)))
  goldbach_inner(n, Map())

def goldbach_list_test() =
  Console.println(goldbach_list(4, 20))

@main def n() = {
  goldbach_test()
  goldbach_list_test()
}
