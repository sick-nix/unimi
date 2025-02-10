/*
is_palindrome: string → bool
    that checks if the string given as input is palindrome
    a string is palindrome when the represented sentence can be read the same way in either directions
    in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...;
 */
def is_palindrome(str: String): Boolean =
  val reg = """[a-zA-Z]""".r
  val s =
    str.toLowerCase().split("").toList.filter((x) => reg.matches(x)).mkString
  s == s.reverse

def is_palindrome_test(): Unit =
  assert(true == is_palindrome(""))
  assert(true == is_palindrome("a"))
  assert(true == is_palindrome("aa"))
  assert(false == is_palindrome("abb"))
  assert(true == is_palindrome("aba"))
  assert(true == is_palindrome("Aba"))
  assert(true == is_palindrome("AbCdcba"))
  assert(true == is_palindrome("Madam I'm Adam"))

/*
is_an_anagram : string → string list → boolean
    that given a dictionary of strings
    checks if the input string is an anagram of one or more of the strings in the dictionary;
 */
def is_an_anagram(str: String, dict: List[String]): Boolean =
  val p = (a: String, b: String) => a.charAt(0).toLower < b.charAt(0).toLower
  val og_str = str.toLowerCase().split("").sortWith(p).mkString
  dict.exists((s) => s.toLowerCase().split("").sortWith(p).mkString == og_str)

def is_an_anagram_test() =
  assert(true == is_an_anagram("cat", List("act", "tac")))
  assert(false == is_an_anagram("cat", List("test", "abcde")))

/*
factors: int → int list
    that given a number calculates all its prime factors;
 */
def factors(n: Integer): List[Integer] =
  def f(n: Integer, i: Integer): List[Integer] =
    if (n == 1 || i > n) then List(1)
    else if n % i == 0 then i :: f(n / i, i)
    else f(n, i + 1)
  f(n, 2)

def factors_test() =
  assert(List(1, 2, 3).sorted == factors(6).sorted)
  assert(List(1, 3, 3, 3).sorted == factors(27).sorted)
  assert(List(1, 7).sorted == factors(7).sorted)
  assert(List(1, 2, 2, 7).sorted == factors(28).sorted)
  assert(List(1, 2, 2, 2, 2, 2, 2, 2).sorted == factors(128).sorted)
/*
is_proper: int → boolean
    that given a number calculates if it is a perfect number or not
    where a perfect number is a positive integer equal to the sum of its proper positive divisors
    (excluding itself), e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;
 */
def sum(l: List[Integer]): Integer = {
  if (l.isEmpty) 0
  else l.head + sum(l.tail)
}

def is_proper(n: Integer): Boolean =
  def f(num: Integer, i: Integer): List[Integer] =
    if i > num / 2 then List(1)
    else if num % i == 0 then i :: f(n, i + 1)
    else f(n, i + 1)
  val divisors = f(n, 2)
  n == sum(divisors)

def is_proper_test() =
  assert(true == is_proper(6))
  assert(false == is_proper(7))
  assert(true == is_proper(28))
  assert(true == is_proper(496))
  assert(false == is_proper(69))
  assert(true == is_proper(8128))

@main def main() = {
  is_palindrome_test()
  is_an_anagram_test()
  factors_test()
  is_proper_test()
  Console.println("All correct")
}
