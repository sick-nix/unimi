/*
You have to write a class Matrix to represent and manipulate integer matrices.
The class should be parametric on its dimensions.
Override the appropriate operators and raise the appropriate exceptions.

The following gives the semantics of such operations.
Let aij denote the i,j-th element of matrix A, located at row i, column j.
Using this notation, the matrix operations listed above may be defined precisely as follows:
    matrix equivalence A ≃ B where A in Zm×n, B in Zp×q when m = p and n = q and bij = aij for i = 1,...,m j = 1,...,n;
    matrix copy B = A where A, B in Zm×n (bij = aij for i = 1,...,m j = 1,...,n);
    matrix addition C = A + B where A, B, C in Zm×n (cij = aij + bij for i = 1,...,m j = 1,...,n);
    scalar-matrix multiplication B = aA where A, B in Zm×n, a in Z (bij = a·aij for i = 1,...,m j = 1,...,n);
    matrix-matrix multiplication C = A·B where A in Zm×p, B in Zp×n, C in Zm×n
        (cij = Σk=1, ..., p aik·bkj for i = 1,...,m j = 1,...,n);
    matrix transposition B = AT where A in Zm×n, B in Zn×m (bji = aij for i = 1,...,m j = 1,...,n);
    matrix norm (matrix 1-norm) a = ‖A‖ where a in Z, A in Zm×n (a = maxj Σi | aij | for i = 1,...,m j = 1,...,n).
    Note that in each case we state the proper matrix dimensions for the operation to be valid.
        For example, when multiplying two matrices A and B, the number of columns of A must match the number of rows of B.
 */

class Matrix(
    n: Int,
    m: Int,
    values: List[List[Int]]
) {
  def nrows = n
  def ncolumns = m
  def items = values
  def nth_opt(r: Int, c: Int) =
    try {
      Some(items(r)(c))
    } catch {
      case e: IndexOutOfBoundsException => None
    }
  def nth(r: Int, c: Int, default: Int = 0) = nth_opt(r, c) match {
    case Some(v) => v
    case None    => default
  }

  def equals(mat: Matrix) = Matrix.equals(this, mat)

  def transpose = Matrix.transpose(this)

  def copy = Matrix.copy(this)

  def add(m: Matrix) = Matrix.add(this, m)

  def smul(num: Int) = Matrix.smul(this, num)

  def mmul(m: Matrix) = Matrix.mmul(this, m)

  def norm = Matrix.norm(this)

  override def toString(): String =
    def print_list(l: List[Int], s: String): String = l match {
      case Nil => s
      case h :: t =>
        print_list(t, s + "\t" + h)
    }
    def print_matrix(l: List[List[Int]], s: String): String = l match {
      case Nil => s
      case h :: t =>
        print_matrix(t, s + "\n" + print_list(h, ""))
    }
    print_matrix(items, "")
}

object Matrix {
  def apply(n: Int, m: Int): Matrix =
    Matrix(n, m, (i, j, n, m) => 0)

  def apply(
      n: Int,
      m: Int,
      fn: (i: Int, j: Int, n: Int, m: Int) => Int
  ): Matrix =
    Matrix(n, m, Matrix.make_items(n, m, fn))

  def apply(n: Int, m: Int, values: List[List[Int]]) =
    new Matrix(n, m, values)

  def make_items(
      n: Int,
      m: Int,
      fn: (i: Int, j: Int, n: Int, m: Int) => Int
  ): List[List[Int]] =
    def make_list_acc(r: Int, c: Int): List[Int] =
      if c == m then List()
      else fn(r, c, n, m) :: make_list_acc(r, c + 1)
    def make_list(r: Int): List[Int] = make_list_acc(r, 0)
    def make_matrix_acc(r: Int): List[List[Int]] =
      if r == n then List()
      else make_list(r) :: make_matrix_acc(r + 1)
    make_matrix_acc(0)

  def equals(mat1: Matrix, mat2: Matrix) = mat1.items == mat2.items

  def transpose(mat: Matrix) =
    Matrix(mat.ncolumns, mat.nrows, (i, j, n, m) => mat.nth(j, i))

  def copy(mat: Matrix) =
    Matrix(mat.nrows, mat.ncolumns, (i, j, n, m) => mat.nth(i, j))

  def add(mat1: Matrix, mat2: Matrix) =
    if mat1.nrows == mat2.nrows && mat1.ncolumns == mat2.ncolumns then
      Matrix(
        mat1.nrows,
        mat1.ncolumns,
        (i, j, n, m) => mat1.nth(i, j) + mat2.nth(i, j)
      )
    else
      throw new Exception(
        "Matrixes should have same number or rows and columns for addition"
      )

  def smul(mat: Matrix, num: Int) =
    Matrix(mat.nrows, mat.ncolumns, (i, j, n, m) => mat.nth(i, j) * num)

  def mmul(mat1: Matrix, mat2: Matrix) =
    val m = mat1.nrows
    val n1 = mat1.ncolumns
    val n2 = mat2.nrows
    val p = mat2.ncolumns
    if n1 == n2 then
      def sum_ij(i: Int, j: Int) =
        def sum_ij_acc(k: Int, acc: Int): Int =
          if k == n1 then acc
          else sum_ij_acc(k + 1, acc + mat1.nth(i, k) + mat2.nth(k, j))
        sum_ij_acc(0, 0)
      Matrix(m, p, (i, j, rows, cols) => sum_ij(i, j))
    else
      throw new Exception(
        "Matrix multiplication requires the second matrix to have the same number of columns" +
          "as the number of rows of the first matrix"
      )

  def norm(mat: Matrix) =
    val tr = mat.transpose
    def get_max(l: List[List[Int]], max: Int): Int = l match {
      case Nil => max
      case h :: t =>
        val m = h.map((v) => Math.abs(v)).foldLeft(0)((a, b) => a + b)
        get_max(t, Math.max(max, m))
    }
    get_max(tr.items, 0)
}

@main def test() =
  val m1 = Matrix(4, 4, (i, j, n, m) => i * m + j + 1)
  val m2 = m1.transpose
  val c1 = m1.copy

  println(m1)
  println(m1.add(m2))
  println(m1.equals(c1))

  val sm1 = m1.smul(3)
  println(sm1)

  val m3 = Matrix(4, 5, (i, j, n, m) => i * m + j + 1)
  val m4 = Matrix(5, 4, (i, j, n, m) => i * m + j + 1)

  println(m3.mmul(m4))

  val m5 = Matrix(3, 2, List(List(5, -4, 2), List(-1, 2, 3), List(-2, 1, 0)))
  println(m5.norm)

  val m6 = Matrix(2, 2, List(List(1, -7), List(-2, -3)))
  println(m6.norm)
