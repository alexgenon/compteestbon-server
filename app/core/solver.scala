package core

import scala.collection.immutable.Stream.consWrapper

/**
 * @author alexandregenon
 */
object Solver {
  abstract class ArithmeticOperation {
    def compute(left: Int, right: Int): Option[Int]
    def apply(left: Int, right: Int): Option[Int] = compute(left, right)
    def isCommutative: Boolean
  }
  object Add extends ArithmeticOperation {
    def compute(left: Int, right: Int): Option[Int] = Some(left + right)
    override def toString = "+"
    def isCommutative = true
  }
  object Sub extends ArithmeticOperation {
    def compute(left: Int, right: Int): Option[Int] = if (left >= right) Some(left - right) else None
    override def toString = "-"
    def isCommutative = false
  }
  object Mul extends ArithmeticOperation {
    def compute(left: Int, right: Int): Option[Int] = Some(left * right)
    override def toString = "*"
    def isCommutative = true
  }
  object Div extends ArithmeticOperation {
    def compute(left: Int, right: Int): Option[Int] = if (right != 0 && left % right == 0) Some(left / right) else None
    override def toString = "/"
    def isCommutative = false
  }

  val operations = List[ArithmeticOperation](Add, Sub, Mul, Div)

  case class Step(op: ArithmeticOperation, left: Int, right: Int) {
    override def toString = left + " " + op + " " + right + " = " + op.compute(left, right).getOrElse(":-/")
  }

  case class Numbers(numbers: List[Int]) {
    def remove(indexes: Int*) =
      Numbers(numbers.zipWithIndex.filter(num => (!indexes.contains(num._2))).map(_._1))

    def ::(number: Int) = Numbers(number :: numbers)

    def length = numbers.length
    def apply(i: Int) = numbers(i)
    def contains(i: Int) = numbers contains (i)
    override def toString = "("+numbers.mkString(",")+")"  
  }

  class Path(operations: List[Step], val finalNumbers: Numbers, val previousPath:Option[Path]=None) {
    def extend(move: Step, updatedNumbers: Numbers) = new Path(move :: operations, updatedNumbers,Some(this))
    def getSteps = operations reverse
    def lastOperation = if(operations.nonEmpty) Some(operations.head) else None
    override def toString = getSteps mkString ("\n")
  }

  def apply(initialNumbers: List[Int], goal: Int) = new Solver(initialNumbers, goal)
  
  class Solver(initialNumbers: List[Int], goal: Int) {

    def from(paths: Set[Path]): Stream[Set[Path]] = {
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          i <- 0 until path.finalNumbers.length; n_i = path.finalNumbers(i)
          j <- 0 until path.finalNumbers.length if i != j; n_j = path.finalNumbers(j)
          op <- operations
          if (op.compute(n_i, n_j)).isDefined // The proposed operation has a natural result
          if (!op.isCommutative || j < i) //if op is commutative, then op(a,b) = op(b,a), we only need to consider it once
        } yield path extend (
          Step(op, n_i, n_j),
          (op.compute(n_i, n_j)).get :: (path.finalNumbers remove (i, j)))
        paths #:: from(more)
      }
    }

    val initialPath = new Path(Nil, Numbers(initialNumbers))
    val pathSets = from(Set(initialPath))

    def solve: Stream[Path] = {
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.finalNumbers contains goal
      } yield path
    }
  }
}