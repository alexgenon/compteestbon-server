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
    def compute:Option[Int] = op.compute(left, right)
  }
  object Step{
    def getValidStep(o: ArithmeticOperation, o1:Int, o2:Int):Option[Step] = 
      // Note : we rely on the fact that o.isCommutative => always o.compute.isDefined
      // and not commutative implies that only 1 possible combination is defined to simplify this logic
      if(o.isCommutative)
        Some(Step(o,o1,o2))
      else if(o.compute(o1,o2).isDefined)
          Some(Step(o,o1,o2))
      else if(o.compute(o2,o1).isDefined)
          Some(Step(o,o2,o1))
      else None
  }

  case class MultiSet[T] (val map : Map[T,Int]){
      def contains(n:T):Boolean = map contains n
      def get(n:T):Int = map getOrElse (n,0)
      def remove(n:T):MultiSet[T] = {
        val e = map get n
        e match {
          case None => MultiSet(map)
          case e:Some[Int] if(e.get==1) => MultiSet(map - n)
          case e:Some[Int] if(e.get>0) => MultiSet(map updated (n, e.get-1))
        }
      }
      def add(n:T):MultiSet[T] = MultiSet(map updated (n,map.getOrElse(n,0)+1))
      def ::(n:T) = add(n)
      def getPairs:List[(T,T)] = {
         def rec(l:List[T],acc:List[(T,T)]):List[(T,T)] = l match {
           case List() => acc
           case head::tail => {
            val subList = tail.map((head,_))
            if(map.get(head).get>=2)
              /* The reason why we can't use the Scala .combinations function
               * over list without a trick such as 
               * map.flatMap(x => {if (x._2>=2) List(x._1,x._1) else List(x._1)}).toList */
              rec(tail,((head,head)::subList)++acc)
            else
              rec(tail,subList++acc)
           }
         }
         rec(map.keys.toList,List())
      }
      def mkString(sep:String):String = map.flatMap(x => List.fill(x._2)(x._1)).mkString(sep)
  }
  
  object MultiSet{
     def apply[T](l:List[T]):MultiSet[T] = l match {
       case List()=>MultiSet(Map[T,Int]())
        case head::tail => MultiSet(tail).add(head) 
      }
  }
  
  case class Numbers(numbers: MultiSet[Int]) {
    
    def remove(n: Int,m:Int) = Numbers(numbers.remove(n).remove(m))

    def ::(number: Int) = Numbers(number :: numbers)
    def pairs = numbers.getPairs
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
          (l,r) <- path.finalNumbers.pairs
          op <- operations
          s<-Step.getValidStep(op,l,r)
        } yield path extend (
          s,
          (s.compute).get :: (path.finalNumbers remove (l,r)))
        paths #:: from(more)
      }
    }

    val initialPath = new Path(Nil, Numbers(MultiSet(initialNumbers)))
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