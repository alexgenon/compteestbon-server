package coreSolverTest

import org.scalatest.FunSuite

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.scalatest.junit.JUnitRunner


class solverTestSuite extends FunSuite {
  import core.Solver
  
  trait BasicCombinations{
    val solver=Solver(List(1,2),3)
  }
  
  test("basic combination for (1,2)") {
    new BasicCombinations{
      val allPathSets = solver.pathSets.toList
      val allCombs = (for(s<-allPathSets; p <-s) yield p.finalNumbers).toSet 
      assert(allCombs.size === 4)
    }
  }
  
  test("solutions to basic combination for (1,2) goal = 3") {
    new BasicCombinations {
      val solutions = solver.solve.toList
      assert(solutions.exists { p => p.finalNumbers.contains(3) })
    }
  }
  
  test("more complex problem with a solution"){
    val solver=Solver(List(5,3,9,25),375)
    val solutions = solver.solve.toList
    assert(solutions.exists { p => p.finalNumbers.contains(375) })
  }
  
  test("no solution"){
    val solver=Solver(List(5,3,9),375)
    val solutions = solver.solve.toList
    assert(solutions.isEmpty)
  }
}