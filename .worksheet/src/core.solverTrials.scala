package core

object solverTrials {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(79); 
  println("Welcome to the Scala worksheet");$skip(36); 
  
  val solver=Solver(List(1,2),3);System.out.println("""solver  : core.Solver.Solver = """ + $show(solver ));$skip(18); 
  println(solver)}
}
