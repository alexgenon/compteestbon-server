object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(57); 
  println("Welcome to the Scala worksheet");$skip(41); 
  
  val solver=core.Solver(List(1,2),3);System.out.println("""solver  : core.Solver.Solver = """ + $show(solver ));$skip(37); 
  val paths = solver.pathSets.toList;System.out.println("""paths  : List[Set[core.Solver.Path]] = """ + $show(paths ));$skip(44); val res$0 = 
  for (path <- paths) yield {println(path)};System.out.println("""res0: List[Unit] = """ + $show(res$0))}
}
