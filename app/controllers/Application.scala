package controllers

import javax.inject.Inject
import play.api._
import play.api.mvc._
import play.api.libs.json._
import core._
import models.daos.drivers.neo4j

class Application @Inject() (db: neo4j) extends Controller {

 implicit val stepWriter = new Writes[Solver.Step]{
      def writes(step: Solver.Step)=Json.obj(
          "l"->step.left,
          "r"->step.right,
          "op"->step.op.toString
          )
    }
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def solveAsJson(goal: Int, valuesStr: String) = Action {
    val values = valuesStr.split("/").map(x => x.toInt).toList
    val solver = Solver(values, goal)
    val result = solver.solve
    if (result.isEmpty)
      NotFound
    else {
      val solutions = (for (p <- result) yield {
        Json.toJson(p.getSteps)
      }).toList
      Ok(Json.toJson(solutions)).as(JSON)
    }
  }
  
  def solveAsHTML(goal: Int, valuesStr: String) = Action{
    val values=valuesStr.split("/").map(x => x.toInt).toList
    val solver = Solver(values, goal)
    val result = solver.solve
    if (result.isEmpty)
      NotFound
    else {
      val solutions = (for (p <- result) yield p).toList
      Ok(views.html.solver("found",solutions))
    }
  }
  
  def uploadAsGraph(goal: Int, values: List[Int]) = Action {
    val solver = Solver(values, goal)
    val pathsStream = solver.pathSets
    for {
      pathsSet <- pathsStream.toList
      path <- pathsSet
    } yield {
      db.createNode(path.finalNumbers)
      val fromNode = path.previousPath
      if(fromNode.isDefined)
        db.createStep(fromNode.get.finalNumbers, path.finalNumbers, path.lastOperation.get)
    }
    Ok(views.html.index("Graph created"))
  }
}
