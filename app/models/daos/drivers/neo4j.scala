package models.daos.drivers

import javax.inject.Inject
import play.api.libs.ws._
import play.api.Play
import play.api.Play.current
import play.api.libs.json._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._
import core._

/**
 * @author alexandregenon
 * Class to generate requests to the neo4j database and get the results.
 *
 * @param ws injected WS play service
 */
class neo4j @Inject() (ws: WSClient) {
  val neo4jEndPoint = "http://" + (Play.configuration.getString("neo4j.server") getOrElse ("localhost")) +
    ":" + (Play.configuration.getInt("neo4j.port") getOrElse ("7474")) +
    (Play.configuration.getString("neo4j.endpoint") getOrElse ("/db/data/"))

  val neo4jUser = Play.configuration.getString("neo4j.user") getOrElse ("neo4j")
  val neo4jPassword = Play.configuration.getString("neo4j.password") getOrElse ("neo4j")

  ws.url(neo4jEndPoint)
    .withAuth(neo4jUser, neo4jPassword, WSAuthScheme.BASIC)
    .withHeaders("Accept" -> "application/json;charset=UTF-8", "Content-Type" -> "application/json")
    .get()
    .map(res => {
      res.status match {
        case 200 =>
          val json = Json.parse(res.body)
          if ((json \ "errors").as[Seq[JsObject]].nonEmpty) {
            throw new Exception(res.body)
          }
        case _ => throw new Exception(res.body)
      }
    })

  def createNode(numbers: Solver.Numbers) = {
    val cypherQuery = "MERGE (node {values:\"" + numbers + "\"})"
    val query = ws.url(neo4jEndPoint + "transaction/commit")
      .withAuth(neo4jUser, neo4jPassword, WSAuthScheme.BASIC)
      .withHeaders("Accept" -> "application/json;charset=UTF-8", "Content-Type" -> "application/json")
      .post(Json.obj(
        "statements" -> Json.arr(Json.obj(
          "statement" -> cypherQuery))))

    val response = Await.result(query, Duration(100, SECONDS))

    response.status match {
      case 200 => {
        val json = Json.parse(response.body)
        if ((json \ "errors").as[Seq[JsObject]].nonEmpty)
          throw new Exception(response.body)
      }
      case _ => throw new Exception(response.body)
    }
  }
  def createStep(from: Solver.Numbers, to: Solver.Numbers, step: Solver.Step) = {
    val cypherQuery = "MATCH (f),(t) "+
      "WHERE f.values = \""+from+"\" AND t.values = \""+to +"\""+
      "CREATE (f)-[r:STEP {step : \""+step+"\"}]->(t)"
      
    val query = ws.url(neo4jEndPoint + "transaction/commit")
      .withAuth(neo4jUser, neo4jPassword, WSAuthScheme.BASIC)
      .withHeaders("Accept" -> "application/json;charset=UTF-8", "Content-Type" -> "application/json")
      .post(Json.obj(
        "statements" -> Json.arr(Json.obj(
          "statement" -> cypherQuery))))

    val response = Await.result(query, Duration(100, SECONDS))

    response.status match {
      case 200 => {
        val json = Json.parse(response.body)
        if ((json \ "errors").as[Seq[JsObject]].nonEmpty)
          throw new Exception(response.body)
      }
      case _ => throw new Exception(response.body)
    }
  }

}