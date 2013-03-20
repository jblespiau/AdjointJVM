package org.wsj.io

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/27/13
 * Time: 10:58 PM
 * To change this template use File | Settings | File Templates.
 */

import scala.util.parsing.json
import org.wsj.PolicyMaker._
import org.wsj._
import org.wsj.SimpleFreewayLink
import scala.Some
import org.wsj.FreewayBC
import org.wsj.OnRamp
import org.wsj.FreewayIC
import org.wsj.FundamentalDiagram

class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }

object M extends CC[Map[String, Any]]
object L extends CC[List[Any]]
object S extends CC[String]
object D extends CC[Double]
object B extends CC[Boolean]

case class FreewayScenario(links: Seq[SimpleFreewayLink], ic: Profile[FreewayIC, SimpleFreewayLink], bc: ProfilePolicy[FreewayBC, SimpleFreewayLink])

object LoadScenario {

  def toDoubleArray(g: List[Any]): Array[Double] = {
    (for (D(d) <- g) yield d).toArray
  }

  def loadScenario(fn: String): FreewayScenario = {
    val source = scala.io.Source.fromFile(fn)
    val js = json.JSON.parseFull(source.mkString)
    source.close()
    val map = js.get.asInstanceOf[Map[String, Any]]
    val p0Array = toDoubleArray(map.get("p0").get.asInstanceOf[List[Any]])
    val l0Array = toDoubleArray(map.get("l0").get.asInstanceOf[List[Any]])
    val vArray = toDoubleArray(map.get("v").get.asInstanceOf[List[Any]])
    val wArray = toDoubleArray(map.get("w").get.asInstanceOf[List[Any]])
    val fmArray = toDoubleArray(map.get("fm").get.asInstanceOf[List[Any]])
    val pArray = toDoubleArray(map.get("p").get.asInstanceOf[List[Any]])
    val lengthArray = toDoubleArray(map.get("L").get.asInstanceOf[List[Any]])
    val rMaxArray = toDoubleArray(map.get("rmax").get.asInstanceOf[List[Any]])
    val demands = map.get("D").get.asInstanceOf[List[Any]]
    val betas = map.get("beta").get.asInstanceOf[List[Any]]
    val betasArray = for (L(b) <- betas) yield toDoubleArray(b)
    val demandsArray = for (L(b) <- demands) yield toDoubleArray(b)
    val onramps = rMaxArray.zip(pArray).map {case (r,pv) => Some(OnRamp(-1, r, pv))}
    val fds = (vArray, wArray, fmArray).zipped.map{case (vv, wv, fmv) => FundamentalDiagram(vv, fmv, fmv*( 1.0 / vv + 1.0 / wv))}
    val links = (onramps, fds, lengthArray).zipped.map{case (or, fd, len) => SimpleFreewayLink(len ,fd, or)}
    val betaDemandPairs = betasArray.zip(demandsArray)
    val bcs: ProfilePolicy[FreewayBC, SimpleFreewayLink] = betaDemandPairs.map{
      case (bp, dp) => {
        val bdLinkPairs = (bp, dp, links).zipped
        val mappedTuples = bdLinkPairs.toList.map {
          case (bpv, dpv, link) => {
            link -> FreewayBC(dpv, bpv)
          }
        }
        val mapValues = Map(mappedTuples:_*)
        mapValues
      }
    }
    val ics: Profile[FreewayIC, SimpleFreewayLink] = ((p0Array, l0Array, links).zipped.map{
      case (p0v, l0v, link) => link -> FreewayIC(p0v, l0v)}).toMap
    FreewayScenario(links, ics, bcs)
  }
}
