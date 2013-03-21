package org.wsj

import edu.berkeley.path.beats.jaxb.{Density, FundamentalDiagramProfile, Link=>BeatsLink}
import edu.berkeley.path.beats.simulator.Scenario
import java.util
import java.lang
import PolicyMaker._



/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 3/20/13
 * Time: 4:18 PM
 * To change this template use File | Settings | File Templates.
 */
class BeatsScalaAdjointPolicyMaker extends BeatsPolicyMaker {

  def toFD(fd: FundamentalDiagramProfile) = {
    val f = fd.getFundamentalDiagram.get(0)
    val v = f.getFreeFlowSpeed.floatValue()
    val pj = f.getJamDensity.floatValue()
    val fmax = f.getCapacity.floatValue()
    FundamentalDiagram(v, fmax, pj)
  }

  def networkToFreeway(scenario: Scenario): WSJSimulatedFreeway = {
    val fds = {
      val fds = List(scenario.getFundamentalDiagramProfileSet.getFundamentalDiagramProfile.toArray:_*).asInstanceOf[List[FundamentalDiagramProfile]]
      fds.map{fd => (fd.getLinkId.toInt -> toFD(fd))}.toMap
    }
    val initialDensities = {
      val ids = List(scenario.getInitialDensitySet.getDensity.toArray:_*).asInstanceOf[List[Density]]
      ids.map {id => (id.getLinkId.toInt -> id.getContent.toDouble)}.toMap
    }
    // fuck it, betas are all 1
    val ps = Map(-1-> 0.0, -2 -> 4.0)
    val network = scenario.getNetworkList.getNetwork.get(0)
    val links = List((network.getLinkList.getLink.toArray):_*).asInstanceOf[List[BeatsLink]]
    val onramps = links.filter{_.getType == "onramp"}
    val mainlines = links.filter {_.getType == "freeway"}
    val orderedRampIds = onramps.sortBy {ramp => {
      val id = ramp.getId.toInt
      val absId = math.abs(id)
      absId
    }}.map{_.getId.toInt}
    val orderedMLIds = mainlines.map {ml => {
      val id = ml.getId.toInt
      id
    }
    }.sorted
    val onRamps = orderedRampIds.map {(id) =>{
      val fd = fds(id)
      val l0 = initialDensities(id)
      val rmax = fd.fMax
      val p = ps(id)
      OnRamp(-1, rmax, p, id)
    }}
    val lks = orderedMLIds.zip(onRamps).map{case (mlId, onRamp) => {
      new SimpleFreewayLink(
        scenario.getLinkWithId(mlId.toString).getLength.floatValue(),
        fds(mlId),
        Some(onRamp),
        mlId
      )
    }}
    new WSJSimulatedFreeway(lks)
  }

  def compute(initialDensity: util.Map[lang.Integer, lang.Double],
              splitRatios: util.Map[lang.Integer, Array[lang.Double]],
              rampDemands: util.Map[lang.Integer, Array[lang.Double]],
              scenario: Scenario) = {
    val freeway: WSJSimulatedFreeway = networkToFreeway(scenario)
    val ic: Profile[FreewayIC, SimpleFreewayLink] = {
      freeway.fwLinks.map{link=> {
        link ->
          FreewayIC(
            initialDensity.get(link.onRamp.get.id)*
              scenario
                .getLinkWithId(link.onRamp.get.id.toString)
                .getLength
                .floatValue(),
            initialDensity.get(link.id)
          )
      }}.toMap
    }
    val bc: ProfilePolicy[FreewayBC, SimpleFreewayLink] = {
      val T = rampDemands.values().iterator().next().length
      (1 to T).map {t => {
        freeway.fwLinks.map{link => link -> FreewayBC(rampDemands.get(link.onRamp.get.id)(t), 1.0)}.toMap
      }}
    }
    val rampMetering = new AdjointRampMetering(freeway, bc, ic)
    val optimizer = new SimpleUpdateROptimizer(rampMetering, 0, 1)
    optimizer.baseOptimizer.alpha = .5
    optimizer.baseOptimizer.maxEvaluations = 400
    rampMetering.initialUScale = 1.0
    rampMetering.R = .01
    rampMetering.optimizer = optimizer
    val u: ProfilePolicy[MaxRampFlux, OnRamp] = rampMetering.givePolicy
    val T = u.length
    val uPairs = rampMetering.orderedRamps.map{or =>{
      new Integer(or.id) -> {
        val list = new util.ArrayList[lang.Double]()
        u.foreach{uu => list.add(uu(or).flux)}
        list.toArray.asInstanceOf[Array[lang.Double]]
      }
    }}
    val uMap = new java.util.HashMap[lang.Integer, Array[lang.Double]]()
    uPairs.foreach{case (k,v) => uMap.put(k,v)}
    uMap
  }
}
