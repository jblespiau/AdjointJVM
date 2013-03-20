package org.wsj

import io.LoadScenario
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import PolicyMaker._

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 3/1/13
 * Time: 6:04 PM
 * To change this template use File | Settings | File Templates.
 */
class WSJFreewayTest extends FunSuite with ShouldMatchers {

  test("let's take it for a spin") {
    val scen = LoadScenario.loadScenario(Constants.bigFN)
    val fw = new WSJSimulatedFreeway(scen.links)
    val rampMetering = new AdjointRampMetering(fw, scen.bc, scen.ic)
    println(rampMetering.nState)
    println(rampMetering.nControl)
    //val optimizer = new SimpleUpdateROptimizer(rampMetering, .3, 7)
    //optimizer.baseOptimizer.alpha = .05
    //optimizer.baseOptimizer.maxEvaluations = 150
    //val optimizer = new IpOptAdjointOptimizer
    val optimizer = new IpOptUpdateROptimizer(rampMetering, .3, 5)
    optimizer.baseOptimizer.maxEvaluations = 4
    rampMetering.initialUScale = 1.0
    rampMetering.R = 1.0
    rampMetering.optimizer = optimizer
    val output: ProfilePolicy[MaxRampFlux, OnRamp] = rampMetering.givePolicy
    println(output)
  }

}
