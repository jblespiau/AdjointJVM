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
    val scen = LoadScenario.loadScenario(Constants.samithaFN)

    val fw = new WSJSimulatedFreeway(scen.links)

    val rampMetering = new AdjointRampMetering(fw, scen.bc, scen.ic)
    //rampMetering.optimizer = new SimpleGradientDescentOptimizer
    rampMetering.optimizer = new SimpleUpdateROptimizer(rampMetering, .1, 5)
    //rampMetering.optimizer = new IpOptUpdateROptimizer(rampMetering, .4, 40)
    val output: ProfilePolicy[MaxRampFlux, OnRamp] = rampMetering.givePolicy
    println(output)
  }

}
