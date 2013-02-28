package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/12/13
 * Time: 10:47 AM
 * To change this template use File | Settings | File Templates.
 */
import AdjointRampMetering._
import io.LoadScenario
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import PolicyMaker._
import org.apache.commons.math3.exception.TooManyEvaluationsException

class AdjointRampMeteringSuite extends FunSuite with ShouldMatchers {

  val networkFilenames = List(
    "/Users/jdr/Documents/github/AdjointRamp/networks/samitha1onramp.json"
  )

  test("ramp metering actually completes full run") {
    println("running this guy")
    val fd = FundamentalDiagram(1,1,1)
    val ramp = OnRamp(1,1,1)
    val T = 3
    val N = 5
    val freeway = new DumbFreeway(for (i <- 1 to N) yield SimpleFreewayLink(1, fd, Some(ramp)))
    val links = freeway.links

    // simple ic's and bc's
    val bc = FreewayBC(1,1)
    val ic = FreewayIC(1,1)

    val bcs: ProfilePolicy[FreewayBC, SimpleFreewayLink] = for (_ <- 1 to 3) yield {
      (for (link <- freeway.fwLinks) yield {
        link -> bc
      }).toMap
    }

    val ics: Profile[FreewayIC, SimpleFreewayLink] = {
      (for (link <- freeway.fwLinks) yield {
        link -> ic
      }).toMap
    }
    val rampPC = new AdjointRampMetering(freeway, bcs, ics)
    // rampPC.optimizer = new IpOptAdjointOptimizer()
    val policy: ProfilePolicy[MaxRampFlux, OnRamp] =  rampPC.givePolicy
    println("sure")
  }


  test("run loadScenario through adjoint on dumb freeway") {
    println("then this guy")
    val scenario = LoadScenario.loadScenario(Constants.samithaFN)
    val rampMetering = new AdjointRampMetering(new DumbFreeway(scenario.links), scenario.bc, scenario.ic)

    val policy: ProfilePolicy[MaxRampFlux, OnRamp] = rampMetering.givePolicy

    rampMetering.R*=1000

    // TODO: not sure why, we should check into this
    evaluating {rampMetering.givePolicy: ProfilePolicy[MaxRampFlux, OnRamp]} should produce[TooManyEvaluationsException]
  }


}
