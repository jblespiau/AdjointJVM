package org.wsj

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import PolicyMaker._
import AdjointRampMetering._
/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/22/13
 * Time: 5:23 PM
 * To change this template use File | Settings | File Templates.
 */


class PolicyMakerTest extends FunSuite with ShouldMatchers {

  // doesn't give good advice, but it implements all the brains from ramp metering policy maker, runs the whole loop
  // feed it all the necessary params, then it can give dumb policy
  // eliminates links w/ no onramps, and just gives a value of 1 for each entity
  class DumbRampPolicyMaker(val freeway: SimulatedFreeway,
                            val boundaryConditionPolicy: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                            val initialConditionPolicy: Profile[FreewayIC, SimpleFreewayLink]) extends RampMeteringPolicyMaker {
    override val network = freeway

    def givePolicy(network: Freeway,
                   bc: PolicyMaker.ProfilePolicy[FreewayBC, SimpleFreewayLink],
                   ic: PolicyMaker.Profile[FreewayIC, SimpleFreewayLink]) = Seq(freeway.fwLinks.map {_.onRamp}.flatten.map {_ -> MaxRampFlux(1)}.toMap)
  }

  test("the dumb test maker compiles and creates a proper profile policy") {
    // create network
    val fd = FundamentalDiagram(1,1,1)
    val ramp = OnRamp(1,1,1)
    class TestFreeway(_fwl: Seq[SimpleFreewayLink]) extends SimulatedFreeway(_fwl) {
      def simulate(u: Adjoint.Control, ic: PolicyMaker.ProfilePolicy[FreewayBC, SimpleFreewayLink], bc: PolicyMaker.Profile[FreewayIC, SimpleFreewayLink]) = {
        val T = bc.size
        val N = ic.length
        val density: DensityProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks) yield link -> 0.0).toMap
        }).toSeq
        val queue: QueueProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks; ramp <- link.onRamp) yield ramp -> 0.0).toMap
        }).toSeq
        AdjointRampMeteringState(density, queue)
      }
    }
    val freeway = new TestFreeway(for (i <- 1 to 5) yield SimpleFreewayLink(1, fd, Some(ramp)))
    val links = freeway.links

    // simple ic's and bc's
    val bc = FreewayBC(1,1)
    val ic = FreewayIC(1,1)
    val bcPolicy: ProfilePolicy[FreewayBC, SimpleFreewayLink] = Seq(freeway.fwLinks.map {link => link -> bc}.toMap)
    val icPolicy: Profile[FreewayIC, SimpleFreewayLink] = freeway.fwLinks.map {link => link -> ic}.toMap

    // construct maker, then policy, and check sln for sanity
    val policyMaker = new DumbRampPolicyMaker(freeway, bcPolicy, icPolicy)
    val policy: ProfilePolicy[MaxRampFlux, OnRamp]  = policyMaker.givePolicy
    policy.length should be (1)
    policy(0).size should be (links.size)
  }
}

