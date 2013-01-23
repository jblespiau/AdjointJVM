package org.wsj

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.wsj.PolicyMaker._
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
  class DumbRampPolicyMaker(val freeway: SimpleFreeway,
                            val boundaryConditionPolicy: ProfilePolicy[FreewayBC, FreewayLink],
                            val initialConditionPolicy: Profile[FreewayIC, FreewayLink]) extends RampMeteringPolicyMaker {
    override val network = freeway
    type AppliedBCEntity = FreewayLink
    type AppliedICEntity = FreewayLink

    def givePolicy(network: Network[FreewayLink],
                   bc: ProfilePolicy[FreewayBC, FreewayLink],
                   ic: Profile[FreewayIC, FreewayLink]) = {
      Seq(freeway.fwLinks.map {_.onRamp}.flatten.map {_ -> MaxRampFlux(1)}.toMap)
    }
  }

  test("the dumb test maker compiles and creates a proper profile policy") {
    // create network
    val fd = FundamentalDiagram(1,1,1)
    val ramp = OnRamp(1,1,1)
    val freeway = SimpleFreeway(for (i <- 1 to 5) yield SimpleFreewayLink(1, fd, Some(ramp)))
    val links = freeway.links

    // simple ic's and bc's
    val bc = FreewayBC(1,1)
    val ic = FreewayIC(1,1)
    val bcPolicy: ProfilePolicy[FreewayBC, FreewayLink] = Seq(freeway.links.map {link => link -> bc}.toMap)
    val icPolicy: Profile[FreewayIC, FreewayLink] = freeway.links.map {link => link -> ic}.toMap

    // construct maker, then policy, and check sln for sanity
    val policyMaker = new DumbRampPolicyMaker(freeway, bcPolicy, icPolicy)
    val policy: ProfilePolicy[MaxRampFlux, OnRamp]  = policyMaker.givePolicy
    policy.length should be (1)
    policy(0).size should be (links.size)
  }
}

