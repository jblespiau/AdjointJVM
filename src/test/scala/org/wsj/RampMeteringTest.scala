package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/12/13
 * Time: 10:47 AM
 * To change this template use File | Settings | File Templates.
 */
import AdjointRampMetering._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import PolicyMaker._

class AdjointRampMeteringSuite extends FunSuite with ShouldMatchers {

  test("ramp metering actually completes full run") {
    val fd = FundamentalDiagram(1,1,1)
    val ramp = OnRamp(1,1,1)
    class TestFreeway(_fwl: Seq[SimpleFreewayLink]) extends SimulatedFreeway(_fwl) {
      def simulate(u: Adjoint.Control, bc: PolicyMaker.ProfilePolicy[FreewayBC, SimpleFreewayLink], ic: PolicyMaker.Profile[FreewayIC, SimpleFreewayLink]) = {
        val T = bc.length
        val N = ic.size
        val density: DensityProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks) yield link -> 0.0).toMap
        }).toSeq
        val flow: FlowProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks) yield link -> 0.0).toMap
        }).toSeq
        val queue: QueueProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks; ramp <- link.onRamp) yield ramp -> 0.0).toMap
        }).toSeq
        val rampFlow: RampFlowProfile = (for (_ <- 0 until T+1) yield {
          (for (link <- fwLinks; ramp <- link.onRamp) yield ramp -> 0.0).toMap
        }).toSeq

        AdjointRampMeteringState(density, queue, Some(flow), Some(flow), Some(flow), Some(flow), Some(rampFlow), Some(rampFlow))
      }
    }
    val T = 3
    val N = 5
    val freeway = new TestFreeway(for (i <- 1 to N) yield SimpleFreewayLink(1, fd, Some(ramp)))
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
}
