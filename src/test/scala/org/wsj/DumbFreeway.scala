package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/28/13
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */

import AdjointRampMetering.{DensityProfile, FlowProfile, QueueProfile, RampFlowProfile}

class DumbFreeway (_fwl: Seq[SimpleFreewayLink]) extends SimulatedFreeway(_fwl) {
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
