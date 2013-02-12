package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/11/13
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
import PolicyMaker._


object AdjointRampMetering {
  type DensityProfile =  ProfilePolicy[Double, SimpleFreewayLink]
  type QueueProfile = ProfilePolicy[Double, OnRamp]
}


import AdjointRampMetering._


// instantiations of IC and BC for ramp metering specifically
case class FreewayBC(demand: Double, splitRatio: Double) extends BoundaryCondition
case class FreewayIC(linkCount: Double, rampCount: Double) extends InitialCondition

abstract class SimulatedFreeway(_fwLinks: Seq[SimpleFreewayLink]) extends SimpleFreeway(_fwLinks) {
  def simulate(u: Adjoint.Control, ic: FreewayIC, bc: FreewayBC): AdjointRampMeteringState
}

// further specifies what's allowed for RampMetering
trait RampMeteringPolicyMaker extends BCICPolicyMaker[FreewayLink, FreewayJunction, Freeway] {
  type AppliedBoundaryCondition = FreewayBC
  type AppliedInitialCondition = FreewayIC
  type AppliedControl = MaxRampFlux
  type AppliedControlEntity = OnRamp
  type AppliedBCEntity = SimpleFreewayLink
  type AppliedICEntity = SimpleFreewayLink
  val freeway: SimpleFreeway
  val N = freeway.links.size
  val orderedRamps = freeway.fwLinks.map {_.onRamp}.flatten
  val nOnramps = orderedRamps.length
  val T = boundaryConditionPolicy.length

  val optimizer = new IpOptAdjointOptimizer

  def initialControl: ProfilePolicy[MaxRampFlux, OnRamp] = {
    (for (_ <- 1 to T) yield {
      orderedRamps.zip(Array.fill(nOnramps)(MaxRampFlux(0.0))).toMap
    }).toSeq
  }

  def uToVector(u: ProfilePolicy[MaxRampFlux, OnRamp]): Adjoint.Control = {
    val control: Adjoint.Control = (u.flatMap {
      prof => orderedRamps.map {ramp => prof(ramp).flux}
    }).toArray
    control
  }

  def vectorToU(vector: Adjoint.Control): ProfilePolicy[MaxRampFlux, OnRamp]  = {
    val first = vector.grouped(nOnramps).map{prof => orderedRamps.zip(prof.map{MaxRampFlux(_)}).toMap}
    val next = first.toArray
    next
  }

  def initialUVector = uToVector(initialControl)
}

abstract class AdjointRampMetering(val _boundaryConditionPolicy: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                          val _initialConditionPolicy: Profile[FreewayIC,SimpleFreewayLink])
  extends RampMeteringPolicyMaker with Adjoint[AdjointRampMeteringState] {

  val boundaryConditionPolicy = _boundaryConditionPolicy
  val initialConditionPolicy = _initialConditionPolicy

  def givePolicy(network: Freeway,
                 bc: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                 ic: Profile[FreewayIC, SimpleFreewayLink]) =  {
    vectorToU(solve(initialUVector))
  }

}
case class AdjointRampMeteringState(density: DensityProfile, queue: QueueProfile) extends SystemState {
  def getState = this
}

