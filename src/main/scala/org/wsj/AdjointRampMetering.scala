package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/11/13
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
import PolicyMaker._

class AdjointRampMetering(val boundaryConditionPolicy: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                          val initialConditionPolicy: Profile[FreewayIC,SimpleFreewayLink])
  extends RampMeteringPolicyMaker with Adjoint {
  def givePolicy(network: Network[FreewayLink],
                 bc: ProfilePolicy[AppliedBoundaryCondition, AppliedBCEntity],
                 ic: PolicyMaker.Profile[AppliedInitialCondition, AppliedICEntity]) = {
    vectorToU(solve(initialUVector))
  }

}
