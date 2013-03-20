package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/22/13
 * Time: 10:13 AM
 * To change this template use File | Settings | File Templates.
 */
import PolicyMaker._

trait PolicyControl // control type to apply
trait ControlEntity // entity upon which to apply the control


// just hands off a policy profile of controls and the entities to which they apply
trait PolicyMaker {
  type AppliedControl <: PolicyControl
  type AppliedControlEntity <: ControlEntity
  def givePolicy: ProfilePolicy[AppliedControl, AppliedControlEntity]
}


object PolicyMaker {
  type Profile[T,C] = Map[C,T] // single instance in time, all entities
  type ProfilePolicy[T, C] = Seq[Profile[T,C]] // all times, all entities

  // types of policies that can be acted upon should all belong here, add more classes as more cases come up
  case class MaxRampFlux(flux: Double) extends PolicyControl

  def profileToArray[T,C](profile: Profile[T,C], ordering: Seq[C], unwrap: (T) => Double) = {
    ordering.map {(c: C) => unwrap(profile(c))}.toArray
  }

  def profilePolicyToArray[T,C](pp: ProfilePolicy[T,C], ordering: Seq[C], unwrap: (T) => Double) = {
    pp.map{profileToArray(_, ordering, unwrap)}.toArray
  }
}


trait BoundaryCondition // to be extended by implementations
trait InitialCondition // ditto
trait ICEntity // ditto
trait BCEntity // ditto

trait BCICPolicyMaker[L<:Link, J<:Junction[L], N<:Network[L,J]] extends PolicyMaker {
  // policies that can be applied to networks
  // information about IC's and BC's is passed in
  // rather than no argument givePolicy

  type AppliedBoundaryCondition <: BoundaryCondition // to be implemented by inheritor
  type AppliedInitialCondition <: InitialCondition
  type AppliedBCEntity <: BCEntity
  type AppliedICEntity <: ICEntity
  val network: N
  val boundaryConditionPolicy: ProfilePolicy[AppliedBoundaryCondition, AppliedBCEntity]
  val initialConditionPolicy: Profile[AppliedInitialCondition, AppliedICEntity]
  def givePolicy(network: N,
                 bc: ProfilePolicy[AppliedBoundaryCondition, AppliedBCEntity],
                 ic: Profile[AppliedInitialCondition, AppliedICEntity]): ProfilePolicy[AppliedControl, AppliedControlEntity]
  def givePolicy = givePolicy(network, boundaryConditionPolicy, initialConditionPolicy) // straight forward implementation
}

