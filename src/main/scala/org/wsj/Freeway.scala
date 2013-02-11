package org.wsj

import org.wsj.PolicyMaker._

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/22/13
 * Time: 2:26 PM
 * To change this template use File | Settings | File Templates.
 */

// standard triangular diagram
case class FundamentalDiagram(v: Double, fMax: Double, rhoMax: Double)

// can be viewed as a simple link
case class OnRamp(maxLength: Double, maxFlux: Double, priority: Double) extends Link with ControlEntity
case class OffRamp(maxFlux: Option[Double] = None) extends Link

// since fw junctions only allow one in and one out, and model on/offramps as buffers
case class FreewayJunction(inLink: Option[FreewayLink],
                           outLink: Option[FreewayLink],
                           onRamp: Option[OnRamp],
                           offRamp: Option[OffRamp])
  extends Junction[FreewayLink](Seq(inLink).flatten, Seq(outLink).flatten)

// special cases for freewayJunctions
case class FreewaySource(link: FreewayLink,
                         _onRamp: OnRamp)
  extends FreewayJunction(None, Some(link), Some(_onRamp), None)

case class FreewaySink(link: FreewayLink,
                         _offRamp: OffRamp = {OffRamp()})
  extends FreewayJunction(None, Some(link), None, Some(_offRamp))


// straightaway network of freeway links
trait Freeway extends Network[FreewayLink] {
  type NetworkJunction = FreewayJunction
}


case class FreewayLink(length: Double,
                       fd: FundamentalDiagram,
                       id: Int = FreewayLink.nextId) // allows for auto-uniqueness of ID
  extends Link  with ControlEntity with BCEntity with ICEntity // allowed to be a control entity (for VSL), and have BC and IC applied to it

object FreewayLink {
  var idCounter = 0

  def nextId = {
    idCounter+=1
    idCounter
  }
}

// simpler expression of Freeway, which is more in tune with our def'n of offramp and onramp.
case class SimpleFreewayLink(_length: Double,
                             _fd: FundamentalDiagram,
                             onRamp: Option[OnRamp])
  extends FreewayLink(_length, _fd)

// main benefit is it automatically constructs junctions
case class SimpleFreeway(fwLinks: Seq[SimpleFreewayLink]) extends Freeway {
  def junctions = {
    val maxInd = fwLinks.length - 1
    // zip with ind, so you know where you are along in the links
    // 0 is source, end is sink, in between, pair with the preceding neighbor
    fwLinks.zipWithIndex.map {case (link, ind) => {
      ind match {
        case 0 => FreewaySource(link, link.onRamp.get)
        case `maxInd` => FreewaySink(link)
        case i => FreewayJunction(Some(fwLinks(i-1)), Some(link), link.onRamp, None)
      }
    }}
  }
}

// instantiations of IC and BC for ramp metering specifically
case class FreewayBC(demand: Double, splitRatio: Double) extends BoundaryCondition
case class FreewayIC(linkCount: Double, rampCount: Double) extends InitialCondition

// further specifies what's allowed for RampMetering
trait RampMeteringPolicyMaker extends BCICPolicyMaker[SimpleFreewayLink] {
  type AppliedBoundaryCondition = FreewayBC
  type AppliedInitialCondition = FreewayIC
  type AppliedControl = MaxRampFlux
  type AppliedControlEntity = OnRamp
  type AppliedBCEntity = SimpleFreewayLink
  type AppliedICEntity = SimpleFreewayLink

  override val network: SimpleFreeway
  val N = network.links.size
  val orderedRamps = network.fwLinks.map {_.onRamp}.flatten
  val nOnramps = orderedRamps.length
  val T = boundaryConditionPolicy.length

  val optimizer = new IpOptAdjointOptimizer

  def initialControl: ProfilePolicy[MaxRampFlux, OnRamp] = {
    (for (_ <- 1 to T) yield {
      orderedRamps.zip(Array.fill(nOnramps)(MaxRampFlux(0.0))).toMap
    }).toArray
  }

  def uToVector(u: ProfilePolicy[MaxRampFlux, OnRamp]): Adjoint.Control = {
    (u.flatMap {
      prof => orderedRamps.map {ramp => prof(ramp).flux}
    }).toArray
  }

  def vectorToU(vector: Adjoint.Control): ProfilePolicy[MaxRampFlux, OnRamp]  = {
    (vector.grouped(nOnramps).map{prof => orderedRamps.zip(prof.map{MaxRampFlux(_)}).toMap}).toArray
  }

  def initialUVector = uToVector(initialControl)
}