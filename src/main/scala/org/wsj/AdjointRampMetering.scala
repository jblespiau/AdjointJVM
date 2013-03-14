package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/11/13
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */
import PolicyMaker._
import cern.colt.matrix.tdouble.impl.SparseDoubleMatrix1D
import org.apache.commons.math3.optimization.general.{ConjugateGradientFormula, NonLinearConjugateGradientOptimizer}
import org.apache.commons.math3.optimization.DifferentiableMultivariateOptimizer

import scala.collection.mutable
import org.wsj

object AdjointRampMetering {
  type DensityProfile =  ProfilePolicy[Double, SimpleFreewayLink]
  type FlowProfile =  ProfilePolicy[Double, SimpleFreewayLink]
  type RampFlowProfile = ProfilePolicy[Double, OnRamp]
  type QueueProfile = ProfilePolicy[Double, OnRamp]
}


import AdjointRampMetering._


// instantiations of IC and BC for ramp metering specifically
case class FreewayBC(demand: Double, splitRatio: Double) extends BoundaryCondition

case class FreewayIC(linkCount: Double, rampCount: Double) extends InitialCondition

abstract class SimulatedFreeway(_fwLinks: Seq[SimpleFreewayLink]) extends SimpleFreeway(_fwLinks) {
  def simulate(u: Adjoint.Control,
               bc: ProfilePolicy[FreewayBC, SimpleFreewayLink],
               ic: Profile[FreewayIC, SimpleFreewayLink]): AdjointRampMeteringState

  lazy val vList = fwLinks.map{_.fd.v}
  lazy val wList = fwLinks.map{_.fd.w}
  lazy val fMaxList = fwLinks.map{_.fd.fMax}
  lazy val pList = fwLinks.map{_.onRamp.get.priority}
  lazy val rMaxList = fwLinks.map{_.onRamp.get.maxFlux}
  lazy val rhoMaxList = fwLinks.map{_.fd.rhoMax}
}

class WSJSimulatedFreeway(_fwLinks: Seq[SimpleFreewayLink]) extends SimulatedFreeway(_fwLinks)    {
  def simulate(u: Adjoint.Control,
               bc: PolicyMaker.ProfilePolicy[FreewayBC, SimpleFreewayLink],
               ic: PolicyMaker.Profile[FreewayIC, SimpleFreewayLink]) =
  {

    val T = bc.length
    val N = ic.size
    val dt = 1

    // Convert the maps into arrays
    val bcDemandArray = bc.map {prof => (fwLinks.map{prof(_).demand}).toArray}.toArray
    val bcSplitRatioArray = bc.map {prof => (fwLinks.map{prof(_).splitRatio}).toArray}.toArray

    val uMatrix = u.grouped(N).toArray

    val density     = Array.ofDim[Double](T+1,N)
    val queueStore  = Array.ofDim[Double](T+1,N)
    val fluxIn      = Array.ofDim[Double](T,N)
    val fluxOut     = Array.ofDim[Double](T,N)
    val fluxRamp    = Array.ofDim[Double](T,N)
    val fluxOffRamp = Array.ofDim[Double](T,N)
    val demandML    = Array.ofDim[Double](T,N)
    val supplyML    = Array.ofDim[Double](T,N)
    val demandRamp  = Array.ofDim[Double](T,N)

    density(0) = fwLinks.map{ic(_).linkCount}.toArray
    queueStore(0) = fwLinks.map{ic(_).rampCount}.toArray
    val fwLinksList = fwLinks.toArray

    for (loopTime <- 0 to T-1) {

      val prevDensity = density(loopTime)
      val prevQueue  = queueStore(loopTime)
      val inFluxes = new Array[Double](N)
      val outFluxes = new Array[Double](N)
      val rampFluxes = new Array[Double](N)
      val mlDemand = new Array[Double](N)
      val mlSupply = new Array[Double](N)
      val rampDemand = new Array[Double](N)
      val offRampFluxes = new Array[Double](N)


      for (loopLink <- 0 to N-1) {
        var linkUp = null.asInstanceOf[SimpleFreewayLink]
        var densityUp = 0.0
        var linkDown = null.asInstanceOf[SimpleFreewayLink]
        var densityDown = 0.0
        var queue = 0.0
        var queueDemand = 0.0
        var uCurrent = 0.0
        var beta = 0.0
        var p = 1.0
        var rmax = 0.0

        if (loopLink > 0) {// start b.c.
          linkUp = fwLinksList(loopLink-1)
          densityUp = prevDensity(loopLink-1)
        }
        if (loopLink == N-1) {// end b.c.
          linkDown = fwLinksList(loopLink)
          densityDown = prevDensity(loopLink)
          queue = prevQueue(loopLink)
          queueDemand = bcDemandArray(loopTime)(loopLink)
          beta = bcSplitRatioArray(loopTime)(loopLink)
          uCurrent = uMatrix(loopTime)(loopLink)
          p = linkDown.onRamp.get.priority
          rmax = linkDown.fd.rhoMax
        }
        val state = solveJunction(linkUp, densityUp, linkDown, densityDown, queue, queueDemand, u.apply(loopLink), beta, p, dt, rmax)

          if (loopLink > 0) {
            outFluxes(loopLink - 1) = state.fluxUSout
            mlDemand(loopLink - 1) = state.demandUS
            offRampFluxes(loopLink - 1) = state.fluxOff
          }
          if (loopLink < N) {
            rampFluxes(loopLink) = state.fluxDSRamp
            rampDemand(loopLink) = state.demandRamp
            inFluxes(loopLink) = state.fluxDSin
            mlSupply(loopLink) = state.supplyDS
          }
      }


      val newDensity = new Array[Double](N)
      val newQueue = new Array[Double](N)
      for (loopLink <- 0 to N-1) {
        val fIn = inFluxes(loopLink)
        val fOut = outFluxes(loopLink)
        val fRampIn = bcDemandArray(loopTime)(loopLink)
        val fRampOut = rampFluxes(loopLink)
        newDensity(loopLink) = prevDensity(loopLink) + dt / fwLinksList(loopLink).length * (fIn - fOut)
        newQueue(loopLink) = prevQueue(loopLink) + dt * (fRampIn - fRampOut)
      }

      fluxIn(loopTime) = inFluxes
      fluxOut(loopTime) = outFluxes
      fluxRamp(loopTime) = rampFluxes
      demandML(loopTime) = mlDemand
      demandRamp(loopTime) = rampDemand
      supplyML(loopTime) = mlSupply
      fluxOffRamp(loopTime) = offRampFluxes

      density(loopTime+1) = newDensity
      queueStore(loopTime + 1) = newQueue
    }
    //convert arrays into profiles
    val densityProfile: DensityProfile = (for (t <- 0 until T+1) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum) -> density(t)(linkNum)).toMap
    }).toSeq
    val queueProfile: QueueProfile = (for (t <- 0 until T+1) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum).onRamp.get -> queueStore(t)(linkNum)).toMap
    }).toSeq
    val demandProfile: FlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum) -> demandML(t)(linkNum)).toMap
    }).toSeq
    val supplyProfile: FlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum) -> supplyML(t)(linkNum)).toMap
    }).toSeq
    val fluxInProfile: FlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum) -> fluxIn(t)(linkNum)).toMap
    }).toSeq
    val fluxOutProfile: FlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum) -> fluxOut(t)(linkNum)).toMap
    }).toSeq
    val rampDemandProfile: RampFlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum).onRamp.get -> demandRamp(t)(linkNum)).toMap
    }).toSeq
    val fluxRampProfile: RampFlowProfile = (for (t <- 0 until T) yield {
      (for (linkNum <- 0 to N-1) yield fwLinksList(linkNum).onRamp.get -> fluxRamp(t)(linkNum)).toMap
    }).toSeq

    AdjointRampMeteringState(densityProfile, queueProfile, demandProfile, supplyProfile, fluxInProfile, fluxOutProfile,
                             rampDemandProfile, fluxRampProfile)
  }

  def solveJunction(linkUp: SimpleFreewayLink, densityUp: Double, linkDown: SimpleFreewayLink,
                    densityDown: Double, queue: Double, queueDemand: Double, u: Double,
                    beta: Double, p: Double, dt: Double, rmax: Double) =
  {
    val pPrime = 1. / (1. + p)

    val demandUS = {if (linkUp == null) 0
                    else List(linkUp.fd.v * densityUp, linkUp.fd.fMax).min}
    val demandRamp = List(queue/dt, rmax, u).min

    val supplyDS = {if (linkDown == null) Double.MaxValue
                    else List(linkDown.fd.fMax, linkDown.fd.w * (linkDown.fd.rhoMax - densityDown)).min}

    val demand = demandUS * beta + demandRamp

    var fluxUSout = 0.0
    var fluxDSRamp = 0.0
    if (demand < supplyDS) { //demand constrained
      fluxUSout = demandUS
      fluxDSRamp = demandRamp
    } else { // supply constrained
      // blindly assume P intersects in feasible region
      fluxDSRamp = (1 - pPrime) * supplyDS
      fluxUSout = pPrime * supplyDS / beta
      if (fluxUSout > demandUS) { // maxed out inlink
        fluxUSout = demandUS
        fluxDSRamp = supplyDS - beta * fluxUSout
      }
      else if (fluxDSRamp > demandRamp) { // maxed out ramp
        fluxDSRamp = demandRamp
        fluxUSout = (supplyDS - fluxDSRamp) / beta
      }
    }
    val fluxDSin = fluxUSout * beta + fluxDSRamp
    val offRampFlux = fluxUSout * (1 - beta)
    assert(List(fluxUSout, fluxDSin, fluxDSRamp, demandUS, demandRamp, supplyDS, offRampFlux).filter {_ < 0}.length == 0)
    AdjointRampMeteringJunctionOutput(fluxUSout, fluxDSin, fluxDSRamp, demandUS, demandRamp, supplyDS, offRampFlux)
  }
}

// further specifies what's allowed for RampMetering
trait RampMeteringPolicyMaker extends BCICPolicyMaker[FreewayLink, FreewayJunction, Freeway] {
  type AppliedBoundaryCondition = FreewayBC
  type AppliedInitialCondition = FreewayIC
  type AppliedControl = MaxRampFlux
  type AppliedControlEntity = OnRamp
  type AppliedBCEntity = SimpleFreewayLink
  type AppliedICEntity = SimpleFreewayLink
  val freeway: SimulatedFreeway
  val network = freeway
  val links = freeway.fwLinks
  val N = links.length
  val orderedRamps = freeway.fwLinks.map {_.onRamp}.flatten
  val nOnramps = orderedRamps.length
  lazy val T = boundaryConditionPolicy.length

  var optimizer: DifferentiableMultivariateOptimizer =
    new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.POLAK_RIBIERE)

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

class AdjointRampMetering( val freeway: SimulatedFreeway,
                                    val _boundaryConditionPolicy: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                          val _initialConditionPolicy: Profile[FreewayIC,SimpleFreewayLink])
  extends RampMeteringPolicyMaker with Adjoint[AdjointRampMeteringState] {

  var R = .001 // tuning parameter


  val boundaryConditionPolicy = _boundaryConditionPolicy
  val initialConditionPolicy = _initialConditionPolicy
  val nControl = N*T
  val nState = N*(T+1)*8

  val dt: Double = 1 // TODO: dt hack

  lazy val linkLengths = freeway.fwLinks.map{_.length}

  def givePolicy(network: Freeway,
                 bc: ProfilePolicy[FreewayBC, SimpleFreewayLink],
                 ic: Profile[FreewayIC, SimpleFreewayLink]) =  {
    vectorToU(solve(initialUVector))
  }

  def forwardSimulate(control: Adjoint.Control) =
    freeway.simulate(control, boundaryConditionPolicy, initialConditionPolicy)

  def objective(state: AdjointRampMeteringState, control: Adjoint.Control) = {
    var sum = 0.0
    state.getState.density.foreach{prof => prof.foreach{case (link, density) => sum+=link.length*density}}
    state.getState.queue.foreach{prof => prof.foreach{case (ramp, queue) => sum+=queue}}
    for (t <- 0 until T; n <- 0 until N)
      sum+=R*maxBarrier(Left(control(N*t + n)), Left(.1 + math.min(freeway.rMaxList(n),state.queue(t)(orderedRamps(n))))).left.get
    sum
  }

  def djdu(state: AdjointRampMeteringState, control: Adjoint.Control) = {
    // TODO: Add barrier functions
    val rMax = freeway.rMaxList
    val temp = for (t <- 0 until T;
         u =  control.view(t*N, (t+1)*N);
         queue = state.queue(t);
         l = orderedRamps.map{queue(_)};
         ceil = rMax.zip(l).map{case (r,l_) => .1 + math.min(r, l_)}.toArray;
         penalty = maxBarrierGrad(Right(u.toArray), Right(ceil)).right.get.map{_*R}.toSeq
         ) yield penalty
    new SparseDoubleMatrix1D(List.concat(temp:_*).toArray)
  }

  def doubleOrArray(fn: (Double, Double) => Double) = {
    def helper(x: Either[Double, Array[Double]], ceil: Either[Double, Array[Double]]) = {
      x match {
        case Left(xSingle) => {
          ceil match {
            case Left(aSingle) => Left(fn(xSingle, aSingle))
            case Right(aArray) => Right({
              val xArray = Array.fill(aArray.length)(xSingle)
              xArray.zip(aArray).map{case (x,a) => fn(x,a)}
            })
          }
        }
        case Right(xArray) => {
          val aArray = ceil match {
            case Left(aSingle) => Array.fill(xArray.length)(aSingle)
            case Right(arr) => arr
          }
          Right(xArray.zip(aArray).map{case (x,a) => fn(x,a)})
        }
      }
    }
    helper _
  }
  val maxBarrierGrad = doubleOrArray((x,a) => 1.0 / math.max(a - x, 0))
  val minBarrierGrad = doubleOrArray((x,a) => 1.0 / math.max(a - x, 0))
  val maxBarrier = doubleOrArray((x,a) => - math.log(math.max(a - x, 0)))
  def eitherSum(x: Either[Double, Array[Double]]) = {
    x match {
      case Left(a) => a
      case Right(b) => b.sum
    }
  }
  def maxBarrierSum(x: Either[Double, Array[Double]], a: Either[Double, Array[Double]]) = eitherSum(maxBarrier(x,a))




  def djdx(state: AdjointRampMeteringState, control: Adjoint.Control) = {
    val sln = new SparseDoubleMatrix1D(nState)
    for (t <- 0 until T+1) {
      for (n <- 0 until N) {
        sln.setQuick(t*8*N + N*0 + n,linkLengths(n) )
        sln.setQuick(t*8*N + N*1 + n, dt )
      }
    }
    val rMax = freeway.rMaxList
    for (
      t <- 0 until T;
      n <- 0 until N;
      l = state.queue(t)(orderedRamps(n));
      r = rMax(n);
      u = control(t*N + n)
    ) {
      if (u > .1 + math.min(l, r))
        sln.setQuick(t*8*N+ N*1 + n, 9999999999. )
      else if (l < r)
        sln.setQuick(t*8*N + N*1 + n,dt +  -R / (.1 + l - u))
    }
    sln
  }

  def dhdu(state: AdjointRampMeteringState, control: Adjoint.Control) = None


  override def dhduT(state: AdjointRampMeteringState, control: Adjoint.Control) = {
    val sln = new AdjointMatrix(nControl, nState)
    for (n <- 0 until N) {
      val link = freeway.fwLinks(n)
      val ramp = link.onRamp.get
      val rMax = ramp.maxFlux
      for (t <- 0 until T) {
        // TODO: this assumes that all ramps are present...
        val l = state.queue(t)(ramp)
        val uInd = t*N + n
        val u = control(uInd)
        if (u < math.min( l / dt, rMax))
          sln.setQuick(uInd, t*N*8 + N*5 + n, -1.0)
      }
    }
    Some(sln)
  }

  override def dhdxT(state: AdjointRampMeteringState, control: Adjoint.Control) = {
    val sln = new AdjointMatrix(nState, nState)
    var hi = -1
    val bc = boundaryConditionPolicy

    for (i <- 0 until sln.columns())
      sln.setQuick(i, i , 1.0)
    for (k <- 0 until T+1) {
      val kk = math.min(k, T-1) // hack from matlab code for not all u values being specified, but not mattering
      // TODO: investigate kk situation
      for (c <- 0 until 8) {
        for (i <- 0 until N) {
          hi+=1
          c match {
            case 0 => {
              if (k > 0) {
                sln.setQuick(N*8*(k - 1) + N*(1) + i, hi, -1)
                val l = linkLengths(i)
                sln.setQuick(N*8*(k - 1) + N*(6) + i, hi, -dt / l)
                sln.setQuick(N*8*(k - 1) + N*(7) + i, hi, dt / l)
              }
            }
             case 1 => {
               if (k > 0) {
                 sln.setQuick(N*8*(k - 1) + N*(2) + i, hi, -1)
                 sln.setQuick(N*8*(k - 1) + N*(8) + i, hi, dt)
              }
            }
            case 2 => {
              val rho = state.density(k)(links(i))
              val v = freeway.vList(i)
              val F = freeway.fMaxList(i)
              if (rho * v < F)
                sln.setQuick(N*8*(k) + N*(0) + i, hi, -v)
            }
            case 3 => {
              val rho = state.density(k)(links(i))
              val w = freeway.wList(i)
              val rhoMax = freeway.rhoMaxList(i)
              val F = freeway.fMaxList(i)
              if (rhoMax - rho < F)
                sln.setQuick(N*8*(k) + N*(0) + i, hi, w)
            }
            case 4 => {
              val l = state.queue(k)(orderedRamps(i))
              val rMax = freeway.rMaxList(i)
              val u = control(kk*N + i)
              if ( l / dt <= math.min(u, rMax))
                sln.setQuick(N*8*(k) + N*(1) + i, hi, -1 / dt)
            }
            case 5 => {
              // TODO: make ramp demand and supply part of the state
              val sig = {
                val rho = state.density(kk)(links(i))
                val w = freeway.wList(i)
                val rhoMax = freeway.rhoMaxList(i)
                w*(rhoMax - rho)
              }
              val d = {
                val r = freeway.rMaxList(i)
                val l = state.queue(kk)(orderedRamps(i))
                val u = control(kk*N + i)
                math.min(r, math.min(l/dt, u))
              }
              i match {
                case 0 => {
                  if (d < sig)
                    sln.setQuick(N*8*(k) + N*(4) + i, hi, -1)
                  else
                    sln.setQuick(N*8*(k) + N*(3) + i, hi, -1)
                }
                case _ => {
                  val beta = bc(kk)(links(i)).splitRatio
                  val del = {
                    val v = freeway.vList(i-1)
                    val rho = state.density(kk)(links(i-1))
                    v * rho
                  }
                  if (del*beta + d < sig) {
                    sln.setQuick(N*8*(k) + N*(2) + i-1, hi, -beta)
                    sln.setQuick(N*8*(k) + N*(4) + i, hi, -1)
                  }
                  else
                    sln.setQuick(N*8*(k) + N*(3) + i, hi, -1)
                }
              }
            }
            case 6 => {
              if (i == N-1)
                sln.setQuick(N*8*(k) + N*(2) + i, hi, -1)
              else {
                val beta = bc(kk)(links(i+1)).splitRatio
                val del = {
                  val v = freeway.vList(i)
                  val rho = state.density(kk)(links(i))
                  v * rho
                }
                val sig = {
                  val rho = state.density(kk)(links(i+1))
                  val w = freeway.wList(i+1)
                  val rhoMax = freeway.rhoMaxList(i+1)
                  w*(rhoMax - rho)
                }
                val d = {
                  val r = freeway.rMaxList(i+1)
                  val l = state.queue(kk)(orderedRamps(i+1))
                  val u = control(kk*N + i+1)
                  math.min(r, math.min(l/dt, u))
                }
                val p = freeway.pList(i+1)
                val fIn = (state.fluxIn).apply(kk)(links(i+1))
                if (del*beta + d <= sig || fIn * p / beta >= del)
                  sln.setQuick(N*8*(k) + N*(2) + i, hi, -1)
                else if (fIn * ( 1- p) >= d) {
                  sln.setQuick(N*8*(k) + N*(5) + i+1, hi, -1/beta)
                  sln.setQuick(N*8*(k) + N*(4) + i+1, hi, 1/ beta)
                }
                else
                  sln.setQuick(N*8*(k) + N*(3) + i+1, hi, -1)
              }
            }
            case 7 => {
              sln.setQuick(N*8*(k) + N*(5) + i, hi, -1)
              if (i > 1)
                sln.setQuick(N*8*(k) + N*(6) + i-1, hi, bc(kk)(links(i)).splitRatio)
            }
          }
        }
      }
    }
    Some(sln)
  }

  // where the physics comes in, must be implemented
  def dhdx(state: AdjointRampMeteringState, control: Adjoint.Control) = None


}

case class AdjointRampMeteringState(density: DensityProfile,
                                    queue: QueueProfile,
                                    demand: FlowProfile,
                                    supply: FlowProfile,
                                    fluxIn: FlowProfile,
                                    fluxOut: FlowProfile,
                                    rampDemand: RampFlowProfile,
                                    rampFlux: RampFlowProfile
                                     ) extends SystemState {
  def getState = this
}

case class AdjointRampMeteringJunctionOutput(fluxUSout: Double,
                                             fluxDSin: Double,
                                             fluxDSRamp: Double,
                                             demandUS: Double,
                                             demandRamp: Double,
                                             supplyDS: Double,
                                             fluxOff: Double
                                             ) extends SystemState {
    def getState = this
}

