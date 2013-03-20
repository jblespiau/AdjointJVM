package org.wsj

import io.LoadScenario
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import PolicyMaker._
import breeze.plot._
import breeze.linalg.DenseVector


/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 3/1/13
 * Time: 6:04 PM
 * To change this template use File | Settings | File Templates.
 */

class FinalCostTest(
                     val rampMetering: RampMeteringPolicyMaker,
                     val finalCost: Double,
                     val tolerance: Double = .1
                     )
  extends FunSuite with ShouldMatchers {

  def testFinalCost() = {
    val output: ProfilePolicy[MaxRampFlux, OnRamp] = rampMetering.givePolicy
    val vector = rampMetering.uToVector(output)
    println(rampMetering.controlProfileToArray(output).map {_.mkString(" ")}.mkString("\n"))
    val state = rampMetering.freeway.simulate(
      vector,
      rampMetering.boundaryConditionPolicy,
      rampMetering.initialConditionPolicy
    )
    val ttt = rampMetering.totalTravelTime(state, vector)
    println(ttt)
    ttt should be (finalCost plusOrMinus tolerance)
  }
}

class SamithaSimpleTestCase extends FinalCostTest({
  val scen = LoadScenario.loadScenario(Constants.samithaFN)
  val fw = new WSJSimulatedFreeway(scen.links)
  val rampMetering = new AdjointRampMetering(fw, scen.bc, scen.ic)
  val optimizer = new SimpleUpdateROptimizer(rampMetering, 0, 1)
  optimizer.baseOptimizer.alpha = .5
  optimizer.baseOptimizer.maxEvaluations = 400
  rampMetering.initialUScale = 1.0
  rampMetering.R = .01
  rampMetering.optimizer = optimizer
  rampMetering},
7.7,
.1) {
  test("samitha 1 on 1 off network with simple gradient descent, 1 R loop") {
    testFinalCost()
    val op: SimpleUpdateROptimizer = rampMetering.optimizer.asInstanceOf[SimpleUpdateROptimizer]
    val intermediates = op.baseOptimizer.intermediates.toArray.map{math.min(_, 15)}
    val fig = Figure()
    val p = fig.subplot(0)
    val x = breeze.linalg.linspace(1, intermediates.length, intermediates.length)
    val y: DenseVector[Double] = DenseVector[Double](intermediates)
    p+=plot(x, y)
    p.xlabel = "Iteration number"
    p.ylabel = "Cost evaluation"
    fig.saveas("plot.pdf")
    println()
  }
}

class SamithaIpOptTestCase extends FinalCostTest({
  val scen = LoadScenario.loadScenario(Constants.samithaFN)
  val fw = new WSJSimulatedFreeway(scen.links)
  val rampMetering = new AdjointRampMetering(fw, scen.bc, scen.ic)
  val optimizer = new IpOptUpdateROptimizer(rampMetering, 0, 1)
  optimizer.baseOptimizer.maxEvaluations = 200
  rampMetering.initialUScale = 1.0
  rampMetering.R = .01
  rampMetering.optimizer = optimizer
  rampMetering},
7.7,
.1) {
  test("samitha 1 on 1 off network with IpOpt, 1 R loop") {
    testFinalCost()

  }
}


