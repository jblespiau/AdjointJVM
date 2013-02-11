package org.wsj

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/17/13
 * Time: 7:41 PM
 * To change this template use File | Settings | File Templates.
 */

import org.scalatest._
import matchers.ShouldMatchers
import org.apache.commons.math3.optimization.general.{ConjugateGradientFormula, NonLinearConjugateGradientOptimizer}
import org.wsj.Adjoint.Control
import cern.colt.matrix.tdouble.impl.{DenseDoubleMatrix1D, SparseDoubleMatrix1D, SparseDoubleMatrix2D}
import org.apache.commons.math3.optimization.DifferentiableMultivariateOptimizer

class AdjointSuite extends FunSuite with ShouldMatchers {

  // simple state to use in testing simple adjoint
  case class SimpleState(state: Double) extends SystemState {
    def getState = state
  }

  // easy to solve by hand system, init a bunch of u values at random, and show they converge for adjoint formulation
  // min x^2 + u^2
  // subject to: 3x + 2u = 3

  class SimpleAdjoint(val optimizer: DifferentiableMultivariateOptimizer = {
    new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.POLAK_RIBIERE)
  } )
    extends Adjoint[SimpleState] {

    override def dhdxT(state: SimpleState, control: Control) = Some(new AdjointMatrix(Array(Array(3.0))))
    def djdx(state: SimpleState, control: Control) = new SparseAdjointVector(Array(2.0 * state.getState))
    def dhdu(state: SimpleState, control: Control) = Some(new AdjointMatrix(Array(Array(2.0))))
    def djdu(state: SimpleState, control: Control) = new DenseDoubleMatrix1D(Array(2.0 * control(0)))


    // where the physics comes in, must be implemented
    def dhdx(state: AdjointSuite.this.type#SimpleState, control: Adjoint.Control) = None

    def forwardSimulate(control: Adjoint.Control) = SimpleState((3 - 2 * control(0)) / 3.0)

    def objective(state: SimpleState, control: Adjoint.Control) = {

      val x = state.getState
      val u = control(0)
      x * x + u * u
    }
  }

  test("see if SimpleAdjoint works as it should") {
    val optimizers = List(
      new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.POLAK_RIBIERE),
      new IpOptAdjointOptimizer
    )
    for (optimizer <- optimizers) {
      val simpleAdjoint = new SimpleAdjoint
      val tolerance = .0001
      val ustar =  6.0 / 13
      val xstar = 9.0 / 13
      for (i <- 1 to 10) {
        val u0: Control = Array(5*math.random)
        val uCheck= simpleAdjoint.solve(u0)
        uCheck(0) should be (ustar plusOrMinus tolerance)
        simpleAdjoint.forwardSimulate(uCheck).getState should be (xstar plusOrMinus tolerance)
        println("next.....")
      }
    }
  }
}