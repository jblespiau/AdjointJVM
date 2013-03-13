package org.wsj

import org.apache.commons.math3.optimization.{GoalType, DifferentiableMultivariateOptimizer}
import org.apache.commons.math3.analysis.{MultivariateVectorFunction, DifferentiableMultivariateFunction}
import cern.colt.matrix.tdouble.impl.{SparseCCDoubleMatrix2D, SparseDoubleMatrix1D}
import cern.colt.matrix.tdouble.algo.{DenseDoubleAlgebra, SparseDoubleAlgebra}
import cern.colt.matrix.tdouble.{DoubleMatrix2D, DoubleMatrix1D}
import cern.jet.math.tdouble.DoubleFunctions


/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 1/17/13
 * Time: 6:27 PM
 * To change this template use File | Settings | File Templates.
 */

trait SystemState {
  def getState: Any
}

object Adjoint {
  type Control = Array[Double]
  type AdjointMatrix = SparseCCDoubleMatrix2D
  type AdjointVector = DoubleMatrix1D
  type SparseAdjointVector = SparseDoubleMatrix1D


  val algebra = new SparseDoubleAlgebra()
  val dAlg = new DenseDoubleAlgebra()
}


import Adjoint._

trait Adjoint[T<:SystemState] {
  type AdjointMatrix = SparseCCDoubleMatrix2D
  type AdjointVector = DoubleMatrix1D
  type SparseAdjointVector = SparseDoubleMatrix1D
  // can customize
  var maxIter = 100
  import Adjoint.{algebra, dAlg}


  // generic optimizer, can sub in and out
  var optimizer: DifferentiableMultivariateOptimizer


  // where the physics comes in, must be implemented
  def dhdx(state: T, control: Control):  Option[AdjointMatrix]
  def djdx(state: T, control: Control): SparseAdjointVector
  def djdu(state: T, control: Control): AdjointVector
  def dhdu(state: T, control: Control): Option[AdjointMatrix]
  def dhdxT(state: T, control: Control): Option[AdjointMatrix] = None
  def getDhdxT(state: T, control: Control) = {
    getTransposeOrFallback(state, control, dhdxT, dhdx)
  }
  def getDhduT(state: T, control: Control) = {
    getTransposeOrFallback(state, control, dhduT, dhdu)
  }
  def dhduT(state: T, control: Control): Option[AdjointMatrix] = None

  // either let the user specify the transpose explicitly or the user specifies the regular one, and we transpose,
  // may be expensive not to go straight to transpose
  def getTransposeOrFallback[A<:DoubleMatrix2D](state: T,
                                control: Control,
                                transpose: (T, Control) => Option[A],
                                standard: (T, Control) => Option[A] ) = {
    transpose(state, control).getOrElse {new AdjointMatrix(dAlg.transpose(standard(state, control).get).toArray)}
  }


  def forwardSimulate(control: Control): T
  def objective(state: T, control: Control): Double

  // this is where the adjoint magic takes place, therefore leaving the coder to just take care of physics
  def gradient(control: Control): AdjointVector ={
    val state = forwardSimulate(control)
    val djduSln = djdu(state, control)
    val lambda = adjointVector(state, control)
    val dhduT = getDhduT(state, control)
    val right = dAlg.mult(dhduT, lambda)
    return djduSln.assign(right, DoubleFunctions.minus)
  }

  def adjointVector(state: T, control: Control): AdjointVector = {
    val dhdxTranspose = getDhdxT(state, control)
    val djdxSln = djdx(state, control)
    algebra.solve(dhdxTranspose, djdxSln)
  }


  // keep track of current state in mutable value, to allow for gradient to respond
  def solve(control0: Control): Control = {
    var currentState: T = forwardSimulate(control0)
    def updateState(control: Control) {
      currentState = forwardSimulate(control)
    }

    val outer = this

    val diffFunction = new DifferentiableMultivariateFunction {
      def gradient(): MultivariateVectorFunction = new MultivariateVectorFunction {
        def value(point: Array[Double]) = {
          outer.gradient(point).toArray
        }
      }
      def value(point: Array[Double]) = {
        updateState(point)
        val obj = objective(currentState, point)
        obj
      }

      def partialDerivative(k: Int) = null

    }

    // return final sln given from optimizer of your choosing
    optimizer.optimize(maxIter, diffFunction, GoalType.MINIMIZE, control0).getPoint
  }
}