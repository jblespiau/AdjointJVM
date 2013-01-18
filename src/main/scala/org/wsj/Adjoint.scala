package org.wsj

import org.ejml.simple.SimpleMatrix
import org.apache.commons.math3.optimization.{GoalType, DifferentiableMultivariateOptimizer}
import org.apache.commons.math3.analysis.{MultivariateVectorFunction, DifferentiableMultivariateFunction}


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
}
import Adjoint.Control

trait Adjoint[T<:SystemState] {
  var maxIter = 100

  val optimizer: DifferentiableMultivariateOptimizer

  def dhdx(state: T, control: Control):  SimpleMatrix
  def djdx(state: T, control: Control): SimpleMatrix
  def djdu(state: T, control: Control): SimpleMatrix
  def dhdu(state: T, control: Control): SimpleMatrix

  def forwardSimulate(control: Control): T

  def objective(state: T, control: Control): Double

  def gradient(control: Control): SimpleMatrix ={
    val state = forwardSimulate(control)
    (djdu(state, control).minus(adjointVector(state, control).transpose().mult(dhdu(state, control)))).transpose()
  }

  def adjointVector(state: T, control: Control): SimpleMatrix = dhdx(state, control).transpose().solve(djdx(state, control))

  def solve(control0: Control): Control = {
    var currentState: T = forwardSimulate(control0)
    def updateState(control: Control) {
      currentState = forwardSimulate(control)
    }

    val outer = this

    val diffFunction = new DifferentiableMultivariateFunction {
      def gradient(): MultivariateVectorFunction = new MultivariateVectorFunction {
        def value(point: Array[Double]) = {
          outer.gradient(point).getMatrix.getData
        }
      }
      def value(point: Array[Double]) = {
        updateState(point)
        val obj = objective(currentState, point)
        println(obj)
        obj
      }

      def partialDerivative(k: Int) = null

    }
    optimizer.optimize(maxIter, diffFunction, GoalType.MINIMIZE, control0).getPoint
  }
}
