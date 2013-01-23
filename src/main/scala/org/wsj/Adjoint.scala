package org.wsj

import org.ejml.simple.SimpleMatrix
import org.apache.commons.math3.optimization.{GoalType, DifferentiableMultivariateOptimizer}
import org.apache.commons.math3.analysis.{MultivariateVectorFunction, DifferentiableMultivariateFunction}
import org.wsj.PolicyMaker._


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
  type ControlPolicy = ProfilePolicy[MaxRampFlux, SimpleFreewayLink]

  // not useful right now, thinking ahead for how it interfaces with policy maker
  implicit def controlPolicyToControl(controlPolicy: ControlPolicy ): Control = {
    controlPolicy.flatMap {_.toList.sortBy {_._1.id}.map {_._2.flux}}.toArray
  }
}
import Adjoint.Control


trait Adjoint[T<:SystemState] {
  // can customize
  var maxIter = 100

  // generic optimizer, can sub in and out
  val optimizer: DifferentiableMultivariateOptimizer


  // where the physics comes in, must be implemented
  def dhdx(state: T, control: Control):  SimpleMatrix
  def djdx(state: T, control: Control): SimpleMatrix
  def djdu(state: T, control: Control): SimpleMatrix
  def dhdu(state: T, control: Control): SimpleMatrix
  def forwardSimulate(control: Control): T
  def objective(state: T, control: Control): Double

  // this is where the adjoint magic takes place, therefore leaving the coder to just take care of physics
  def gradient(control: Control): SimpleMatrix ={
    val state = forwardSimulate(control)
    (djdu(state, control).minus(adjointVector(state, control).transpose().mult(dhdu(state, control)))).transpose()
  }

  def adjointVector(state: T, control: Control): SimpleMatrix = dhdx(state, control).transpose().solve(djdx(state, control))


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

    // return final sln given from optimizer of your choosing
    optimizer.optimize(maxIter, diffFunction, GoalType.MINIMIZE, control0).getPoint
  }
}