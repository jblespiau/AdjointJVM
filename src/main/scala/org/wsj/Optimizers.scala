package org.wsj

import org.apache.commons.math3.optimization.{ConvergenceChecker, PointValuePair, GoalType, DifferentiableMultivariateOptimizer}
import org.apache.commons.math3.analysis.DifferentiableMultivariateFunction
import org.coinor.Ipopt

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/11/13
 * Time: 11:56 AM
 * To change this template use File | Settings | File Templates.
 */

class IpOptAdjointOptimizer extends DifferentiableMultivariateOptimizer {


  class IpOptAdjoint extends Ipopt {

    var u0: Array[Double] = null
    var fn: DifferentiableMultivariateFunction = null

    protected def get_starting_point(n: Int, init_x: Boolean, x: Array[Double], init_z: Boolean, z_L: Array[Double], z_U: Array[Double], m: Int, init_lambda: Boolean, lambda: Array[Double]) = {
      u0 copyToArray x
      true
    }


    protected def get_bounds_info(n: Int, x_l: Array[Double], x_u: Array[Double], m: Int, g_l: Array[Double], g_u: Array[Double]) = {
      for (i <- 0 until x_l.length) {
        x_l(i) = 0
        x_u(i) = 1000.0
      }
      true
    }


    protected def eval_f(n: Int, x: Array[Double], new_x: Boolean, obj_value: Array[Double]) = {
      obj_value(0) = fn.value(x)
      true
    }


    protected def eval_grad_f(n: Int, x: Array[Double], new_x: Boolean, grad_f: Array[Double]) = {
      fn.gradient().value(x) copyToArray grad_f
      true
    }


    protected def eval_g(n: Int, x: Array[Double], new_x: Boolean, m: Int, g: Array[Double]) = {
      true
    }


    protected def eval_jac_g(n: Int, x: Array[Double], new_x: Boolean, m: Int, nele_jac: Int, iRow: Array[Int], jCol: Array[Int], values: Array[Double]) = {
      true
    }


    protected def eval_h(n: Int, x: Array[Double], new_x: Boolean, obj_factor: Double, m: Int, lambda: Array[Double], new_lambda: Boolean, nele_hess: Int, iRow: Array[Int], jCol: Array[Int], values: Array[Double]) = {
      true
    }




  }

  def optimize(maxEval: Int, f: DifferentiableMultivariateFunction, goalType: GoalType, startPoint: Array[Double]) = {
    val ipopt = new IpOptAdjoint
    val n = startPoint.length
    ipopt.fn = f
    ipopt.u0 = startPoint
    ipopt.create(n, 0, 0, 0, Ipopt.C_STYLE)
    ipopt.setStringOption(Ipopt.KEY_MAX_ITER, getMaxEvaluations.toString)
    ipopt.setStringOption(Ipopt.KEY_MU_STRATEGY,"adaptive")
    ipopt.setStringOption(Ipopt.KEY_HESSIAN_APPROXIMATION,"limited-memory")//LBFGS
    val status = ipopt.OptimizeNLP()
    println("status")
    println(status)
    val somePoint = {
      if (status <= 0 || status >= 0)
        Some(new PointValuePair(ipopt.getState(), 0.0))
      else
        None
    }
    somePoint.getOrElse {
      throw new Exception("did not have proper status")
    }
  }

  def getMaxEvaluations = 100

  def getEvaluations = 10

  def getConvergenceChecker = new ConvergenceChecker[PointValuePair] {
    println("converged outer")
    def converged(iteration: Int, previous: PointValuePair, current: PointValuePair) = {
      println("converged")
      false
    }
  }

}
