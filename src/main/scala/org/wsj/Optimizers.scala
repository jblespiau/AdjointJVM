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

class IpOptAdjointOptimizer extends Ipopt with DifferentiableMultivariateOptimizer {


  setStringOption(Ipopt.KEY_MU_STRATEGY,"adaptive")
  //hs071.setStringOption(Ipopt.KEY_OUTPUT_FILE,"hs071cpp.out");
  //hs071.setStringOption(Ipopt.KEY_PRINT_USER_OPTIONS,"yes");
  //setStringOption("nlp_scaling_method","usER-ScAling");//ignor case
  setStringOption(Ipopt.KEY_HESSIAN_APPROXIMATION,"lImIted-memory")//LBFGS
  setStringOption(Ipopt.KEY_DERIVATIVE_TEST,"first-order")
  setStringOption(Ipopt.KEY_PRINT_USER_OPTIONS,"yes")
  setStringOption("print_options_documentation","yes")

  var u0: Array[Double] = null
  var fn: DifferentiableMultivariateFunction = null

  protected def get_starting_point(n: Int, init_x: Boolean, x: Array[Double], init_z: Boolean, z_L: Array[Double], z_U: Array[Double], m: Int, init_lambda: Boolean, lambda: Array[Double]) = {
    u0 copyToArray x
    true
  }


  protected def get_bounds_info(n: Int, x_l: Array[Double], x_u: Array[Double], m: Int, g_l: Array[Double], g_u: Array[Double]) = {
    // lower_bound copyToArray x_l
    // upper_bound copyToArray x_u
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


  protected def eval_g(n: Int, x: Array[Double], new_x: Boolean, m: Int, g: Array[Double]) = false


  protected def eval_jac_g(n: Int, x: Array[Double], new_x: Boolean, m: Int, nele_jac: Int, iRow: Array[Int], jCol: Array[Int], values: Array[Double]) = false


  protected def eval_h(n: Int, x: Array[Double], new_x: Boolean, obj_factor: Double, m: Int, lambda: Array[Double], new_lambda: Boolean, nele_hess: Int, iRow: Array[Int], jCol: Array[Int], values: Array[Double]) = false

  def optimize(maxEval: Int, f: DifferentiableMultivariateFunction, goalType: GoalType, startPoint: Array[Double]) = {
    val n = startPoint.length
    fn = f
    u0 = startPoint
    create(n, 0, 0, 0, Ipopt.C_STYLE)
    OptimizeNLP() match {
      case 0 => new PointValuePair(getState, 0.0)
      case _ => throw new Exception("yes")
    }
  }

  def getMaxEvaluations = 100

  def getEvaluations = 100

  def getConvergenceChecker = new ConvergenceChecker[PointValuePair] {
    def converged(iteration: Int, previous: PointValuePair, current: PointValuePair) = false
  }
}
