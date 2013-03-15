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
    val solver = new IpOptAdjoint
    val n = startPoint.length
    solver.fn = f
    solver.u0 = startPoint
    solver.create(n, 0, 0, 0, Ipopt.C_STYLE)
    solver.setStringOption(Ipopt.KEY_MU_STRATEGY,"adaptive")
    solver.setStringOption(Ipopt.KEY_HESSIAN_APPROXIMATION,"limited-memory")
    solver.setIntegerOption(Ipopt.KEY_MAX_ITER, getMaxEvaluations)

    val status = solver.OptimizeNLP()
    println("status")
    println(status)
    val somePoint = {
      if (status <= 0 || status >= 0)
        Some(new PointValuePair(solver.getState(), 0.0))
      else
        None
    }
    somePoint.getOrElse {
      throw new Exception("did not have proper status")
    }
  }

  def getMaxEvaluations = 20

  def getEvaluations = 10

  val convergenceChecker = new MaxIterationConvergenceChecker(getMaxEvaluations)

  def getConvergenceChecker = convergenceChecker
}

class MaxIterationConvergenceChecker(maxIter: Int) extends ConvergenceChecker[PointValuePair] {
  def converged(iteration: Int, previous: PointValuePair, current: PointValuePair) = {
    iteration >= maxIter
  }
}

trait LineSearch {
  def searchAlongLine(f: DifferentiableMultivariateFunction,
                      gradient: Array[Double],
                      currentPoint: Array[Double],
                       iteration: Int = -1): Array[Double]
}



trait GradientDescentOptimizer extends DifferentiableMultivariateOptimizer with LineSearch {
  def getMaxEvaluations = 100

  def getEvaluations = 100

  def optimize(maxEval: Int, f: DifferentiableMultivariateFunction, goalType: GoalType, startPoint: Array[Double]) = {
    var iter = 1
    val maxIter = getMaxEvaluations
    var u = startPoint
    var prevCost = f.value(u)
    var break = false
    while (iter <= maxIter && !break) {
      iter+=1
      val grad = f.gradient().value(u)
      val nextU = searchAlongLine(f, grad, u, iter )
      val nextCost = f.value(nextU)
      val nextPoint = new PointValuePair(nextU, nextCost)
      val prevPoint = new PointValuePair(u, prevCost)
      if (getConvergenceChecker.converged(iter, prevPoint, nextPoint))
        break = true
      u = nextU
      prevCost = nextCost
      println("cost: " + nextCost)
    }
    new PointValuePair(u, prevCost)
  }

  val convergenceChecker = new MaxIterationConvergenceChecker(getMaxEvaluations)

  def getConvergenceChecker = convergenceChecker
}

class SimpleGradientDescentOptimizer extends GradientDescentOptimizer {

  val alpha = .1

  def searchAlongLine(f: DifferentiableMultivariateFunction,
                      gradient: Array[Double],
                      currentPoint: Array[Double],
                      iteration: Int): Array[Double] =  {
    val stepSize = alpha / iteration
    currentPoint.zip{gradient}.map{case (uVal, gVal) => math.max(uVal - stepSize*gVal, 0.0)}
  }
}

trait FeedItBackOptimizer extends DifferentiableMultivariateOptimizer {
  def update(): Unit
  val feedItIterations:Int
  val baseOptimizer: DifferentiableMultivariateOptimizer
  def getMaxEvaluations = baseOptimizer.getMaxEvaluations
  def getEvaluations = baseOptimizer.getEvaluations
  def getConvergenceChecker = baseOptimizer.getConvergenceChecker

  def optimize(maxEval: Int, f: DifferentiableMultivariateFunction, goalType: GoalType, startPoint: Array[Double]) = {
    var iter = 0
    var sln = startPoint
    while (iter < feedItIterations) {
      iter+=1
      sln = baseOptimizer.optimize(maxEval, f, goalType, sln).getPoint
      update()
    }
    new PointValuePair(sln, f.value(sln))
  }
}

trait UpdateROpFeedItBackOptimizer extends FeedItBackOptimizer {
  val rampMetering: AdjointRampMetering
  val scaleFactor: Double
  def update() {
    rampMetering.R*=scaleFactor
  }
}


class SimpleUpdateROptimizer(val rampMetering: AdjointRampMetering,
                             val scaleFactor: Double,
                             val feedItIterations: Int = 10) extends UpdateROpFeedItBackOptimizer {
  val baseOptimizer = new SimpleGradientDescentOptimizer
}
class IpOptUpdateROptimizer(val rampMetering: AdjointRampMetering,
                             val scaleFactor: Double,
                             val feedItIterations: Int = 10) extends UpdateROpFeedItBackOptimizer {
  val baseOptimizer = new IpOptAdjointOptimizer
}

