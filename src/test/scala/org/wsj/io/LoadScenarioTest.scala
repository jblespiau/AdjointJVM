package org.wsj.io

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.wsj.{Constants, DumbFreeway}

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 2/28/13
 * Time: 10:50 AM
 * To change this template use File | Settings | File Templates.
 */
class LoadScenarioTest extends FunSuite with ShouldMatchers {


  test("load samitha network and assure it has the correct dimensions") {
    val scenario = LoadScenario.loadScenario(Constants.samithaFN)
    val N = 2
    val T = 5
    val p0FirstLink = .9
    val fmL0 = .9
    val fmL1 = 1.0
    val w = 1.0

    val freeway = new DumbFreeway(scenario.links)

    freeway.fwLinks.length should be (N)
    scenario.bc.length should be (T)
    scenario.ic(freeway.fwLinks(0)).linkCount should be (p0FirstLink)
    freeway.fwLinks(0).fd.fMax should be (fmL0)
    freeway.fwLinks(1).fd.fMax should be (fmL1)
    freeway.fwLinks.foreach {_.fd.w should be (w)}
  }
}
