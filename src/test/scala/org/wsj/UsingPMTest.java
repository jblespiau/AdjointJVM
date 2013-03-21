package org.wsj;

import edu.berkeley.path.beats.simulator.Scenario;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 3/21/13
 * Time: 1:57 PM
 * To change this template use File | Settings | File Templates.
 */
public class UsingPMTest {

    public static void main(String[] args) {
        BeatsScalaAdjointPolicyMaker pm = new BeatsScalaAdjointPolicyMaker();
        Map<Integer, Double> ic = new HashMap<Integer, Double>();
        ic.put(-1, .18);
        ic.put(-2, .04);
        ic.put(1, .18);
        ic.put(2, 0.0);
        Map<Integer, Double[]> sr = new HashMap<Integer, Double[]>();
        Double[] srs = new Double[] {1.,1.,1.,1.,1.};
        sr.put(-1, srs);
        sr.put(-2, srs);
        Map<Integer, Double[]> dm = new HashMap<Integer, Double[]>();
        Double[] dm1 = new Double[] {.18,0.,0.,0.,0.};
        Double[] dm2 = new Double[] {0.0,0.,0.,0.,0.};
        dm.put(-1, dm1);
        dm.put(-2, dm2);
        // Scenario scenario = new Scenario("input.xml")
        // pm.compute(ic, sr, dm, scenario)
    }
}
