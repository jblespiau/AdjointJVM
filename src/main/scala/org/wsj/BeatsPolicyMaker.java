package org.wsj;


import edu.berkeley.path.beats.simulator.Scenario;

import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 3/20/13
 * Time: 3:16 PM
 * To change this template use File | Settings | File Templates.
 */




public interface BeatsPolicyMaker {
    public Map<Integer,Double[]> compute(
            Map<Integer, Double> initialDensity,
            Map<Integer,Double[]> splitRatios,
            Map<Integer,Double[]> rampDemands,
            Scenario scenario
    );
}

