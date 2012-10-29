Random walk negative binomial: a model for persistent count series.
===

**What**: Poster presented at 2012 Peace Science meeting in Savannah, GA

**Date**: October 2012

**By**: Andreas Beger ([andreas.beger@duke.edu](mailto:andreas.beger@duke.edu)), Kentaro Fukumoto, and Will H. Moore

<a rel="license" href="http://creativecommons.org/licenses/by-nc/3.0/deed.en_US"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc/3.0/88x31.png" /></a><br /><span xmlns:dct="http://purl.org/dc/terms/" property="dct:title">Random walk negative binomial</span> by <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Andreas Beger, Kentaro Fukumomto, and Will Moore</span> is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/3.0/deed.en_US">Creative Commons Attribution-NonCommercial 3.0 Unported License</a>.

Please reference:  
Beger, Andreas. 2012. ``Random walk negative binomial model for persistent count series.'' Post at [andybeger.wordpress.com](http://andybeger.wordpress.com/2012/10/29/random-walk-negative-binomial-model-for-persistent-count-series/).

or cite:  
Beger, Andreas, Kentaro Fukumoto and Will H. Moore. 2012. “Event Count Time Series Models: A Bayesian State Space Investigation.” Working paper.

Summary
---

 * The origional poster pdf is included, as well as code needed to create all the graphics in it.
 * Code (R/JAGS) for the random walk negative binomial model (rw-nb).
 * Code to create simulated count time-series data. You can use this to test the rw-nb model estimates.

Creating your own simulated data
---

To create your own simulated data you can use the i1count function. Specify obs_type as either "poisson" or "nbinom", give end as how many time periods you want, initial mean for transition equation mu1, and proc.s and obs.s parameters for the transition and measurement equation variance respectively.