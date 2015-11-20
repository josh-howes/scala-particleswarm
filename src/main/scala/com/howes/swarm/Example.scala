package com.howes.swarm

// TODO: Parallelize using akka Agents

object Example extends App {

  println("Particle Swarm Optimization Example in Scala")
  //  A silly example of a fitness function minimizing at the origin.
  def fitnessFunction(xs: Array[Double]): Double = {
    import Math.abs
    val ys = Array(0.0, 0.0, 0.0, 0.0, 0.0)
    (xs zip ys map {case (a, b) => abs(a - b)}) reduce (_+_)
  }

  val x: Double = 100000.0
  // Array of (lowerBound, upperBound) tuples for each dimension
  val searchSpaceBounds: Array[(Double, Double)] = Array((-x, x), (-x, x), (-x, x), (-x, x), (-x, x))
  val numberOfParticles: Int = 1000
  val swarm: Swarm = new Swarm(fitnessFunction, numberOfParticles, searchSpaceBounds)

  val numberOfIterations = 100
  val optimized: Array[Double] = swarm.runOptimization(numberOfIterations)
  println(optimized.mkString(";"))  // Should be really close to origin
}
