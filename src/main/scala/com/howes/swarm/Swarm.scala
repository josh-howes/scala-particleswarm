package com.howes.swarm

import Particle.FitnessFunction

class Swarm(fitnessFunction: FitnessFunction, numberOfParticles: Int, searchSpaceBounds: Array[(Double, Double)]) {
  // Initialize the Particles
  val particles: Array[Particle] = generateRandomParticles(numberOfParticles, fitnessFunction, searchSpaceBounds)
  var bestParticle: Particle = particles reduce findBestParticle
  var globalBest: Array[Double] = bestParticle.localBestPosition
  var globalBestFitness: Double = fitnessFunction(globalBest)

  // TODO: Could I abstract here and make it possible to create swarms of swarms for example?
  def runOptimization(numberIterations: Int): Array[Double] = {

    for (i <- 1 to numberIterations) {
      for (particle <- particles) {
        particle.update(globalBest)
      }

      bestParticle = particles reduce findBestParticle
      if (bestParticle.localBestFitness < globalBestFitness) {
        globalBest = bestParticle.localBestPosition
        globalBestFitness = bestParticle.localBestFitness
      }
    }
    globalBest
  }

  // TODO: Make Particles sortable and move this logic into that class?
  private def findBestParticle(p1: Particle, p2: Particle): Particle = if (p1.localBestFitness < p2.localBestFitness) p1 else p2

  private def randomizeInitialState(searchSpaceBounds: Array[(Double, Double)]): Array[Double] = {
    import util.Random
    val rand = Random
    searchSpaceBounds map {case (lowerBound, upperBound) => rand.nextDouble() * (upperBound - lowerBound) + lowerBound}
  }

  private def generateRandomParticles(n: Int, fit: FitnessFunction, bounds: Array[(Double, Double)]): Array[Particle] = {
    val particles: Array[Particle] = Array.tabulate(n){case(_) => new Particle(fit, randomizeInitialState(bounds))}
    particles
  }

}
