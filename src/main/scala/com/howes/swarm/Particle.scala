package com.howes.swarm

object Particle {

  type FitnessFunction = Array[Double] => Double

  // Make some of the update logic of the particles a bit easier to read.  Basically adding the ability
  //  to do basic element-wise scalar operations on an Array as well as pair-wise ops given two arrays
  class AlgebraArray(x: Array[Double]) {

    def +(d: Double): Array[Double] = x map (_ + d)
    def +(xs: Array[Double]): Array[Double] = x zip xs map { case(a, b) => a + b}
    def -(d: Double): Array[Double] = x map (_ - d)
    def -(xs: Array[Double]): Array[Double] = x zip xs map { case(a, b) => a - b}
    def *(d: Double): Array[Double] = x map (_ * d)
    def *(xs: Array[Double]): Array[Double] = x zip xs map { case(a, b) => a * b}
    def /(d: Double): Array[Double] = x map (_ / d)
    def /(xs: Array[Double]): Array[Double] = x zip xs map { case(a, b) => a / b}
  }

  implicit def arrayToAlgebraArray(x: Array[Double]): AlgebraArray = new AlgebraArray(x)
}

class Particle(fitnessFunction: Particle.FitnessFunction, initialPosition: Array[Double],
               velocityWeight: Double = 0.25, localWeight: Double = 1.5, globalWeight: Double = 2.0) {

  import Particle._
  import util.Random

  val rand = Random
  val numberOfDimensions = initialPosition.size
  var currPosition = initialPosition  // Current Position
  var velocity = Array.fill(numberOfDimensions)(0.0)  // Current velocity
  var localBestPosition = currPosition // Current best
  var localBestFitness: Double = fitnessFunction(currPosition)

  /**
   *  Update the particle position with the next iteration
   */
  def update(globalBestPosition: Array[Double]): Unit = {
    /* This is basically a stochastic weighted average of current velocity and the difference between the local and
     *  global best answers.
     *
     * See: Particle.AlgebraArray to see how adding some implicit operation made this a bit easier with the arrays
     */
    val newVelocity = (velocity * velocityWeight) +
      ((localBestPosition - currPosition) * localWeight * rand.nextDouble) +
      ((globalBestPosition - currPosition) * globalWeight * rand.nextDouble)

    velocity = newVelocity
    currPosition = velocity + currPosition

    val currFitness: Double = fitnessFunction(currPosition)
    if (currFitness < localBestFitness) {
      localBestFitness = currFitness
      localBestPosition = currPosition
    }
  }
}

