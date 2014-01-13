package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val initiallyInfected: Int = 3

    val roomRows: Int = 8
    val roomColumns: Int = 8

    val maxDaysBeforeMove: Int = 5
    val daysBeforeSick: Int = 6
    val daysBeforeDead: Int = 14
    val daysBeforeImmune: Int = 16
    val daysBeforeHealthy: Int = 18

    val dieRate: Double = 0.25
    val transRate: Double = 0.4

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = List.range(0, population) map { new Person(_) }
  persons.take(initiallyInfected) foreach { _.becomeInfected }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    scheduleMove

    def scheduleMove {
      afterDelay(randomBelow(maxDaysBeforeMove + 1)) {
        if (!dead)
        {
          performMove
          scheduleMove
        }
      }
    }

    def performMove {
      val moves = getFeasibleMoves
      if (!moves.isEmpty)
      {
        val randomMove = Random.shuffle(moves).head
        randomMove()
        if (!infected)
        {
          val infectiousPersons = personsAt(row, col) filter { p => p.infected }
          if (!infectiousPersons.isEmpty && random < transRate)
          {
            becomeInfected
          }
        }
      }
    }

    def becomeInfected {
      infected = true
      afterDelay(daysBeforeSick) {
        sick = true
      }
      afterDelay(daysBeforeDead) {
        if (random < dieRate)
        {
          dead = true
        }
      }
      afterDelay(daysBeforeImmune) {
        if (!dead)
        {
          immune = true
        }
      }
      afterDelay(daysBeforeHealthy) {
        if (!dead)
        {
          infected = false
          sick = false
          immune = false
        }
      }

    }

    def getFeasibleMoves: List[() => Unit] =
    {
      val directions = List((-1, 0), (1, 0), (0, -1), (0, 1))

      directions
        .map { direction =>
          val newRow = (row + direction._1 + roomRows) % roomRows
          val newCol = (col + direction._2 + roomColumns) % roomColumns
          (newRow, newCol)
        }
        .filter { move =>
          personsAt(move._1, move._2).filter { p => p.sick || p.dead }.isEmpty
        }
        .map { move => () => {
            row = move._1
            col = move._2
          }
        }
    }
  }

  def personsAt(row: Int, col: Int): List[Person] = {
    persons.filter {
      p => p.row == row && p.col == col
    }
  }

}
