//   _____      _             _       _   _______       _ _ _       _     _
//  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
// | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
// | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
// | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
//  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
//                                                             __/ |
//                                                            |___/
// A scala implementation of the solo AI for the game
// Colonial Twilight, designed by Brian Train and
// Published by GMT Games
//
// Copyright (c) 2017 Curt Sellmer
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

package coltwi.cards

import coltwi.ColonialTwilight._
import Card._
import coltwi.{ Human, Bot }

object Card_004 extends Card(4, "Oil & Gas Discoveries", Single, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent  // Bot never plays this event
  override def executeUnshaded(role: Role): Unit = {
    // Executor of the Event my add up to +2 Commitment. French cubes
    // equal to twice the Commitment added are removed from the map or
    // Available to out of play (Gov player's choice which cubes go)
    val frenchCubes   = game.availablePieces.frenchCubes + game.totalOnMap(_.frenchCubes)
    val maxNum        = 2 min (frenchCubes / 2) min (EdgeTrackMax - game.commitment)
    val numCommitment = askInt("Add how much commitment", 0, maxNum)
    val numCubes      = numCommitment * 2
    if (numCommitment == 0)
      log("No commitment added.")
    else {
      increaseCommitment(numCommitment)
      def removeCubes(remaining: Int): Unit = if (remaining > 0) {
        val frenchCubesAvail = game.availablePieces.frenchCubes
        val frenchCubesOnMap = game.totalOnMap(_.frenchCubes)
        println()
        println(s"You must remove $remaining French cubes to out of play")
        val choice = {
          if (frenchCubesAvail == 0)      "map"
          else if (frenchCubesOnMap == 0) "avail"
          else {
            val choices = List("avail" -> "Remove cubes from the available box",
                                "map"   -> "Remove cubes from the map")
            println("\nChoose one:")
            askMenu(choices).head
          }
        }

        val numRemoved = if (choice == "avail") {
          val num = askInt("Remove how many total cubes from available", 0, game.availablePieces.frenchCubes min remaining)
          if (num > 0) {
            val cubes = askPieces(game.availablePieces, num, FRENCH_CUBES)
            movePiecesFromAvailableToOutOfPlay(cubes)
          }
          num
        }
        else {
          val name = askSpace("\nSelect space to remove french cubes: ", spaceCandidates(_.frenchCubes > 0))
          val sp = game.getSpace(name)
          val num = askInt(s"Remove how many total cubes from ${sp.nameAndZone}", 0, sp.frenchCubes min remaining)
          if (num > 0) {
            val cubes = askPieces(sp.pieces, num, FRENCH_CUBES)
            removeToOutOfPlay(name, cubes)
          }
          num
        }
        removeCubes(remaining - numRemoved)
      }

      removeCubes(numCubes)
    }
  }
  override def executeShaded(role: Role): Unit = () // Single event
}
