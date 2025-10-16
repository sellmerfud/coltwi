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

object Card_007 extends Card(7, "5th Bureau", Dual, FlnMarked, AlwaysPlay)
{
  val fifthBureau = (sp: Space) => sp.isSector && sp.population > 0 && !sp.isOppose

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(fifthBureau))
      Shaded
    else
      NoEvent
  override def executeUnshaded(role: Role): Unit = {
    // Psychological warfare: Choose 1 Wilaya and roll 1d6;
    // if roll <= total guerrillas in wilaya, remove up to the number
    // rolled from that Wilaya to available.
    val withGuerrillas = game.algerianSpaces
      .filter(_.totalGuerrillas > 0)
      .map(_.wilaya)
      .toSet

    if (withGuerrillas.isEmpty)
      log("The event has no effect")
    else {
      val wilaya = askWilaya(allowed = withGuerrillas)
      val numGs  = game.wilayaSpaces(wilaya)
        .map(_.totalGuerrillas)
        .sum
      val die = dieRoll
      log()
      log(s"Number of guerrillas in Wilaya $wilaya: $numGs")
      log(s"Die roll: $die")
      if (die > numGs)
        log("The event has no effect")
      else {
        println(s"\nRemove a total of $die guerrillas from spaces in Wilaya $wilaya")

        def removeGuerrillas(remaining: Int): Unit = {
          val candidates = wilayaCandidates(wilaya)(_.totalGuerrillas > 0)
          if (remaining > 0  && candidates.nonEmpty) {
            val name = askSpace(s"Select space in Wilaya $wilaya: ", candidates)
            val sp = game.getSpace(name)
            val num = askInt(s"Remove how many guerrillas from ${sp.nameAndZone}", 0, remaining min sp.totalGuerrillas)
            if (num > 0) {
              val guerrillas = askPieces(sp.pieces, num, GUERRILLAS)
              removeToAvailable(name, guerrillas)
            }
            removeGuerrillas(remaining - num)
          }
        }

        removeGuerrillas(die)
      }
    }
  }
  override def executeShaded(role: Role): Unit = {
    // Propaganda flop: Shift any 2 Sectors 1 level each towards Opposition.
    def shiftOppose(candidates: List[Space], remaining: Int): Unit = {
      if (remaining > 0 && candidates.nonEmpty) {
        val sp = Bot.topPriority(candidates, OppositionPriorities)
        decreaseSupport(sp.name, 1)
        shiftOppose(candidates.filterNot(_.name == sp.name), remaining - 1)
      }
    }
    shiftOppose(game.algerianSpaces.filter(fifthBureau), 2)
  }
}
