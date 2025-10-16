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

object Card_006 extends Card(6, "Factionalism", Dual, false, false)
{
  override def botEventSelection(): EventSelection = Shaded
  override def executeUnshaded(role: Role): Unit = {
    // Friction: Either remove up to 3 Guerrillas in any 1 Wilaya to Available,
    // or move the France Track up to 2 boxes towards 'A'
    val guerrillaInAlgeria = game hasAlgerianSpace (_.totalGuerrillas > 0)
    val choice = if (game.franceTrack > 0 && guerrillaInAlgeria) {
      val choices = List(
        "france" -> "Shift the France track 2 boxes towards 'A'",
        "wilaya" -> "Remove up to 3 Guerrillas in any 1 Wilaya")
      println("\nChoose one:")
      askMenu(choices).head
    }
    else if (game.franceTrack > 0) "france"
    else if (guerrillaInAlgeria)   "wilaya"
    else "no-effect"

    choice match {
      case "france" =>
        decreaseFranceTrack(2 min game.franceTrack)
      case "wilaya" =>
        val withGuerrillas = (game.algerianSpaces filter (_.totalGuerrillas > 0) map (_.wilaya)).toSet
        val wilaya = askWilaya(allowed = withGuerrillas)
        val numToRemove = 3 min (game.wilayaSpaces(wilaya) map (_.totalGuerrillas)).sum
        println(s"\nRemove a total of $numToRemove guerrillas from spaces in Wilaya $wilaya")

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

        removeGuerrillas(numToRemove)
      case _        =>
        log("The event has no effect")
    }
  }

  override def executeShaded(role: Role): Unit = {
    // Lube: Free Rally in any 1 selectable space with a base.
    Bot.turnState = Bot.TurnState(
      specialActivityAllowed = false,
      freeOperation          = true,
      maxSpaces              = Some(1),
      onlyIn                 = spaceCandidates(_.flnBases > 0).toSet)
    Bot.ConsiderRally.execute() // Rally will only be used if it adds a base and/or shifts the France Track
  }
}
