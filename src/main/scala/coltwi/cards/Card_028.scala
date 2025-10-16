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

object Card_028 extends Card(28, "Atrocities and Reprisals", Single, FlnMarked, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (game.resources(Fln) > 0 && game.terrorMarkersAvailable > 0 &&
        ((game.commitment > 0 && game.hasAlgerianSpace(!_.isOppose)) || game.hasAlgerianSpace(_.isSupport)))
      Unshaded
    else
      NoEvent

  // Executor of Event may place up to 2 Terror markers, placed
  // after paying 1 resource per marker, in any spaces in Algeria
  // (even if terror already present)
  // Set these spaces to Neutral, -1 Commitment for each terror marker
  // placed (no matter who executed the event)
  override def executeUnshaded(role: Role): Unit = if (role == Gov) {
    if (game.resources(Gov) == 0)
      log(s"$Gov has no resources.  The event has no effect")
    else if (game.terrorMarkersAvailable == 0)
      log(s"There are no available terror markers.  The event has no effect")
    else {
      def nextMarker(remaining: Int): Unit = if (remaining > 0) {
        val name = askSpace("\nSelect space for terror marker: ", spaceNames(game.algerianSpaces).sorted)
        decreaseResources(Gov, 1)
        addTerror(name, 1)
        setSupport(name, Neutral)
        decreaseCommitment(1)
        nextMarker(remaining - 1)
      }
      val num = if ((game.terrorMarkersAvailable min game.resources(Gov)) == 1) 1
      else askInt("Place how many terror markers", 1, 2)
      nextMarker(num)
    }
  }
  else { // role == Fln
    val priorities = List(
      new Bot.BooleanPriority[Space]("Is at support",      _.isSupport),
      new Bot.HighestPriority[Space]("Highest population", _.population))

    def placeTerror(selected: Set[String], remaining: Int): Unit = {
      if (remaining > 0 && game.resources(Fln) > 0 && game.terrorMarkersAvailable > 0) {
        val criteria = if (game.commitment > 0)
          (sp: Space) => !sp.isOppose
        else
          (sp: Space) => sp.isSupport

        (game.algerianSpaces filter criteria) match {
          case Nil =>
          case candidates =>
            val sp = Bot.topPriority(candidates, priorities)
            decreaseResources(Fln, 1)
            addTerror(sp.name, 1)
            setSupport(sp.name, Neutral)
            decreaseCommitment(1)
            placeTerror(selected + sp.name, remaining - 1)
        }
      }
    }
    placeTerror(Set.empty, 2)
  }
  override def executeShaded(role: Role): Unit = () // Single event
}
