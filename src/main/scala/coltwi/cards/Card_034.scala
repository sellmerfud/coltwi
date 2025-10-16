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

object Card_034 extends Card(34, "Elections", Dual, FlnMarked, AlwaysPlay)
{
  val elections = (sp: Space) => sp.isSector && sp.population > 0 && sp.isSupport

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(elections))
      Shaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Integrationist mandate: +1 Commitment or set 1 non-terrorized
    // Neutral space to Support
    val candidates = algerianCandidates(sp => sp.isNeutral && sp.terror == 0 && sp.population > 0)
    val choice = if (game.commitment == EdgeTrackMax && candidates.isEmpty)
      "none"
    else if (game.commitment == EdgeTrackMax)
      "space"
    else if (candidates.isEmpty)
      "commit"
    else {
      println("\nChoose one:")
      askMenu(List("commit" -> "Increase commitment by 1",
                    "space"  -> "Set non-terrorized Neutral space to Support")).head
    }
    choice match {
      case "commit" => increaseCommitment(1)
      case "space"  => setSupport(askSpace("\nSelect space: ", candidates), Support)
      case _        => log("The event has no effect")
    }
  }

  override def executeShaded(role: Role): Unit = {
    // Voter Supression: Set 1 sector to Neutral
    val priorities = List(
      new Bot.HighestPriority[Space]("Highest population", _.population),
      new Bot.BooleanPriority[Space]("Gov cannot train",  !_.canTrain))

    val candidates = game.algerianSpaces.filter(elections)
    if (candidates.nonEmpty) {
      val sp = Bot.topPriority(candidates, priorities)
      setSupport(sp.name, Neutral)
    }
  }
}
