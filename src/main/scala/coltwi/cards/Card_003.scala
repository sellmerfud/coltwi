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

object Card_003 extends Card(3, "Leadership Snatch", Dual, FlnMarked, AlwaysPlay)
{
  val leadershipSnatch = (sp: Space) => sp.population > 0 && sp.isFlnControlled && !sp.isOppose

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(leadershipSnatch))
      Shaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Gotcha: Activate all Guerrillas in 1 Wilaya
    val wilaya = askWilaya("\nSelect Wilaya to activate guerrillas: ")
    println()
    for (sp <- game.wilayaSpaces(wilaya))
      activateHiddenGuerrillas(sp.name, sp.hiddenGuerrillas)
  }
  override def executeShaded(role: Role): Unit = {
      // Widespread rage: Set up to 2 FLN-Controlled spaces to Oppose.
    def setOppose(candidates: List[Space], remaining: Int): Unit = {
      if (remaining > 0 && candidates.nonEmpty) {
        val sp = Bot.topPriority(candidates, OppositionPriorities)
        setSupport(sp.name, Oppose)
        setOppose(candidates filterNot (_.name == sp.name), remaining - 1)
      }
    }
    setOppose(game.algerianSpaces.filter(leadershipSnatch), 2)
  }
}
