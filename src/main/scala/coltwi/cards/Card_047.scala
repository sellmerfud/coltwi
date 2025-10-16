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

object Card_047 extends Card(47, "Third Force", Dual, FlnMarked, AlwaysPlay)
{
  val thirdForceCriteria = (sp: Space) =>
    sp.isSector &&
    sp.population > 0 &&
    sp.algerianPolice > 0 &&
    !sp.isOppose

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(thirdForceCriteria))
      Shaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Rally dissident insurgents: Replace up to 3 Guerrillas in 1 Sector
    // with an equal number of Algerian Police.
    val candidates = algerianCandidates(sp => sp.isSector && sp.totalGuerrillas > 0)
    if (candidates.isEmpty)
      log("The event has no effect")
    else {
      val name = askSpace("\nSelect sector with guerrillas: ", candidates)
      val guerrillas = askPieces(game.getSpace(name).pieces, 3, GUERRILLAS,
                              Some(s"Select guerrillas to remove from ${displaySpace(name)}"))
      removeToAvailable(name, guerrillas)
      val police = askPiecesToPlace(name, List(AlgerianPolice), guerrillas.total)
      placePieces(name, police)
    }
  }
  override def executeShaded(role: Role): Unit = {
    // Private army mistreats civilians: Set any 1 Sector with Algerian
    // Police in it to Opposition
    val sp = Bot.topPriority(game.algerianSpaces.filter(thirdForceCriteria), OppositionPriorities)
    setSupport(sp.name, Oppose)
  }
}
