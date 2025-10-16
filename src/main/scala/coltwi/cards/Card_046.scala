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

object Card_046 extends Card(46, "Moghazni", Dual, false, AlwaysPlay)
{
  val moghazniCriteria = (sp: Space) => {
    sp.isSector && sp.isSupport && sp.algerianPolice > 0 &&
    Bot.getGuerrillasToPlace(sp.algerianPolice, sp).total > 0
  }

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(moghazniCriteria))
      Shaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Rural self-defense: Until Propaganda round, Government may Train
    // in any Sector that is at Support and is Government controlled;
    // however, may place only Algerian Police.
    playMomentum(MoMoghazni)
  }
  override def executeShaded(role: Role): Unit = {
    // Force K: Replace up to all Algerian Police in any 1 sector with
    // an equal number of Guerrillas.
    val priorities = List(
      new Bot.HighestPriority[Space]("Most alerian police", _.algerianPolice),
      new Bot.HighestPriority[Space]("Most Fln pieces",     _.totalFln),
      new Bot.HighestPriority[Space]("Highest population",  _.population))
    val sp = Bot.topPriority(game.algerianSpaces.filter(moghazniCriteria), priorities)
    val guerrillas = Bot.getGuerrillasToPlace(sp.algerianPolice, sp)
    removeToAvailable(sp.name, Pieces(algerianPolice = guerrillas.total))
    Bot.placeGuerrillas(sp.name, guerrillas)
  }
}
