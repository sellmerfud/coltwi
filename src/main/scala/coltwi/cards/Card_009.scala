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

object Card_009 extends Card(9, "Beni-Oui-Oui", Single, FlnMarked, AlwaysPlay)
{
  val beniOuiOui = (sp: Space) => sp.terror == 0 && sp.isNeutral

  override def botEventSelection(): EventSelection =
    if (game.hasAlgerianSpace(beniOuiOui))
      Unshaded
    else
      NoEvent

  override def executeUnshaded(role: Role): Unit = {
    // Set any two non-terrorized Neutral spaces to Support or Oppose
    if (role == Gov) {
      def affectNeutral(candidates: List[String], remaining: Int): Unit = {
        if (remaining > 0 && candidates.nonEmpty) {
          println()
          val name = askSpace("\nSelect a non-terrorized neutral space: ", candidates)
          val choices  = List(
            "support" -> s"Set ${displaySpace(name)} to Support",
            "oppose"  -> s"Set ${displaySpace(name)} to Opposition"
          )
          setSupport(name, if (askMenu(choices).head == "support") Support else Oppose)
          affectNeutral(candidates.filterNot(_ == name), remaining - 1)
        }
      }
      val candidates = algerianCandidates(beniOuiOui)
      if (candidates.isEmpty)
        log("No non-terrorized neutral spaces. The event has no effect")
      else
        affectNeutral(candidates, 2)
    }
    else { // Role == Fln
      def setOppose(candidates: List[Space], remaining: Int): Unit = {
        if (remaining > 0 && candidates.nonEmpty) {
          val sp = Bot.topPriority(candidates, OppositionPriorities)
          setSupport(sp.name, Oppose)
          setOppose(candidates.filterNot(_.name == sp.name), remaining - 1)
        }
      }
      setOppose(game.algerianSpaces.filter(beniOuiOui), 2)
    }
  }
  override def executeShaded(role: Role): Unit = () // Single event
}
