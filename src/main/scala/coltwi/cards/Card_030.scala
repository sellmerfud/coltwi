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

object Card_030 extends Card(30, "Change in Tactics", Single, false, AlwaysPlay)
{
  override def botEventSelection(): EventSelection =
    if (game.capabilities exists (_.startsWith("Gov:"))) Unshaded else NoEvent
  // Remove any 1 Capability marker that is in effect.
  // That capability no longer applies.
  override def executeUnshaded(role: Role): Unit = if (role == Gov) {
    val caps = (game.capabilities filter (_.startsWith("Dual:"))) :::
                (game.capabilities filter (_.startsWith("FLN:")))  :::
                (game.capabilities filter (_.startsWith("Gov:")))
    val index = caps.size match {
      case 0 => -1
      case 1 =>  0
      case _ =>
        val choices = caps.zipWithIndex map (_.swap)
        println("\nRemove which capability from play:")
        askMenu(choices).head
    }
    if (index == -1 )
      log("There are no capabilities in play.  The event has no effect")
    else
      removeCapabilityFromPlay(caps(index))
  }
  else {
    import scala.util.Random.shuffle
    val cap = shuffle(game.capabilities filter (_.startsWith("Gov:"))).head
    removeCapabilityFromPlay(cap)
  }
  override def executeShaded(role: Role): Unit = () // Single event
}
