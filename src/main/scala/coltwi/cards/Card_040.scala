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

object Card_040 extends Card(40, "Strategic Movement", Dual, false, false)
{
  override def botEventSelection(): EventSelection = NoEvent  // Bot will never use this event
  override def executeUnshaded(role: Role): Unit = {
    // Sea Lift: May redistribute up to 6 cubes among
    // any 3 coastal spaces for free.
    def movableCubes(sp: Space) = sp.pieces.only(CUBES) - Human.movingGroups(sp.name)
    def totalMoved = Human.movingGroups.toList.foldLeft(0) { case (sum, (_, p)) => sum + p.totalCubes }

    def selectSpaces(num: Int, total: Int, candidates: List[String]): List[String] = {
      if (num > total)
        Nil
      else {
        val name = askSpace(s"\nSelect ${ordinal(num)} coastal space: ", candidates)
        name :: selectSpaces(num + 1, total, candidates filterNot (_ == name))
      }
    }

    def nextChoice(selectedNames: List[String]): Unit = {
      val selectedSpaces = spaces(selectedNames)
      val sourceCandidates = selectedSpaces filter (movableCubes(_).total > 0)
      val choices = List(
        choice(totalMoved < 6 && sourceCandidates.nonEmpty, "cube",  s"Select a cubes to move"),
        choice(true,                                        "done",  s"Finished moving cubes"),
        choice(true,                                        "abort", s"Abort the entire $Gov turn")
      ).flatten

      val avail = sourceCandidates.sortBy(_.name) map { sp =>
        s"${sp.nameAndZone} (${sp.pieces.only(CUBES) - Human.movingGroups(sp.name)})"
      }
      val moved = Human.movingGroups.toList.sortBy(_._1) map { case (n, p) => s"$n ($p)"}
      println("\n")
      println("Cubes available to move:")
      println(separator())
      wrap("  ", avail) foreach println

      println("\n")
      println("Cubes already to moved:")
      println(separator())
      wrap("  ", moved) foreach println

      println("\nChoose one:")
      askMenu(choices, allowAbort = false).head match {
        case "cube" =>
          val savedState = game
          try {
            val source = askSpace(s"\nSelect source space: ", spaceNames(sourceCandidates).sorted)
            val dest   = askSpace(s"Select destination space: ", (selectedNames filterNot (_ == source)))
            val sp     = game.getSpace(source)
            val most   = (6 - totalMoved) min movableCubes(sp).total
            val num    = askInt(s"Move how many cubes from ${displaySpace(sp)} to ${displaySpace(dest)}", 0, most)
            val p      = askPieces(movableCubes(sp), num, CUBES)
            movePieces(p, source, dest)
            Human.movingGroups.add(dest, p)
          }
          catch {
            case AbortAction =>
              println(s"\n>>>> Aborting cube movement <<<<")
              println(separator())
              displayGameStateDifferences(game, savedState)
              game = savedState
          }
          nextChoice(selectedNames)

        case "abort" =>
          if (askYorN("Really abort? (y/n) ")) throw AbortAction
          nextChoice(selectedNames)

        case "done" =>
      }
    }

    val numSpaces = askInt("How many coastal spaces do you wish to select", 2, 3)
    val selectedNames = selectSpaces(1, numSpaces, algerianCandidates(_.coastal))
    nextChoice(selectedNames)
  }
  override def executeShaded(role: Role): Unit = ()
}
