
// Colonial Twilight
//
// An scala implementation of the solo AI for the game 
// Falling Sky The Gallic Revolt Against Caesar, 
// designed by Andrew Ruhnke and Volko Ruhnke
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

package coltwi.scenarios

import coltwi.ColonialTwilight._

class Short extends Scenario {
  val name               = "Short: 1960-1962: The End Game"
  val resources          = Resources(gov = 20, fln = 15)
  val commitment         = 15
  val franceTrack        = franceTrackFromLetter('E')
  val borderZoneTrack    = 3
  val outOfPlay          = Pieces(hiddenGuerrillas = 5)
  val pivotalCardsPlayed = Set(PivotalMobilization, PivotalRecallDeGaulle, PivotalMoroccoTunisiaIndepdent)

  val spaces = List(
    DefaultBarika.copy(
      support = Oppose, 
      pieces  = Pieces(algerianPolice = 1, hiddenGuerrillas = 1)),
    DefaultBatna,
    DefaultBiskra.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultOumElBouaghi,
    DefaultTebessa.copy(
      support = Oppose,
      pieces  = Pieces(algerianPolice = 1, hiddenGuerrillas = 1)),
    DefaultNegrine.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultConstantine.copy(
      support = Support,
      pieces  = Pieces(frenchPolice = 1)),
    DefaultSetif.copy(
      pieces  = Pieces(hiddenGuerrillas = 1),
      markers = List(ResettledMarker)),
    DefaultPhilippeville.copy(
      pieces  = Pieces(frenchTroops = 4, algerianPolice = 1, govBases = 1)),
    DefaultSoukAhras.copy(
      support = Oppose,
      pieces  = Pieces(frenchTroops = 1, algerianPolice = 1, govBases = 1,
                       hiddenGuerrillas = 1, flnBases = 1)),
    DefaultTiziOuzou.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, hiddenGuerrillas = 1, flnBases = 1)),
    DefaultBordjBouArreridj.copy(
      pieces  = Pieces(frenchPolice = 1),
      markers = List(ResettledMarker)),
    DefaultBougie.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, hiddenGuerrillas = 1, flnBases = 1)),
    DefaultAlgiers.copy(
      support = Support,
      pieces  = Pieces(frenchTroops = 4, algerianTroops = 1, frenchPolice = 1)),
    DefaultMedea.copy(
      pieces  = Pieces(algerianTroops = 1, govBases = 1)),
    DefaultOrleansVille.copy(
      support = Oppose,
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1, hiddenGuerrillas = 1, flnBases = 1)),
    DefaultOran.copy(
      support = Support,
      pieces  = Pieces(algerianTroops = 1, algerianPolice = 1, frenchPolice = 1)),
    DefaultMecheria.copy(
      pieces  = Pieces(frenchPolice = 1, algerianPolice = 1)),
    DefaultTlemcen.copy(
      pieces  = Pieces(frenchPolice = 2, hiddenGuerrillas = 1),
      markers = List(ResettledMarker)),
    DefaultSidiBelAbbes.copy(
      pieces  = Pieces(frenchPolice = 1, govBases = 1)),
    DefaultMostaganem.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultSaida,
    DefaultMascara.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultTiaret.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultAinSefra.copy(
      pieces  = Pieces(frenchPolice = 1)),
    DefaultLaghouat,
    DefaultSidiAissa,
    DefaultAinOussera.copy(
      support = Oppose),
    DefaultMorocco.copy(
      pieces  = Pieces(hiddenGuerrillas = 4, flnBases = 2)),
    DefaultTunisia.copy(
      pieces  = Pieces(hiddenGuerrillas = 5, flnBases = 2))
  )
}

