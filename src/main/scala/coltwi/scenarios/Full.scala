
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

class Full extends Scenario {
  val name               = "Full: 1955-1962: Algerie Francaise!"
  val numberOfPropCards  = 4
  val resources          = Resources(gov = 16, fln = 8)
  val commitment         = 25
  val franceTrack        = franceTrackFromLetter('A')
  val borderZoneTrack    = 0
  val outOfPlay          = Pieces(frenchTroops = 6, frenchPolice = 15, govBases = 3)
  val pivotalCardsPlayed = Set.empty[Int]

  val spaces = List(
    // Wilaya I
    DefaultBarika,
    DefaultBatna.copy(
      pieces = Pieces(hiddenGuerrillas = 1, flnBases = 1)),
    DefaultBiskra,
    DefaultOumElBouaghi,
    DefaultTebessa,
    DefaultNegrine,
      
    // Wilaya II
    DefaultConstantine.copy(
      pieces  = Pieces(frenchTroops = 1, frenchPolice = 1, hiddenGuerrillas = 1)),
    DefaultSetif,
    DefaultPhilippeville.copy(
      support = Oppose,
      pieces  = Pieces(algerianTroops = 1, hiddenGuerrillas = 1, flnBases = 1)),
    DefaultSoukAhras,
                       
    // Wilaya III
    DefaultTiziOuzou.copy(
      support = Oppose,
      pieces  = Pieces(hiddenGuerrillas = 1, flnBases = 1)),
    DefaultBordjBouArreridj,
    DefaultBougie,
      
    // Wilaya IV
    DefaultAlgiers.copy(
      pieces  = Pieces(algerianTroops = 1, frenchPolice = 1, algerianPolice = 1)),
    DefaultMedea.copy(
      pieces  = Pieces(algerianPolice = 1, hiddenGuerrillas = 1)),
    DefaultOrleansVille,
      
    // Wilaya V
    DefaultOran.copy(
      pieces  = Pieces(algerianTroops = 1, frenchPolice = 1)),
    DefaultMecheria,
    DefaultTlemcen.copy(
      support = Oppose,
      pieces  = Pieces(hiddenGuerrillas = 1, flnBases = 1)),
    DefaultSidiBelAbbes.copy(
      pieces  = Pieces(frenchTroops = 1)),
    DefaultMostaganem.copy(
      pieces  = Pieces(hiddenGuerrillas = 1)),
    DefaultSaida,
    DefaultMascara,
    DefaultTiaret,
    DefaultAinSefra,
    DefaultLaghouat,
    
    // Wilaya VI
    DefaultSidiAissa,
    DefaultAinOussera,

    // Countries
    DefaultMorocco,
    DefaultTunisia
  )
}

