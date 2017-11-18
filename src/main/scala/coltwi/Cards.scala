
// Colonial Twilight
//
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

package coltwi

import ColonialTwilight._

object Cards {
  val Single = false
  val Dual   = true

  // Convenience method for adding a card to the deck.
  private def entry(card: Card) = (card.number -> card)
  
  val deckMap: Map[Int, Card] = Map(
    // ------------------------------------------------------------------------
    entry(new Card(1, "Quadrillage", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(2, "Balky Conscripts", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(3, "Leadership Snatch", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(4, "Oil & Gas Discoveries", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(5, "Peace of the Brave", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(6, "Factionalism", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(7, "5th Bureau", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(8, "Cross-border air strike", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(9, "Beni-Oui-Oui", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(10, "Moudjahidine", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(11, "Bananes", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(12, "Ventilos", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(13, "SAS", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(14, "Protest in Paris", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(15, "Jean-Paul Sarte", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(16, "NATO", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(17, "Commandos", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(18, "Torture", Single, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(19, "General Strike", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(20, "Suave qui peut", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(21, "United Nations Resolution", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(22, "The Government of USA is Convinced...", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(23, "Diplomatic Leanings", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(24, "Economic Development", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(25, "Purge", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(26, "Casbah", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(27, "Covert Movement", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(28, "Atrocities and Reprisals", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(29, "The Call Up", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(30, "Change in Tactics", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(31, "Intimidation", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(32, "Teleb the Bomb-maker", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(33, "Overkill", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(34, "Elections", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(35, "Napalm", Dual, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(36, "Assassination", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(37, "Integration", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(38, "Economic Crisis in France", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(39, "Retreat into Djebel", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(40, "Strategic Movement", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(41, "Egypt", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(42, "Czech Arms Deal", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(43, "Refugees", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(44, "Paranoia", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(45, "Challe Plan", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(46, "Moghazni", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(47, "Third Force", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(48, "Ultras", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(49, "Factional Plot", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(50, "Bleuite", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(51, "Stripey Hole", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(52, "Cabinet Shuffle", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(53, "Population Control", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(54, "Operation 744", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(55, "Development", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(56, "Hardened Attitudes", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(57, "Peace Talks", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(58, "Army in Waiting", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(59, "Bandung Conference", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(60, "Soummam Conference", Dual, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(61, "Morocco and Tunisia Independent", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(62, "Suez Crisis", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(63, "OAS", Single, false, true,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(64, "Mobilization", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(65, "Recall De Gaulle", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(66, "Coup d'etat", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(67, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(68, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(69, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(70, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    )),
    entry(new Card(71, "Propaganda!", Single, false, false,
      () => NoEvent,
      (role: Role) => (),
      (role: Role) => ()
    ))
  )
}

