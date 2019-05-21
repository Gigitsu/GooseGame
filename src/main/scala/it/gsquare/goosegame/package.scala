package it.gsquare

/**
  * created by gigitsu on 21/05/2019.
  */
package object goosegame {
  type Board = Map[String, Int]
  type BoardWithMessage = (Board, String)

  object Board {
    def apply(elems: (String, Int)*): Board = Map[String, Int](elems: _*)
  }
}
