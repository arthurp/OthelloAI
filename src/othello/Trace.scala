package othello

import scala.xml.Elem
import scala.xml.PrettyPrinter

abstract class SearchTree {
  override def toString = {
    val pp = new PrettyPrinter(80, 2)
    pp.format(toXML)
  }

  def toXML : Elem
  
  val move : Option[Move] 
  val score : Score
}

/**
 * @param move - the move made at this level
 * @param score - The score of this node. The score is derived from it's children if it's an internal node
 * @param board - The game board before the move is made
 * @param maxin - True if the AI is currently returning the maximum score.
 * @param decendents - The descendants of this node from which the scores are calculated. Empty for leaf node.
 * @param positionScore - Optionally the score of the position. This will not equal score. 
 */
case class NaiveSearchTree(
    move : Option[Move], 
    score : Score,
    board : GameEngine,
    maxmin : Boolean,
    descendants : IndexedSeq[NaiveSearchTree], 
    positionScore : Option[Score] = None)
     extends SearchTree {
  def toXML : Elem = {
    <node maxmin={maxmin.toString} move={move.toString}><board>{board.toString}</board>{score.toXML} <children> {descendants.map(_.toXML)} </children> </node>
  }
}

/**
 * @see NaiveSearchTree
 * 
 * @param prunedN The number of additional descendants that where not processed.
 */
case class AlphaBetaSearchTree(
		score : Score, 
		alpha : Score, 
		beta : Score,
		maxmin : Boolean,
		descendants : Seq[AlphaBetaSearchTree], 
		prunedN : Int = 0,
		board : GameEngine,
		positionScore : Option[Score] = None
	) {
  def toXML : Elem = {
    <node maxmin={maxmin.toString} prunedN={prunedN.toString}><board>{board.toString}</board>{score.toXML +: descendants.map(_.toXML)}</node>
  }
}
