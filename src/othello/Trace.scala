package othello

import scala.xml.Elem
import scala.xml.PrettyPrinter

/**
 * @param move - the move made at this level
 * @param score - The score of this node. The score is derived from it's children if it's an internal node
 * @param board - The game board after the move was made
 * @param maxin - True if the AI is currently returning the maximum score.
 * @param decendents - The descendants of this node from which the scores are calculated. Empty for leaf node.
 * @param positionScore - Optionally the score of the position. This will not equal score. 
 */
case class SearchTree(
    move : Option[(Int, Int)], 
    score : Score, 
    board : GameEngine,
    maxmin : Boolean,
    descendants : IndexedSeq[SearchTree] , 
    positionScore : Option[Score] = None 
	) {
 
  def indent(n : Int, s : String) = {
    val ind = "> " * n
    s.lines.map(ind + _).mkString("\n")
  }

  override def toString = {
    /*indent(1,
	    "Score: %s%nAlpha: %s, Beta: %s, Max? %b, Pruned: %d%nPos. Score: %s%n%s%n%s".
	    	format(score, alpha.score, beta.score, maxmin, prunedN, positionScore, position,
	    	    descendants.map(_.toString).mkString("\n")) 
	    	)*/
    val pp = new PrettyPrinter(80, 2)
    pp.format(toXML)
  }
  
  def toXML : Elem = {
    <node maxmin={maxmin.toString} move={move.toString}><board>{board.toString}</board>{score.toXML +: descendants.map(_.toXML)}</node>
  }
}

case class TraceNodeAlphaBeta(
		score : Score, // The score of this node (derived from it's children, if it is an internal node).
					   // This may not actually be the best possible since it might have pruned.
		alpha : Score, // The lower bound on the score of this subtree.
		beta : Score, // The upper bound on the score of this subtree.
		maxmin : Boolean, // True if this node is maximizing
		descendants : Seq[SearchTree], // The descendants of this node from which the scores are calculated. Empty for leaves.
		prunedN : Int = 0, // The number of additional descendants that where not processed.
		position : GameEngine, // The board at this node i
		positionScore : Option[Score] = None // Optionally the score of the position. This will not equal score.
	) {
 
  def indent(n : Int, s : String) = {
    val ind = "> " * n
    s.lines.map(ind + _).mkString("\n")
  }

  override def toString = {
    /*indent(1,
	    "Score: %s%nAlpha: %s, Beta: %s, Max? %b, Pruned: %d%nPos. Score: %s%n%s%n%s".
	    	format(score, alpha.score, beta.score, maxmin, prunedN, positionScore, position,
	    	    descendants.map(_.toString).mkString("\n")) 
	    	)*/
    val pp = new PrettyPrinter(80, 2)
    pp.format(toXML)
  }
  
  def toXML : Elem = {
    <node maxmin={maxmin.toString}><board>{position.toString}</board>{score.toXML +: descendants.map(_.toXML)}</node>
  }
}