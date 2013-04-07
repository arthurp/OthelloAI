package othello

case class TraceNode(
		score : Score, // The score of this node (derived from it's children, if it is an internal node).
					   // This may not actually be the best possible since it might have pruned.
		alpha : Score, // The lower bound on the score of this subtree.
		beta : Score, // The upper bound on the score of this subtree.
		maxmin : Boolean, // True if this node is maximizing
		descendants : Seq[TraceNode], // The descendants of this node from which the scores are calculated. Empty for leaves.
		prunedN : Int = 0, // The number of additional descendants that where not processed.
		position : GameEngine, // The board at this node i
		positionScore : Option[Score] = None // Optionally the score of the position. This will not equal score.
	) {
 
  def indent(n : Int, s : String) = {
    val ind = "> " * n
    s.lines.map(ind + _).mkString("\n")
  }

  override def toString = {
    indent(1,
	    "Score: %s%nAlpha: %s, Beta: %s, Max? %b, Pruned: %d%nPos. Score: %s%n%s%n%s".
	    	format(score, alpha.score, beta.score, maxmin, prunedN, positionScore, position,
	    	    descendants.map(_.toString).mkString("\n")) 
	    	)
  }
}