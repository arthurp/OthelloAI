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
abstract class NaiveSearchTree extends SearchTree {
    val board : GameEngine
    val positionScore : Option[Score]
}

object NaiveSearchTree {
  case class Choice(
    thismove : Move, 
    score : Score,
    board : GameEngine,
    maxmin : Boolean,
    descendants : IndexedSeq[NaiveSearchTree], 
    positionScore : Option[Score] = None)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="Choice" maxmin={maxmin.toString} move={thismove.toString}><board>{board.toString}</board>{score.toXML} <children> {descendants.map(_.toXML)} </children> </node>
    }
    
    val move = Some(thismove)
  }
  case class SearchLimit(
    score : Score,
    board : GameEngine)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="SearchLimit"><board>{ board.toString }</board>{ score.toXML }</node>
    }
    
    val positionScore = Some(score)
    val move = None
  }
  case class GameOver(
    score : Score,
    board : GameEngine)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="GameOver"><board>{ board.toString }</board>{ score.toXML }</node>
    }
    
    val positionScore = Some(score)
    val move = None
  }

}

/**
 * @see NaiveSearchTree
 * 
 * @param alpha the lower bound on the resulting score
 * @param beta the upper bound of the resulting score
 * @param prunedN The number of additional descendants that where not processed.
 */
abstract class AlphaBetaSearchTree extends SearchTree {
    val board : GameEngine
    val positionScore : Option[Score]
}

object AlphaBetaSearchTree {
  case class Choice(
    thismove : Move, 
    score: Score,
    alpha: Score,
    beta: Score,
    maxmin: Boolean,
    descendants: Seq[AlphaBetaSearchTree],
    prunedN: Int = 0,
    board: GameEngine,
    positionScore: Option[Score] = None) 
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="Choice" maxmin={ maxmin.toString } prunedN={ prunedN.toString } alpha={ alpha.toString } beta={ beta.toString}><board>{ board.toString }</board>{ score.toXML +: descendants.map(_.toXML) }</node>
    }
    val move = Some(thismove)
  }
  
  case class SearchLimit(
    score : Score,
    board : GameEngine)
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="SearchLimit"><board>{ board.toString }</board>{ score.toXML }</node>
    }
    
    val positionScore = Some(score)
    val move = None
  }
  case class GameOver(
    score : Score,
    board : GameEngine)
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="GameOver"><board>{ board.toString }</board>{ score.toXML }</node>
    }
    
    val positionScore = Some(score)
    val move = None
  }

}
