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
  val score : Option[Score]
  
  val time : Float
}

class Timer {
  val start = System.nanoTime()
  def time() = (System.nanoTime() - start) / (1000.0f*1000*1000)
}

object SearchTree {
  def time[A](f : => A) : (A, Float) = {
    val t = System.nanoTime()
    val r = f
    (r, System.nanoTime() - t)
  }
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
}

object NaiveSearchTree {
  case class Choice(
    thismove : Move, 
    thisScore : Score,
    board : GameEngine,
    maxmin : Boolean,
    descendants : IndexedSeq[NaiveSearchTree], 
    positionScore : Option[Score] = None,
    time : Float = -1)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="Choice" maxmin={maxmin.toString} move={thismove.toString} time={time.toString}><board>{board.toString}</board>{thisScore.toXML} <children> {descendants.map(_.toXML)} </children> </node>
    }
    val score = Some(thisScore)
    val move = Some(thismove)
  }
  case class SearchLimit(
    thisScore : Score,
    board : GameEngine,
    time : Float)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="SearchLimit" time={time.toString}><board>{ board.toString }</board>{ thisScore.toXML }</node>
    }
    
    val score = Some(thisScore)
    val move = None
  }
  case class GameOver(
    thisScore : Score,
    board : GameEngine,
    time : Float)
    extends NaiveSearchTree {
    def toXML: Elem = {
      <node kind="GameOver" time={time.toString}><board>{ board.toString }</board>{ thisScore.toXML }</node>
    }
    
    val score = Some(thisScore)
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
    val score : Option[Score]		
	val board : GameEngine
    val move : Option[Move]
    
    def compareTo(b: AlphaBetaSearchTree, maximizing : Boolean) : Boolean ={
      if( this.score == None ) false
	  else if( b.score == None ) true
	  else if(maximizing) (this.score.get > b.score.get)
	  else (b.score.get > this.score.get)
    }
}

object AlphaBetaSearchTree {
  
  def sortTrees(subTrees : Seq[AlphaBetaSearchTree], maximizing: Boolean) ={
    subTrees.sortWith((a, b) => a.compareTo(b,maximizing));
  }
  
  case class Choice(
    thismove : Move, 
    thisScore: Score,
    alpha: Score,
    beta: Score,
    maxmin: Boolean,
    descendants: Seq[AlphaBetaSearchTree],
    board: GameEngine,
    time : Float) 
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="Choice" maxmin={ maxmin.toString } 
      alpha={ alpha.score.toString } beta={ beta.score.toString} move={thismove.toString} time={time.toString}>
      <board>{ board.toString }</board>
      { score.get.toXML}      
      <children> { descendants.map(_.toXML) } </children>
      </node>
      }
    val move = Some(thismove)
    val score = Some(thisScore)
  }
  
  case class SearchLimit(
    thisScore : Score,
    board : GameEngine,
    time : Float)
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="SearchLimit" time={time.toString}><board>{ board.toString }</board>{ thisScore.toXML}</node>
    }
    
    val move = None
    val score = Some(thisScore)
    val descendants = IndexedSeq[NaiveSearchTree]()
  }  
   
  case class GameOver(
    thisScore : Score,
    board : GameEngine,
    time : Float)
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="GameOver" time={time.toString}><board>{ board.toString }</board>{ thisScore.toXML }</node>
    }
    
    val move = None
    val score = Some(thisScore)
  }

  case class Pruned(
    thismove : Move,
    board : GameEngine,
    time : Float)
    extends AlphaBetaSearchTree {
    def toXML: Elem = {
      <node kind="Pruned" move={thismove.toString} time={time.toString}><board>{ board.toString }</board></node>
    }
    val score = None
    val move = Some(thismove)
  }
}
