package othello
import scala.util.control.Breaks

import scala.math.Ordered

object AI {
  type Heuristic = (GameEngine, PlayerCellState) => Float
}

case class Score(score : Float, heuristics : Map[String, Float] = Map()) extends Ordered[Score] {
  def compare(o : Score) = {
    score compareTo o.score
  }
  
  override def toString = {
    "%s (%s)".format(score, heuristics)
  }
}

class AI {
  val mybreaks = new Breaks
  import mybreaks.{break, breakable}
  import AI._
  
  implicit def heuristic2Rich(h1 : Heuristic) = new {
    def +(h2 : Heuristic) : Heuristic = (b, p) => h1(b, p) + h2(b, p)
    def *(h2 : Heuristic) : Heuristic = (b, p) => h1(b, p) * h2(b, p)
  }
  implicit def constantHeuristic(v : Float) : Heuristic = (b, p) => v
  implicit def constantHeuristic(v : Double) : Heuristic = (b, p) => v.floatValue()
    
  val mobility1 : Heuristic = (b, p) => {
    (b.allLegalMoves(p).size - b.allLegalMoves(p.otherPlayer).size) / 6
  }
  
  val positionScores : Seq[Seq[Float]] = 
		  			   Seq(Seq(99,  -8,  8,  6,  6,  8,  -8, 99), 
		  				   Seq(-8, -24,  1 , 2,  2,  1, -24, -8),
		  				   Seq( 8,   1,  7,  4,  4,  7,   1,  8),
		  				   Seq( 6,   2,  4,  0,  0,  4,   2,  6),
		  				   Seq( 6,   2,  4,  0,  0,  4,   2,  6),
		  				   Seq( 8,   1,  7,  4,  4,  7,   1,  8),
		  				   Seq(-8, -24,  1,  2,  2,  1, -24, -8),
		  				   Seq(99,  -8,  8,  6,  6,  8,  -8, 99))

  val positional : Heuristic = (b, p) => {
    (for( (c, s) <- b.board.flatten.zip(positionScores.flatten) ) yield {
      if( c == p )
        s
      else if( c == p.otherPlayer )
        -s
      else 
        0
    }).sum / 100
  }
  
  val mobility2 : Heuristic = (b, p) => {
    var score : Float =0.0f; 
    for( (x,y) <- b.allLegalMoves(p) ) {
       if( positionScores(x)(y) > 0 ) 
         score += positionScores(x)(y) 
    }
    for( (x,y) <- b.allLegalMoves(p.otherPlayer)) {
       if( positionScores(x)(y) > 0 ) 
    	 score -= positionScores(x)(y)
    }
    score / 500
  }
  
  val heuristic = mobility1 + (positional * 2.0) + mobility2 * 1.5
  
  def makeMove(b : GameEngine) = {
    val lookahead = 3
    makeMoveInternal(b,lookahead)
  }
  
  val ninf = Float.NegativeInfinity
  val inf = Float.PositiveInfinity
  
  def makeMoveInternal(b : GameEngine, lookahead : Int) = {
    def scoreMove(x : Int, y : Int) = {
      //scoreLookaheadAlphaBeta(heuristic, x,y,b,lookahead, ninf, inf, b.currentTurn)
      val (s, trace) = scoreLookaheadNaive(heuristic, x,y,b,lookahead, b.currentTurn)
      //println(trace)
      s
    }
    val moves = for((x,y) <- b.allLegalMoves(b.currentTurn)) yield
      			    ((x, y), scoreMove(x, y))
    moves.minBy(_._2)._1
  }
  
  def scoreNaive(heuristic : Heuristic, x:Int, y:Int, b:GameEngine) ={
    val clone = b.copy()
    clone.makeMove(x,y,b.currentTurn) 
    heuristic(clone, b.currentTurn)
  } 
  
  val allHeuristics = Seq(("mobility1", mobility1), ("mobility2", mobility2), ("positional", positional))
  
  def computeAllHeuristics(b : GameEngine, p : PlayerCellState) : Map[String, Float] = {
    Map() ++ allHeuristics.map(v => (v._1, v._2(b, p)))
  }
  
  def scoreLookaheadNaive(heuristic : Heuristic, x:Int, y:Int, b:GameEngine, lookahead:Int, topPlayer : PlayerCellState) : (Score, TraceNode) ={
    val clone = b.copy()
    clone.makeMove(x, y, b.currentTurn) 
    
    val maximizing = topPlayer == clone.currentTurn // maximize the next possible moves if we will be making the choice
    if(lookahead == 0) {
    	val s = heuristic(clone, topPlayer)
    	val score = Score(s, computeAllHeuristics(clone, topPlayer))
    	(score, TraceNode(score, score, score, maximizing, Seq(), 0, clone, Some(score)))
    } else {
    	val childrenResults = for( (mx, my) <- clone.allLegalMoves(clone.currentTurn) ) yield {
    	  scoreLookaheadNaive(heuristic, mx, my, clone, lookahead - 1, topPlayer)
    	}
    	val (children, traces) = childrenResults.unzip
    	val s = if( !children.isEmpty ) {
	    	if( maximizing )
	    	  children.max
	    	else
	    	  children.min
    	} else {
    		if( clone.leadingPlayer == topPlayer ) {
    		  Score(clone.score(topPlayer))
    		} else {
    		  Score(-clone.score(topPlayer.otherPlayer))    		  
    		}
    	}
    	
    	(s, TraceNode(s, s, s, maximizing, traces, 0, clone, None))
    }          
  } 
  
  /*
   * Alpha = the lower bound on the resulting score
   * Beta = the upper bound of the resulting score
   */
  def scoreLookaheadAlphaBeta(heuristic : Heuristic, x : Int, y : Int, b : GameEngine, lookahead : Int, 
		  					_alpha : Float, _beta : Float, maxPlayer : PlayerCellState) : Float ={
    val clone = b.copy() 
    clone.makeMove(x, y, b.currentTurn) 
    if(lookahead == 0) {
    	val s = heuristic(clone, maxPlayer)
    	s
    } else {
     	var alpha = _alpha
     	var beta = _beta
     	val moves = clone.allLegalMoves(clone.currentTurn)
     	if( moves.isEmpty ) {
     	  // Game has ended
    		if( clone.leadingPlayer == maxPlayer ) {
    		  clone.score(maxPlayer)
    		} else {
    		  -clone.score(maxPlayer.otherPlayer)    		  
    		}
     	} else if( clone.currentTurn == maxPlayer ) {
	     	breakable {
	     	  for( (mx, my) <- moves ) {
	     		  alpha = alpha max scoreLookaheadAlphaBeta(heuristic, mx, my, clone, lookahead - 1, alpha, beta, maxPlayer)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
	     	alpha
     	} else {
	     	breakable {
	     	  for( (mx, my) <- moves ) {
	     		  beta = beta min scoreLookaheadAlphaBeta(heuristic, mx, my, clone, lookahead - 1, alpha, beta, maxPlayer)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
	     	beta
     	}
    }          
  } 
  
}