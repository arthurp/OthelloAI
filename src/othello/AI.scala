package othello
import scala.util.control.Breaks
import scala.math._
import scala.collection.mutable.ListBuffer
import scala.xml.Elem
import java.io.File

abstract class Heuristic {
  val logLen = 100
  val log = ListBuffer[Float](1)
  
  def apply(g : GameEngine, p : PlayerCellState) : Float = {
    val v = compute(g, p)
    if( log.length > logLen ) log.remove(logLen)
    v +=: log
    val avg = averageValue
    if( avg == 0.0f ) v 
    else v / avg 
  }
  
  def averageValue : Float = sqrt(log.map(pow(_, 2)).sum / log.length).floatValue()
  
  def compute(g : GameEngine, p : PlayerCellState) : Float = throw new Exception()
}

object AI {
}

case class Score(score : Float, heuristics : Map[String, Float] = Map()) extends Ordered[Score] {
  def compare(o : Score) = {
    score compareTo o.score
  }
  
  override def toString = {
    "%s (%s)".format(score, heuristics)
  }
  
  def toXML : Elem = { 
    <score value={score.toString}>
     	{ for((heuristic, value) <- heuristics ) yield <heuristic name={heuristic} value={value.toString} />}
    </score>	
  }
}

class AI {
  val mybreaks = new Breaks
  import mybreaks.{break, breakable}
  import AI._
  
  implicit def heuristic2Rich(h1 : Heuristic) = new {
    def +(h2 : Heuristic) : Heuristic = new Heuristic { 
      override def apply(b : GameEngine, p : PlayerCellState) = h1(b, p) + h2(b, p) 
    }
    def *(h2 : Heuristic) : Heuristic = new Heuristic {
      override def apply(b : GameEngine, p : PlayerCellState)  = h1(b, p) * h2(b, p)
    }
  }
  implicit def constantHeuristic(v : Float) : Heuristic =  new Heuristic {
    override def apply(b : GameEngine, p : PlayerCellState)  = v
  }
  implicit def constantHeuristic(v : Double) : Heuristic = new Heuristic {
    override def apply(b : GameEngine, p : PlayerCellState)  = v.floatValue()
  }  
  val mobility1 : Heuristic = new Heuristic {
      override def compute(b : GameEngine, p : PlayerCellState)  = {
    (b.allLegalMoves(p).size - b.allLegalMoves(p.otherPlayer).size)
  } }
  
  val positionScores : Seq[Seq[Float]] = 
		  			   Seq(Seq(99,  -8,  8,  6,  6,  8,  -8, 99), 
		  				   Seq(-8, -24,  1 , 2,  2,  1, -24, -8),
		  				   Seq( 8,   1,  7,  4,  4,  7,   1,  8),
		  				   Seq( 6,   2,  4,  0,  0,  4,   2,  6),
		  				   Seq( 6,   2,  4,  0,  0,  4,   2,  6),
		  				   Seq( 8,   1,  7,  4,  4,  7,   1,  8),
		  				   Seq(-8, -24,  1,  2,  2,  1, -24, -8),
		  				   Seq(99,  -8,  8,  6,  6,  8,  -8, 99))

  val positional = new Heuristic {
      override def compute(b : GameEngine, p : PlayerCellState)  =  {
    (for( (c, s) <- b.board.flatten.zip(positionScores.flatten) ) yield {
      if( c == p )
        s
      else if( c == p.otherPlayer )
        -s
      else 
        0
    }).sum 
  } }
  
  val mobility2 = new Heuristic {
      override def compute(b : GameEngine, p : PlayerCellState)  = {
    var score : Float =0.0f; 
    for( (x,y) <- b.allLegalMoves(p) ) {
       if( positionScores(x)(y) > 0 ) 
         score += positionScores(x)(y) 
    }
    for( (x,y) <- b.allLegalMoves(p.otherPlayer)) {
       if( positionScores(x)(y) > 0 ) 
    	 score -= positionScores(x)(y)
    }
    score
  } }
  
  val heuristic = mobility1 + (positional * 30.0) + mobility2 * 1.5
  
  def makeMove(b : GameEngine) = {
    val lookahead = 3
    makeMoveInternal(b,lookahead)
  }
  
  val ninf = Float.NegativeInfinity
  val inf = Float.PositiveInfinity
  
  def makeMoveInternal(b : GameEngine, lookahead : Int) = {
    val trace = scoreLookaheadNaive(heuristic, b, lookahead, b.currentTurn)
    Utils.printToFile(new File("trace.xml"))(p => p.println(trace))
    trace.move.get
  }
  
  def scoreNaive(heuristic : Heuristic, x:Int, y:Int, b:GameEngine) ={
    heuristic(b.makeMove(x,y,b.currentTurn), b.currentTurn)
  } 
  
  val allHeuristics = Seq(("mobility1", mobility1), ("mobility2", mobility2), ("positional", positional))
  
  def computeAllHeuristics(b : GameEngine, p : PlayerCellState) : Map[String, Float] = {
    Map() ++ allHeuristics.map(v => (v._1, v._2(b, p)))
  }
  
  def scoreLookaheadNaive(heuristic : Heuristic, b:GameEngine, lookahead:Int, topPlayer : PlayerCellState) : TraceNode ={
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)
    
    if(currentLegalMoves.isEmpty) {
    	val bestScore = if( b.leadingPlayer == topPlayer ) {
    		Score(b.score(topPlayer))
    	} else {
    		Score(-b.score(topPlayer.otherPlayer))    		  
    	}      
    	// We want this to be a root node
    	TraceNode(None, bestScore, b, maximizing, Seq(), Some(bestScore))
    } else if(lookahead == 0) {
    	val scores = currentLegalMoves.map(move => {
     	  val newb = b.makeMove(move._1,move._2,b.currentTurn)
    	  (move, Score(heuristic(b, topPlayer), computeAllHeuristics(newb, topPlayer)), newb)
    	  })
    	val (bestMove, bestScore, bestBoard) = if( maximizing ) scores.maxBy(_._2) else scores.minBy(_._2);
    	// TODO: Fix trace generation
    	TraceNode(Some(bestMove), bestScore, b, maximizing, Seq(), Some(bestScore))
    } else {
    	val childrenResults = for( (mx, my) <- b.allLegalMoves(b.currentTurn) ) yield {
    	  val trace = scoreLookaheadNaive(heuristic, b.makeMove(mx,my,b.currentTurn), lookahead - 1, topPlayer)
    	  TraceNode(Some((mx, my)), trace.score, trace.position, trace.maxmin,trace.descendants)
    	}
   	    // TODO sort the child nodes
    	val traceNode = if( maximizing )
	    	  childrenResults.maxBy(_.score)
	    	else
	    	  childrenResults.minBy(_.score)
    	TraceNode(traceNode.move, traceNode.score, traceNode.position, maximizing, childrenResults)
    }          
  } 
  
  /*
   * Alpha = the lower bound on the resulting score
   * Beta = the upper bound of the resulting score
   */
  def scoreLookaheadAlphaBeta(heuristic : Heuristic, x : Int, y : Int, b : GameEngine, lookahead : Int, 
		  					_alpha : Float, _beta : Float, maxPlayer : PlayerCellState) : Float ={
    val newb = b.makeMove(x, y, b.currentTurn) 
    if(lookahead == 0) {
    	val s = heuristic(newb, maxPlayer)
    	s
    } else {
     	var alpha = _alpha
     	var beta = _beta
     	val moves = newb.allLegalMoves(newb.currentTurn)
     	if( moves.isEmpty ) {
     	  // Game has ended
    		if( newb.leadingPlayer == maxPlayer ) {
    		  newb.score(maxPlayer)
    		} else {
    		  -newb.score(maxPlayer.otherPlayer)    		  
    		}
     	} else if( newb.currentTurn == maxPlayer ) {
	     	breakable {
	     	  for( (mx, my) <- moves ) {
	     		  alpha = alpha max scoreLookaheadAlphaBeta(heuristic, mx, my, newb, lookahead - 1, alpha, beta, maxPlayer)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
	     	alpha
     	} else {
	     	breakable {
	     	  for( (mx, my) <- moves ) {
	     		  beta = beta min scoreLookaheadAlphaBeta(heuristic, mx, my, newb, lookahead - 1, alpha, beta, maxPlayer)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
	     	beta
     	}
    }          
  } 
  
}
