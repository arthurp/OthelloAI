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
  
  def max(x : Score) = if(score > x.score) this else x  
  def min(x : Score) = if(score < x.score) this else x  
}

object Score {
  val MaxValue = Score(Float.MaxValue)
  val MinValue = Score(Float.MinValue)  
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
    for( Move(x,y) <- b.allLegalMoves(p) ) {
       if( positionScores(x)(y) > 0 ) 
         score += positionScores(x)(y) 
    }
    for( Move(x,y) <- b.allLegalMoves(p.otherPlayer)) {
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
    val trace = scoreLookaheadAlphaBeta(heuristic, b, lookahead, Score.MinValue, Score.MaxValue, b.currentTurn)
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
  
  def scoreLookaheadNaive(heuristic : Heuristic, b:GameEngine, lookahead:Int, topPlayer : PlayerCellState) : NaiveSearchTree ={
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)
    
    if(currentLegalMoves.isEmpty) {
    	val bestScore = if( b.leadingPlayer == topPlayer ) {
    		Score(b.score(topPlayer))
    	} else {
    		Score(-b.score(topPlayer.otherPlayer))    		  
    	}      
    	// We want this to be a root node
    	NaiveSearchTree.GameOver(bestScore, b)
    } else if(lookahead == 0) {
    	val score = Score(heuristic(b, topPlayer), computeAllHeuristics(b, topPlayer))
    	NaiveSearchTree.SearchLimit(score, b)
    } else {
    	val childrenResults = for( m@Move(mx,my) <- currentLegalMoves ) yield {
    	  val searchTree = scoreLookaheadNaive(heuristic, b.makeMove(mx,my,b.currentTurn), lookahead - 1, topPlayer)
    	  (m, searchTree)
    	}
    	val sortedChildren = childrenResults.sortWith(
    		(x,y) => 
    			if(maximizing) x._2.score > y._2.score
    			else x._2.score < y._2.score
    		);
    	val (m,chosenMove) = sortedChildren.head
	    NaiveSearchTree.Choice(m, chosenMove.score, b, maximizing, sortedChildren.unzip._2)

    }          
  } 
  
  /*
   * Alpha = the lower bound on the resulting score
   * Beta = the upper bound of the resulting score
   */
  def scoreLookaheadAlphaBeta(heuristic : Heuristic, b : GameEngine, lookahead : Int, 
		  					_alpha : Score, _beta : Score, topPlayer : PlayerCellState) : AlphaBetaSearchTree = {
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)

    if(currentLegalMoves.isEmpty) {
    	val bestScore = if( b.leadingPlayer == topPlayer ) {
    		Score(b.score(topPlayer))
    	} else {
    		Score(-b.score(topPlayer.otherPlayer))    		  
    	}      
    	// We want this to be a root node
    	AlphaBetaSearchTree.GameOver(bestScore, b)
    } else if(lookahead == 0) {
    	val score = Score(heuristic(b, topPlayer), computeAllHeuristics(b, topPlayer))
    	AlphaBetaSearchTree.SearchLimit(score, b)
    } else {
    	val childrenResults = for( m@Move(mx,my) <- b.allLegalMoves(b.currentTurn) ) yield {
    	  val searchTree = scoreLookaheadNaive(heuristic, b.makeMove(mx,my,b.currentTurn), lookahead - 1, topPlayer)
    	  (m, searchTree)
    	}
    	val sortedChildren = childrenResults.sortWith(
    		(x,y) => 
    			if(maximizing) x._2.score > y._2.score
    			else x._2.score < y._2.score
    		);
    	val (m,chosenMove) = sortedChildren.head
	    NaiveSearchTree.Choice(m, chosenMove.score, b, maximizing, sortedChildren.unzip._2)
	    
     	if( b.currentTurn == topPlayer ) {
     	  /* Another way to do this. This is incorrect, but surves as a sketch.
     		val beta = _beta
     		def loop(alpha : Score, l : Seq[Move]) : AlphaBetaSearchTree = l match {
     		  case Move(mx, my) :: rest => {
	     		  val st = scoreLookaheadAlphaBeta(heuristic, b.makeMove(mx, my, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		  if( beta <= alpha || rest.isEmpty )
	     		    st
	     		  else
	     			loop(alpha max st.score, rest)
	     	  }
     		}
     		
     		loop(_alpha, currentLegalMoves)
     		*/
     		
     	  /* Another way that might be cleaner
     		currentLegalMoves.foldRight(AlphaBetaSearchTree.Choice(null, null, _alpha, _beta, maximizing, Nil, 0, b)) {
     			(m, st) =>
	     		  if( st.beta <= st.alpha ) {
	     		    st
	     		  } else {
	     		    val child = scoreLookaheadAlphaBeta(heuristic, b.makeMove(m.x, m.y, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		    AlphaBetaSearchTree.Choice(m, child.score, b, maximizing, st
	     		  }
     		}
     		*/
     		
     	  // Arthur: I think this is easier for most people to understand. But I don't really like it. Oh well.
     		var alpha = _alpha
     		var ret = AlphaBetaSearchTree.Choice(null, null, _alpha, _beta, maximizing, Nil, 0, b)
     		val beta = _beta
	     	breakable {
	     	  for( m@Move(mx, my) <- currentLegalMoves ) {
	     		  val child = scoreLookaheadAlphaBeta(heuristic, b.makeMove(mx, my, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		  alpha = alpha max child.score
	     		  ret = AlphaBetaSearchTree.Choice(
	     				  			m, child.score, alpha, beta, maximizing, 
	     				  			child +: ret.descendants, 0, b)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
     		assert(ret != null)
	     	ret
     	} else {
     	  // Arthur: I want this to share most of it's code with the one above. Can you see a way?
     		val alpha = _alpha
     		var ret = AlphaBetaSearchTree.Choice(null, null, _alpha, _beta, maximizing, Nil, 0, b)
     		var beta = _beta
	     	breakable {
	     	  for( m@Move(mx, my) <- currentLegalMoves ) {
	     		  val child = scoreLookaheadAlphaBeta(heuristic, b.makeMove(mx, my, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		  beta = beta min child.score
	     		  ret = AlphaBetaSearchTree.Choice(
	     				  			m, child.score, alpha, beta, maximizing, 
	     				  			child +: ret.descendants, 0, b)
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
     		assert(ret != null)
	     	ret
     	}
    }                    
  } 
  
}
