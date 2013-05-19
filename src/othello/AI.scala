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
  
  val heuristic = mobility1 + (positional * 2) + (mobility2 * 4)
  
  def makeMove(b : GameEngine) = {
    val lookahead = 5
    makeMoveInternal(b,lookahead)
  }
  
  val ninf = Float.NegativeInfinity
  val inf = Float.PositiveInfinity
  
  def makeMoveInternal(b : GameEngine, lookahead : Int) = {
    val traceab = scoreLookaheadAlphaBeta(heuristic, b, lookahead, Score.MinValue, Score.MaxValue, b.currentTurn)
    //val tracenaive = scoreLookaheadNaive(heuristic, b, lookahead, b.currentTurn)
    Utils.printToFile(new File("traceab.xml"))(p => p.println(traceab))
    //Utils.printToFile(new File("tracenaive.xml"))(p => p.println(tracenaive))
    
    /*if( traceab.move != tracenaive.move ) {
      println(s"Moves differ: ${traceab.move} (ab) ${tracenaive.move} (naive)")
    }
    */
    traceab.move.get
  }
  
  def scoreNaive(heuristic : Heuristic, x:Int, y:Int, b:GameEngine) ={
    heuristic(b.makeMove(x,y,b.currentTurn), b.currentTurn)
  } 
  
  val allHeuristics = Seq(("mobility1", mobility1), ("mobility2", mobility2), ("positional", positional))
  
  def computeAllHeuristics(b : GameEngine, p : PlayerCellState) : Map[String, Float] = {
    Map() ++ allHeuristics.map(v => (v._1, v._2(b, p)))
  }
  
  def scoreLookaheadNaive(heuristic : Heuristic, b:GameEngine, lookahead:Int, topPlayer : PlayerCellState) : NaiveSearchTree ={
    val timer = new Timer()
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)
    
    if(currentLegalMoves.isEmpty) {
    	val bestScore = if( b.leadingPlayer == topPlayer ) {
    		Score(b.score(topPlayer))
    	} else {
    		Score(-b.score(topPlayer.otherPlayer))    		  
    	}      
    	// We want this to be a root node
    	NaiveSearchTree.GameOver(bestScore, b, timer.time)
    } else if(lookahead == 0) {
    	val score = Score(heuristic(b, topPlayer), computeAllHeuristics(b, topPlayer))
    	NaiveSearchTree.SearchLimit(score, b, timer.time)
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
	    NaiveSearchTree.Choice(m, chosenMove.score, b, maximizing, sortedChildren.unzip._2, time = timer.time)

    }          
  } 
  
  /*
   * Alpha = the lower bound on the resulting score
   * Beta = the upper bound of the resulting score
   */
  def scoreLookaheadAlphaBeta(heuristic : Heuristic, b : GameEngine, lookahead : Int, 
		  					_alpha : Score, _beta : Score, topPlayer : PlayerCellState) : AlphaBetaSearchTree = {
    val timer = new Timer()
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)

    if (currentLegalMoves.isEmpty) {
      val bestScore = if (b.leadingPlayer == topPlayer) {
        Score(b.score(topPlayer))
      } else {
        Score(-b.score(topPlayer.otherPlayer))
      }
      // We want this to be a root node
      AlphaBetaSearchTree.GameOver(bestScore, b, timer.time)
    } else if (lookahead == 0) {
    	val score = Score(heuristic(b, topPlayer), computeAllHeuristics(b, topPlayer))
    	AlphaBetaSearchTree.SearchLimit(score, b, timer.time)      
    } else {
      // The inductive case where pruning occurs. Here be dragons.
      // TODO: Sorting could occur here for better pruning, it has to be done based on a very simple metric
      def generatePrunedList(ms : Seq[Move], b : GameEngine) = ms.map(m => AlphaBetaSearchTree.Pruned(m, b, 0))
      
      if( maximizing ) {
        def loop(ms : Seq[Move], currentFavoredChild : Option[(Move, AlphaBetaSearchTree)], alpha : Score, beta : Score, processedChildren : List[AlphaBetaSearchTree]) : AlphaBetaSearchTree.Choice = {
          val m = ms.head
          val searchTree = scoreLookaheadAlphaBeta(
        		  			heuristic, b.makeMove(m.x, m.y, b.currentTurn), lookahead-1,
        		  			alpha, beta, topPlayer)
          val newalpha = alpha max searchTree.score
          val newcurrentFavoredChild = if( currentFavoredChild.isEmpty || alpha < searchTree.score ) Some((m, searchTree)) else currentFavoredChild
          if(newalpha >= beta || ms.tail.isEmpty) {
            val Some((chosenMove, chosenTree)) = newcurrentFavoredChild
            AlphaBetaSearchTree.Choice(chosenMove, chosenTree.score, newalpha, beta, maximizing, 
                (processedChildren :+ searchTree) ++ generatePrunedList(ms.tail, b) sortWith (_.score > _.score), ms.tail.length, b, time = timer.time)
          } else {
            loop(ms.tail, newcurrentFavoredChild, newalpha, beta, processedChildren :+ searchTree)
          }
        }
        
        loop(currentLegalMoves, None, _alpha, _beta, List())
      } else {
        def loop(ms : Seq[Move], currentFavoredChild : Option[(Move, AlphaBetaSearchTree)], alpha : Score, beta : Score, processedChildren : List[AlphaBetaSearchTree]) : AlphaBetaSearchTree.Choice = {
          val m = ms.head
          val searchTree = scoreLookaheadAlphaBeta(
        		  			heuristic, b.makeMove(m.x, m.y, b.currentTurn), lookahead-1,
        		  			alpha, beta, topPlayer)
          val newbeta = beta min searchTree.score
          val newcurrentFavoredChild = if( currentFavoredChild.isEmpty || beta > searchTree.score ) Some((m, searchTree)) else currentFavoredChild
          if(alpha >= newbeta || ms.tail.isEmpty) {
            val Some((chosenMove, chosenTree)) = newcurrentFavoredChild
            AlphaBetaSearchTree.Choice(chosenMove, chosenTree.score, alpha, newbeta, maximizing, 
                (processedChildren :+ searchTree) ++ generatePrunedList(ms.tail, b) sortWith (_.score < _.score), ms.tail.length, b, time = timer.time)
          } else {
            loop(ms.tail, newcurrentFavoredChild, alpha, newbeta, processedChildren :+ searchTree)
          }
        }
        
        loop(currentLegalMoves, None, _alpha, _beta, List())
        
      }
    }
  }
  
  def scoreLookaheadAlphaBetaBad(heuristic : Heuristic, b : GameEngine, lookahead : Int, 
		  					_alpha : Score, _beta : Score, topPlayer : PlayerCellState) : AlphaBetaSearchTree = {
    val timer = new Timer()
    val maximizing = topPlayer == b.currentTurn // maximize the next possible moves if we will be making the choice
    val currentLegalMoves = b.allLegalMoves(b.currentTurn)

    if(currentLegalMoves.isEmpty) {
    	val bestScore = if( b.leadingPlayer == topPlayer ) {
    		Score(b.score(topPlayer))
    	} else {
    		Score(-b.score(topPlayer.otherPlayer))    		  
    	}      
    	// We want this to be a root node
    	AlphaBetaSearchTree.GameOver(bestScore, b, timer.time)
    } else if(lookahead == 0) {
    	val score = Score(heuristic(b, topPlayer), computeAllHeuristics(b, topPlayer))
    	AlphaBetaSearchTree.SearchLimit(score, b, timer.time)
    } else {      
     	if( b.currentTurn == topPlayer ) {
     	  // Arthur: I think this is easier for most people to understand. But I don't really like it. Oh well.
     		var alpha = _alpha
     		var ret = AlphaBetaSearchTree.Choice(null, null, _alpha, _beta, maximizing, Nil, 0, b, time = timer.time())
     		val beta = _beta
	     	breakable {
	     	  for( m@Move(mx, my) <- currentLegalMoves ) {
	     		  val child = scoreLookaheadAlphaBeta(heuristic, b.makeMove(mx, my, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		  alpha = alpha max child.score
	     		  ret = AlphaBetaSearchTree.Choice(
	     				  			m, child.score, alpha, beta, maximizing, 
	     				  			child +: ret.descendants, 0, b, time = timer.time())
	     		  if( beta <= alpha )
	     		    break
	     	  }
	     	}
     		assert(ret != null)
	     	ret
     	} else {
     	  // Arthur: I want this to share most of it's code with the one above. Can you see a way?
     		val alpha = _alpha
     		var ret = AlphaBetaSearchTree.Choice(null, null, _alpha, _beta, maximizing, Nil, 0, b, time = timer.time())
     		var beta = _beta
	     	breakable {
	     	  for( m@Move(mx, my) <- currentLegalMoves ) {
	     		  val child = scoreLookaheadAlphaBeta(heuristic, b.makeMove(mx, my, b.currentTurn), lookahead - 1, alpha, beta, topPlayer)
	     		  beta = beta min child.score
	     		  ret = AlphaBetaSearchTree.Choice(
	     				  			m, child.score, alpha, beta, maximizing, 
	     				  			child +: ret.descendants, 0, b, time = timer.time())
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
