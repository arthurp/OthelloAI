package othello

class AI {
  implicit def heuristic2Rich(h1 : Heuristic) = new {
    def +(h2 : Heuristic) : Heuristic = (b, p) => h1(b, p) + h2(b, p)
    def *(h2 : Heuristic) : Heuristic = (b, p) => h1(b, p) * h2(b, p)
  }
  implicit def constantHeuristic(v : Float) : Heuristic = (b, p) => v
  implicit def constantHeuristic(v : Double) : Heuristic = (b, p) => v.floatValue()
  
  type Heuristic = (GameEngine, PlayerCellState) => Float
  
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
      else
        -s
    }).sum / 100
  }

  
  val heuristic = mobility1 + (positional * 2.0)
  
  def makeMove(b : GameEngine) = {
    val lookahead = 3;
    makeMoveInternal(b,lookahead)
  }
  def makeMoveInternal(b : GameEngine, lookahead : Int) = {
    var winningMove : (Int,Int) = b.allLegalMoves(b.currentTurn).head; 
    var winningScore = scoreLookaheadTree(heuristic, winningMove._1,winningMove._2,b,lookahead, b.currentTurn)
    for((x,y) <- b.allLegalMoves(b.currentTurn).tail) {
      val s = scoreLookaheadTree(heuristic, x,y,b,lookahead, b.currentTurn)
      if (s > winningScore) {
        winningMove = (x,y)
        winningScore = s 
      }      
    }
    winningMove
  }
  
  def scoreNaive(heuristic : Heuristic, x:Int, y:Int, b:GameEngine) ={
    val clone = b.copy()
    clone.makeMove(x,y,b.currentTurn) 
    heuristic(clone, b.currentTurn)
  } 
  
  var trace = false
  
  def traceln(l : Int, v : Any) {
    if( ! trace ) return
    val ind = "> " * (6 - l)
    val s = v.toString
    val s1 = s.lines.map(ind + _).mkString("\n")
    println(s1)
  }
  
  def scoreLookaheadTree(heuristic : Heuristic, x:Int, y:Int, b:GameEngine, lookahead:Int, topPlayer : PlayerCellState) : Float ={
    val clone = b.copy()
    clone.makeMove(x, y, b.currentTurn) 
    if(lookahead == 0) {
    	traceln(lookahead, clone)
    	val s = heuristic(clone, topPlayer)
    	traceln(lookahead, s)
    	s
    } else {
     	traceln(lookahead, clone)
    	val children = for( (mx, my) <- clone.allLegalMoves(clone.currentTurn) ) yield {
    	  scoreLookaheadTree(heuristic, mx, my, clone, lookahead - 1, topPlayer)
    	}
     	traceln(lookahead, children)
    	val s = if( !children.isEmpty ) {
	    	if( topPlayer == b.currentTurn )
	    	  children.max
	    	else
	    	  children.min
    	} else {
    		if( clone.leadingPlayer == topPlayer ) {
    		  clone.score(topPlayer)
    		} else {
    		  -clone.score(topPlayer.otherPlayer)    		  
    		}
    	}
    	traceln(lookahead, s)
   	
    	s
    }          
  } 
}