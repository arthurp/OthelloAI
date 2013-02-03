package othello

import scala.collection.mutable.ArrayBuffer
sealed trait CellState


sealed trait PlayerCellState extends CellState {
  def otherPlayer : PlayerCellState
  def intVal : Int
}

case object Black extends PlayerCellState {
  override def toString = "X"
  def intVal = 1  
  def otherPlayer = White
}
case object White extends PlayerCellState {
  override def toString = "O"
  def intVal = -1
  def otherPlayer = Black
}

case object Empty extends CellState {
  def intVal = 0
  override def toString = " "
}

sealed class Direction(val step : (Int,Int))
case object N extends Direction((0,-1))  
case object NE extends Direction((1,-1))
case object E extends Direction((1,0))
case object SE extends Direction((1,1))
case object S extends Direction((0,1))
case object SW extends Direction((-1,1))
case object W extends Direction((-1,0))
case object NW extends Direction((-1,-1))

object Direction {
  val all = List(N,NE,E,SE,S,SW,W,NW)
}

class GameEngine {
	type Player = PlayerCellState
    private val _board = new ArrayBuffer(8) ++ (0 to 7).map( _ => 
	   new ArrayBuffer[CellState](8) ++ (0 to 7).map( _ => Empty ) )
	private var _currentTurn : PlayerCellState = Black
	
	private var _gameOver = false
	
	def gameOver = _gameOver
	
	_board(3)(3) = White
	_board(4)(3) = Black
	_board(4)(4) = White
	_board(3)(4) = Black

	def currentTurn = _currentTurn
	def cell(x:Int,y:Int) = if(x < 0 || x >= 8 || y < 0 || y >= 8) Empty else _board(x)(y)
	def board : Seq[Seq[CellState]] = _board
	def copy() ={
	  var newGame = new GameEngine()
	  newGame._currentTurn = _currentTurn
	  newGame._gameOver = _gameOver
	  for(i <- (0 to 7) ; j <- (0 to 7)) {
	    newGame._board(i)(j) = _board(i)(j)
	  }
	  newGame
	}
	def makeMove(x:Int, y:Int, p:Player) {
	  if(p != currentTurn) {
	    throw new Exception("It is not players turn");
	  }
	  if(isValidMove(x,y,p)) {
		  _board(x)(y) = p
		  for(d <- Direction.all if directionHasFlip(x, y, p, d)) {
		    flipDirection(x, y, d)
		  }
		  
		  if(allLegalMoves(p.otherPlayer).isEmpty) {
		    if(allLegalMoves(p).isEmpty)
		    	_gameOver = true
		  } else {
			  _currentTurn = _currentTurn.otherPlayer
		  }
	  }
	}
	
	def score(p : PlayerCellState) : Int = board.flatten.count(_ == p)
	
	def leadingPlayer : CellState = {
	  val b = score(Black)
	  val w = score(White)
	  if( b > w )
		Black
	  else if( w > b )
	    White
	  else
	    Empty
	}
	
	def isValidMove(x:Int,y:Int,p:Player) = cell(x,y) == Empty && Direction.all.exists( directionHasFlip(x, y, p, _) )
	
	private def directionHasFlip(x : Int, y : Int, p : Player, d : Direction) : Boolean = {
		def h(x : Int, y : Int, d : Direction) : Boolean = {
			val (dx, dy) = d.step
		
			(cell(x+dx, y+dy) == p.otherPlayer && h(x+dx, y+dy, d)) ||
			(cell(x+dx, y+dy) == p)
		}
		
		val (dx, dy) = d.step
		h(x, y, d) && cell(x+dx, y+dy) == p.otherPlayer
	}
	
	private def flipDirection(x : Int, y : Int, d : Direction) {
		val (dx, dy) = d.step
		
		if(cell(x+dx, y+dy) == currentTurn.otherPlayer) {
			_board(x+dx)(y+dy) = currentTurn
			flipDirection(x+dx, y+dy, d)
		}
	}
	
	def allLegalMoves(p : Player) = {
	  for(i <- 0 to 7; j <- 0 to 7 if isValidMove(i, j, p)) yield (i, j)
	} 
	
	override def toString = {
	  "c=" + currentTurn + ". go=" + gameOver + "\n" +
	  _board.map(_.mkString(",")).mkString("\n")
	}
}