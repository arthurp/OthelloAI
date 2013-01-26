package othella

import scala.collection.mutable.ArrayBuffer
sealed trait CellState

sealed trait PlayerCellState extends CellState {
  def otherPlayer : PlayerCellState
}

case object Black extends PlayerCellState {
  override def toString = "X"
  def otherPlayer = White
}
case object White extends PlayerCellState {
  override def toString = "O"
  def otherPlayer = Black
}

case object Empty extends CellState {
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

object Direction{ val all = List(N,NE,E,SE,S,SW,W,NW)}

class GameEngine {
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
	
	def makeMove(x:Int,y:Int) {
	  if(isValidMove(x,y)) {
		  _board(x)(y) = currentTurn
		  for(d <- Direction.all if directionHasFlip(x, y, d)) {
		    flipDirection(x, y, d)
		  }
		  _currentTurn = _currentTurn.otherPlayer
		  if(allLegalMoves().isEmpty) {
			  _currentTurn = _currentTurn.otherPlayer
			  if(allLegalMoves().isEmpty)
				  _gameOver = true
		  }
	  }
	}
	
	def isValidMove(x:Int,y:Int) = cell(x,y) == Empty && Direction.all.exists( directionHasFlip(x, y, _) )
	
	private def directionHasFlip(x : Int, y : Int, d : Direction) : Boolean = {
		def h(x : Int, y : Int, d : Direction) : Boolean = {
			val (dx, dy) = d.step
		
			(cell(x+dx, y+dy) == currentTurn.otherPlayer && h(x+dx, y+dy, d)) ||
			(cell(x+dx, y+dy) == currentTurn)
		}
		
		val (dx, dy) = d.step
		h(x, y, d) && cell(x+dx, y+dy) == currentTurn.otherPlayer
	}
	
	private def flipDirection(x : Int, y : Int, d : Direction) {
		val (dx, dy) = d.step
		
		if(cell(x+dx, y+dy) == currentTurn.otherPlayer) {
			_board(x+dx)(y+dy) = currentTurn
			flipDirection(x+dx, y+dy, d)
		}
	}
	
	def allLegalMoves() = {
	  for(i <- 0 to 7; j <- 0 to 7 if isValidMove(i, j)) yield (i, j)
	} 
}