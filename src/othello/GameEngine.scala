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
  override def toString = "_"
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

case class GameEngine(val board: List[List[CellState]], val currentTurn: PlayerCellState, val gameOver: Boolean) {
	import GameEngine._
	
	def cell(x:Int,y:Int) = if(x < 0 || x >= 8 || y < 0 || y >= 8) Empty else board(x)(y)
		
	def makeMove(x:Int, y:Int, p:Player) = {
	  if(p != currentTurn) {
	    throw new Exception("It is not players turn");
	  }
	  if(isValidMove(x,y,p)) {
		var newboard = updateCell(board, x, y, p)

		for( d <- Direction.all if directionHasFlip(x, y, p, d)) {
		  newboard = flipDirection(newboard, x, y, d)
		}

		val (newgameover, newturn) =
		  if(allLegalMoves(p.otherPlayer).isEmpty) {
		    if(allLegalMoves(p).isEmpty)
		    	(true, currentTurn)
		    else
		    	(false, currentTurn)
		  } else {
			  (false, currentTurn.otherPlayer)
		  }
	    GameEngine(newboard, newturn, newgameover)
	  } else {
	    this
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
	
	private def flipDirection(b: Board, x : Int, y : Int, d : Direction) : Board = {
		val (dx, dy) = d.step
		
		if(cell(x+dx, y+dy) == currentTurn.otherPlayer) {
			flipDirection(updateCell(b, x+dx, y+dy, currentTurn), x+dx, y+dy, d)
		} else {
		  b
		}
	}
	
	def allLegalMoves(p : Player) = {
	  for(i <- 0 to 7; j <- 0 to 7 if isValidMove(i, j, p)) yield (i, j)
	} 
	
	override def toString = {
	  "c=" + currentTurn + ". go=" + gameOver + "\n" +
	  board.map(_.mkString("")).mkString("\n")
	}
}

object GameEngine { 
  type Player = PlayerCellState
  type Board = List[List[CellState]]
  
  def updateCell(b: Board, x: Int, y: Int, v: CellState) = b.updated(x, b(x).updated(y, v))
 
  private val emptyBoard : Board = (0 to 7).toList.map(_ => (0 to 7).toList.map(_ => Empty)) 
  // emptyBoard * (3, 3, White) * ....
  val starting = GameEngine(
      updateCell(updateCell(updateCell(updateCell(emptyBoard, 4, 3, Black), 3, 4, Black), 4, 4, White), 3, 3, White),
      Black, 
      false)
}