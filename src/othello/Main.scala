package othello
import scala.swing._
import scala.swing.event.MouseClicked
import scala.collection.mutable.ArrayBuffer
import java.awt.Color
 
object Main extends SimpleSwingApplication {
	lazy val buttons = (0 to 7).map(x => (0 to 7).map(y => 
	  	new Button("") {
	  	  action = Action((x, y).toString) {
	  	      handleClick(x, y)
	  	  }
	  	}
	  	))
	  	
	lazy val statusLabel = new Label("")
	
	def top = new MainFrame {
		title = "Othello"
		contents = new BoxPanel(Orientation.Vertical) {
			contents += statusLabel
			contents += new GridPanel(8, 8) {
				contents ++= buttons.flatten
			}
		}
	}
	
	var engine  = GameEngine.starting
	val ai = new AI
	
	def handleClick(x : Int, y : Int) {
	  println((x,y))
	  
	  if( engine.currentTurn == Black ) {
		  engine = engine.makeMove(x, y, Black)
		  render()
	  }
	  
	  if( engine.currentTurn == White ) {
		  Swing.onEDT {
			  val Move(mx, my) = ai.makeMove(engine)
			  engine = engine.makeMove(mx, my, White)
			  render()
		  }
	  }
	}
	
	def render() {
	  for{ (bs, ds) <- (buttons zip engine.board)
	        (b, d) <- (bs zip ds) } {
	    b.text = d.toString
	    b.background = d match {
	      case Black => Color.darkGray
	      case White => Color.white
	      case Empty => Color.lightGray
	    }
	  }
	        
	  statusLabel.text = "Next play: " + engine.currentTurn + "       " + 
			  Black + ": " + engine.score(Black) + " " + White + ": " + engine.score(White)
	  if(engine.gameOver)
		  statusLabel.text = "Game Over!! " + engine.leadingPlayer + " Wins!"
	}
	
	render()
}