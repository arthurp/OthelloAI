package othella
import scala.swing._
import scala.swing.event.MouseClicked
import scala.collection.mutable.ArrayBuffer
 
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
	
	val engine  = new GameEngine
	
	def handleClick(x : Int, y : Int) {
	  println((x,y))
	  
	  engine.makeMove(x, y)
	  	  
	  render()
	  
	  if(engine.gameOver)
	    statusLabel.text = "Game Over!!"
	}
	
	def render() {
	  for{ (bs, ds) <- (buttons zip engine.board)
	        (b, d) <- (bs zip ds) } {
	    b.text = d.toString
	  }
	        
	  statusLabel.text = "Next play: " + engine.currentTurn
	}
	
	render()
}