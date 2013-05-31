package othello

import scala.util.control.Breaks
import scala.math._
import scala.collection.mutable.ListBuffer
import scala.xml.Elem
import java.io.File

case class Score(score : Float, heuristics : Map[String, Float] = Map(), maxValue : Boolean = false, minValue: Boolean = false) extends Ordered[Score] {
    def compare(o : Score) = {
    score compareTo o.score
  }
  
  def toStringLong = {
    if(maxValue){
      "MaxValue"
    } else if(minValue){
      "MinValue"
    } else {
      "%s (%s)".format(score, heuristics)
    }    	
  }
  
  override def toString = {
    if(maxValue){
      "MaxValue"
    } else if(minValue){
      "MinValue"
    } else {
      "%s".format(score)
    }    
  }
  def toXML : Elem = { 
    <score value={this.toString}>
     	{ for((heuristic, value) <- heuristics ) yield <heuristic name={heuristic} value={value.toString} />}
    </score>	
  }
  
  def max(x : Score) = if(score > x.score) this else x  
  def min(x : Score) = if(score < x.score) this else x  
}

object Score {
  val MaxValue = Score(Float.MaxValue, maxValue=true)
  val MinValue = Score(Float.MinValue, minValue=true)  
}