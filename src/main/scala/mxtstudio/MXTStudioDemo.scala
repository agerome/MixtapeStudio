package mxtstudio
  
object MXTStudioTest extends MXTStudio {
  def main(args:Array[String]) = {
    Record
    Sound('x) as 60
    Display("Hello!", 'x)
    Display(MashKeys(0,1))
    Generate
    Play
  }
}
