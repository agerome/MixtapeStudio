package mxtstudio
  
object MXTStudioTest extends MXTStudio {
  def main(args:Array[String]) = {
    Record
    Sound('x) as (60 + 10)
    Sound('y) as (60 Mul 10)
    // Scala Comment
    Mute("MXTStudiio Comment")
    Sound('z) as (60 - 10 Div 2)
    Display("Hello!", 'x, 'y, 'z)
    Display(MashKeys(0,1))
    Note(60,1,2,120)
    Loop
        Display('z, " is the value of z")
        If('z LowerThan 5)
            Stop
        Else
            Ask('in)
            Sound('z) as ('z Sub 'in)
        Close
    EndLoop
    Display("Bye!", 'x, 'y, 'z)
    Generate
    Play
  }
}
