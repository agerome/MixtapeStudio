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
    Note(60, 4, 1, 120)
    Note(60, 5, 1, 120)
    Note(67, 6, 1, 120)
    Note(67, 7, 1, 120)
    Note(69, 8, 1, 120)
    Note(69, 9, 1, 120)
    Note(67, 10, 2, 120)
    
    Note(67, 12, 2, 120)
    Note(60, 14, 2, 120)
    Note(0, 16, 1, 0)
    Note(72, 17, 2, 120)
    Note(69, 19, 1, 120)
    Note(67, 20, 2, 120)
    Note(60, 22, 2, 120)
    
    Note(0, 24, 1, 0)
    Note(67, 25, 2, 120)
    Note(65, 27, 1, 120)
    Note(64, 28, 1, 120)
    Note(64, 29, 1, 120)
    Note(65, 30, 1, 120)
    Note(67, 31, 1, 120)
    Note(60, 32, 2, 120)
    Note(62, 34, 2, 120)
    Note(60, 36, 4, 120)
    
    Generate
    Play
  }
}