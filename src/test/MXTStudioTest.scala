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
    
    ChangeTempo(250)
    
    Note(60, 4, 2, 120)
    Note(60, 6, 2, 120)
    Note(67, 8, 2, 120)
    Note(67, 10, 2, 120)
    Note(69, 12, 2, 120)
    Note(69, 14, 2, 120)
    Note(67, 16, 4, 120)
    
    Note(67, 20, 2, 120)
    Note(60, 22, 2, 120)
    Note(0, 24, 1, 0)
    Note(72, 25, 2, 120)
    Note(69, 27, 1, 120)
    Note(67, 28, 2, 120)
    Note(60, 30, 2, 120)
    
    Note(0, 31, 1, 0)
    Note(67, 33, 2, 120)
    Note(65, 35, 1, 120)
    Note(64, 36, 1, 120)
    Note(64, 37, 1, 120)
    Note(65, 38, 1, 120)
    Note(67, 39, 1, 120)
    Note(60, 40, 2, 120)
    Note(62, 42, 2, 120)
    Note(60, 44, 4, 120)
    
    Generate
    Play
  }
}