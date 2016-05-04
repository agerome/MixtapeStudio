import javax.sound.midi._
import java.io.File

object LittleStar {
    
  def main (args:Array[String]) {
    
    //Might need to use regedit to open up a pref in JavaSoft to play music
    var nDuration : Int = 1000
    
     /*
     * The 12 notes
     * C C# D D# E F F# G G# A A# B
     * Middle C (at octave 4) is 60
     * Octaves start from 1 (so %12 - 1)
     * Octaves list:
     * 0th: 12 13 14 15 16 17 18 19 20 21 22 23
     * 1st: 24 - 35
     * 2nd: 36 - 47
     * 3rd: 48 - 59
     * 4th: 60 - 71
     * 5th: 72 - 83
     * 6th: 84 - 95
     * 7th: 96 - 107
     * 8th: 108 - 119
     * 9th: 120 - 127
     */
    
    //Sequencer
    var mySeq : Sequencer = null
    try {
      mySeq = MidiSystem.getSequencer()
    }
    catch {
      case e: MidiUnavailableException => e.printStackTrace()
      System.exit(1)
    }
    
    //Creation of a MIDI sequence
    var sequence : Sequence = null
    try {
      sequence = new Sequence(Sequence.PPQ, 1)
    }
    catch {
      case e : InvalidMidiDataException => e.printStackTrace()
      System.exit(1)
    }
    
    val track = sequence.createTrack()
//    val track2 = sequence.createTrack()
    
      //note(track, note, start, duration)
      cnote(track, 60, 0, 1)
      cnote(track, 60, 1, 1)
      cnote(track, 67, 2, 1)
      cnote(track, 67, 3, 1)
      cnote(track, 69, 4, 1)
      cnote(track, 69, 5, 1)
      cnote(track, 67, 6, 2)
      
      cnote(track, 65, 8, 1)
      cnote(track, 65, 9, 1)
      cnote(track, 64, 10, 1)
      cnote(track, 64, 11, 1)
      cnote(track, 62, 12, 1)
      cnote(track, 62, 13, 1)
      cnote(track, 60, 14, 2)
      
      cnote(track, 67, 16, 1)
      cnote(track, 67, 17, 1)
      cnote(track, 65, 18, 1)
      cnote(track, 65, 19, 1)
      cnote(track, 64, 20, 1)
      cnote(track, 64, 21, 1)
      cnote(track, 62, 22, 2)
      
      cnote(track, 67, 24, 1)
      cnote(track, 67, 25, 1)
      cnote(track, 65, 26, 1)
      cnote(track, 65, 27, 1)
      cnote(track, 64, 28, 1)
      cnote(track, 64, 29, 1)
      cnote(track, 62, 30, 2)
      
      cnote(track, 60, 32, 1)
      cnote(track, 60, 33, 1)
      cnote(track, 67, 34, 1)
      cnote(track, 67, 35, 1)
      cnote(track, 69, 36, 1)
      cnote(track, 69, 37, 1)
      cnote(track, 67, 38, 2)
      
      cnote(track, 65, 40, 1)
      cnote(track, 65, 41, 1)
      cnote(track, 64, 42, 1)
      cnote(track, 64, 43, 1)
      cnote(track, 62, 44, 1)
      cnote(track, 62, 45, 1)
      cnote(track, 60, 46, 2)
    
    //Playing
    mySeq.open()
    mySeq.setSequence(sequence)
    mySeq.start()
    println("mySeq started")
    println("tick length: " + mySeq.getTickLength())
    while (mySeq.isRunning()) {
      Thread.sleep(nDuration)
    }
    mySeq.close()
    
    //Save to a MIDI file
    val newFile = new File("butts2.midi")
    MidiSystem.write(sequence, 0, newFile)
  }//end main
  
  
  def cnote (track:Track, note:Int, start:Int, duration:Int) : Int = {
    var msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, 0, note, 50)
    var event = new MidiEvent(msg, start)
    track.add(event)
    
    msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_OFF, 0, note)
    val end : Int = start + duration
    event = new MidiEvent(msg, end)
    track.add(event)
    
    return end
  }
}//end class