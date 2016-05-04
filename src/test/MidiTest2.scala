import javax.sound.midi._
import java.io.File

object MidiTest2 {
  //Loading into sequencers
    val e : InvalidMidiDataException = null
    var mySeq : Sequencer = null
    try {
      mySeq = MidiSystem.getSequencer()
    }
    catch {
      case e: MidiUnavailableException => e.printStackTrace()
      System.exit(1)
    }
    
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
    
    track.add(makeNote(60, 0))
    track.add( endNote(60, 1))
    track.add(makeNote(60, 1))
    track.add( endNote(60, 2))
    
    track.add(makeNote(67, 2))
    track.add( endNote(67, 3))
    track.add(makeNote(67, 3))
    track.add( endNote(67, 4))
    
    track.add(makeNote(69, 4))
    track.add( endNote(69, 5))
    track.add(makeNote(69, 5))
    track.add( endNote(69, 6))
    
    track.add(makeNote(67, 6))
    track.add( endNote(67, 8))
    
//    //Track1
//    //Creation of MIDI events
//    var msg = new ShortMessage()
//    msg.setMessage(ShortMessage.NOTE_ON, 0, 60, 50)
//    var event = new MidiEvent(msg, 0)
//    track.add(event)
//    
//    msg = new ShortMessage()
//    msg.setMessage(ShortMessage.NOTE_OFF, 0, 60)
//    event = new MidiEvent(msg, 1)
//    track.add(event)
//    
//    msg = new ShortMessage()
//    msg.setMessage(ShortMessage.NOTE_ON, 0, 67, 50)
//    event = new MidiEvent(msg, 0)
//    track.add(event)
//    
//    msg = new ShortMessage()
//    msg.setMessage(ShortMessage.NOTE_OFF, 0, 67)
//    event = new MidiEvent(msg, 2)
//    track.add(event)
//  
//    println("track size: " + track.size())
//    println("track length: " + track.ticks())
    
//    //Track2
//    val t2m0 = new ShortMessage()
//    t2m0.setMessage(ShortMessage.NOTE_ON, 0, 64, 127)
//    val t2e0 = new MidiEvent(t2m0, 0)
//    track2.add(t2e0)
//    
//    val t2m1 = new ShortMessage()
//    t2m1.setMessage(ShortMessage.NOTE_OFF, 0, 64)
//    val t2e1 = new MidiEvent(t2m1, 1)
//    track2.add(t2e1)
    
    
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
  
  def makeNote (note:Int, time:Int) : MidiEvent = {
    var msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, 0, note, 127)
    var event = new MidiEvent(msg, time)
    return event
  }
  
  def endNote (note:Int, time:Int) : MidiEvent = {
    var msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_OFF, 0, note)
    var event = new MidiEvent(msg, time)
    return event
  }
}//end class