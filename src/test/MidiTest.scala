import javax.sound.midi._
import java.io.File

object MidiTest {
  
  def main (args:Array[String]) {
    
    //Might need to use regedit to open up a pref in JavaSoft to play music
    
    var nChannelNumber : Int = 4
    var nNoteNumber : Int = 60
    var nVelocity : Int = 93
    var nDuration : Int = 1000
    
    //Synthesizer
    val synth : Synthesizer = MidiSystem.getSynthesizer()
    /*
    val myMsg = new ShortMessage()
    // Play the note Middle C (60) moderately loud
    // (velocity = 93) on channel 4 (zero-based).
    myMsg.setMessage(ShortMessage.NOTE_ON, 4, 60, 93)
    val synthRcvr = synth.getReceiver()
    synthRcvr.send(myMsg, -1)
    */
    
    synth.open()
    var channels : Array[MidiChannel] = synth.getChannels()
    // Check for null; maybe not all 16 channels exist
    var channel : MidiChannel = channels(nChannelNumber)
    channel.noteOn(nNoteNumber, nVelocity)
    Thread.sleep(nDuration)
    channel.noteOff(nNoteNumber)
    synth.close()
    
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
     */
    
    //Sequencer
    //Creation of a MIDI sequence
    val sequence = new Sequence(Sequence.PPQ, 1)
    val track = sequence.createTrack()
    
    //Creation of MIDI events
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, 0, 60, 127)
    val event1 = new MidiEvent(msg, 0)
    track.add(event1)
    
    val msg2 = new ShortMessage()
    msg2.setMessage(ShortMessage.NOTE_OFF, 0, 60)
    val event2 = new MidiEvent(msg2, 2)
    track.add(event2)
    
    val msg3 = new ShortMessage()
    msg3.setMessage(ShortMessage.NOTE_ON, 0, 24, 127)
    val event3 = new MidiEvent(msg3, 3)
    track.add(event3)
    
    val msg4 = new ShortMessage()
    msg4.setMessage(ShortMessage.NOTE_OFF, 0, 24)
    val event4 = new MidiEvent(msg4, 4)
    track.add(event4)
    
    
    println("track size: " + track.size())
    println("track length: " + track.ticks())
    
    //Loading into sequencers
    val mySeq = MidiSystem.getSequencer()
    
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
    val newFile = new File("butts.midi")
    MidiSystem.write(sequence, 0, newFile)
  }//end main
  
}//end class