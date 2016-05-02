import javax.sound.midi._

object MidiTest {
  
  def main (args:Array[String]) {
    
    //Might need to use regedit to open up a pref in JavaSoft to play music
    
    var nChannelNumber : Int = 4
    var nNoteNumber : Int = 60
    var nVelocity : Int = 93
    var nDuration : Int = 1000
    
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
    
    //Creation of a MIDI sequence
    val sequence = new Sequence(Sequence.PPQ, 1)
    val track = sequence.createTrack()
    
    //Creation of MIDI events
    val msg = new ShortMessage()
    msg.setMessage(ShortMessage.NOTE_ON, 0, 60, 93)
    val event = new MidiEvent(msg, 0)
    track.add(event)
    
    val msg2 = new ShortMessage()
    msg2.setMessage(ShortMessage.NOTE_OFF, 0, 60)
    val event2 = new MidiEvent(msg2, 1)
    track.add(event2)
    
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
  }//end main
  
}//end class