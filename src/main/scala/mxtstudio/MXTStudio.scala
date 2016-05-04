/**
 * MixtapeStudio
 * 
 * A Scala DSL
 *
 * CS345, Assignment 5, Spring 2016
 *
 * Alex Gerome, Eric Chao
 *
 *
 */
 package mxtstudio

import scala.collection.mutable.{ HashMap, Stack }
import scala.util.Random

import javax.sound.midi._
import java.io.{ File, IOException }

class MXTStudio{
  abstract sealed class MXTLine
  // These functions handle printing to console; multiple functions to avoid match errors
  // Debug option allows for error printing; if true, then uses error printing
  case class PrintString(num: Int, s: String, debug: Boolean) extends MXTLine
  case class PrintVariable(num: Int, s: Symbol, debug: Boolean) extends MXTLine
  case class PrintNumber(num: Int, s: Int, debug: Boolean) extends MXTLine
  case class PrintFunction(num: Int, s: Function0[Any], debug: Boolean) extends MXTLine
  case class PrintConcat(num: Int, s: Vector[Any], debug: Boolean) extends MXTLine

  // Deals with user input
  case class Input(num: Int, s: Symbol) extends MXTLine

  // Deals with conditionals
  case class IfStart(num: Int, fun: Function0[Boolean]) extends MXTLine
  case class IfExpr(num: Int, sym: Symbol) extends MXTLine
  case class ElseStart(num: Int) extends MXTLine
  case class EndIf(num: Int) extends MXTLine

  // Variable assignment
  case class Assign(num: Int, fn: Function0[Unit]) extends MXTLine

  // Loops
  case class LoopStart() extends MXTLine
  case class Break() extends MXTLine
  case class LoopEnd(loopStartLine: Int) extends MXTLine

  // Functions
  case class FnStart(name: Symbol) extends MXTLine
  case class FnEnd() extends MXTLine
  case class FnRet(value: Any) extends MXTLine
  case class FnCall(fnName: Symbol) extends MXTLine
  case class FnCallRet(fnName: Symbol, variable: Symbol) extends MXTLine

  // Ending program
  case class End(num: Int) extends MXTLine

  // Sound generation case classes
  case class GenerateFile(num: Int) extends MXTLine
  case class GenerateNote(note: Int, start: Int, duration: Int, volume: Int) extends MXTLine

  // follow the model of using a pointer the the line we are working with
  var current: Int = 1

  //Datastructures for recording information about lines for lookup
  var lines = new HashMap[Int, MXTLine]
  val binds = new Bindings
  val fnStartLines = new HashMap[Symbol, Int]
  val random = new Random
  val loopStartLines = new Stack[Int]
  val pcStack = new Stack[Int]
  val returnStack = new Stack[Any]

  //Midi requirements
  val sequence = new Sequence(Sequence.PPQ, 1)
  val track = sequence.createTrack()
  var sequencer : Sequencer = null
  try{
    sequencer = MidiSystem.getSequencer() 
  } catch {
    case me : MidiUnavailableException => Console.err.println(Console.RED + 
        "Setup: No Midi device can be found! You code can still run, but no audio can be heard or generated!" + Console.RESET)
    case e : Exception => Console.err.println(Console.RED + 
        "Setup: An unknown error has occurred" + Console.RESET)
  }

  //End File and play back audio
  def Play() = {
    lines(current) = End(current)
    gotoLine(lines.keys.toList.sorted.head)

    try{
      //Midi requirements for playing an audio sequence
      sequencer = MidiSystem.getSequencer() 
      sequencer.open()
      sequencer.setSequence(sequence)
      sequencer.start()
      while(sequencer.isRunning()){
        Thread.sleep(1000)
      }
      sequencer.close()
    } catch {
      case me : MidiUnavailableException => Console.err.println(Console.RED + 
        "Playback: No Midi device was found, so audio cannot heard!" + Console.RESET)
      case e : Exception => Console.err.println(Console.RED + 
        "Playback: An unknown error has occurred" + Console.RESET)
    }
  }

  //Saving a mixtape
  def Generate() = {
    lines(current) = GenerateFile(current)
    current += 1
  }

  //TODO: make commands for tracks/sequences/files?

  //Opens a program and starting recording programmed sequences
  def Record() = {
    lines = new HashMap[Int, MXTLine]
    binds.createScope()
  }

  /**
   * assignments
   */
  case class Assignment(sym: Symbol) {
    def as(v: String): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def as(v: AnyVal): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v)))
      current += 1
    }
    def as(v: Function0[Any]): Unit = {
      lines(current) = Assign(current, (() => binds.set(sym, v())))
      current += 1
    }
  }

  /**
   * runtime evaluator
   */
  private def gotoLine(line: Int) {

    def MasterIf(bool: Boolean): Unit = {
      if (bool) {
        gotoLine(line + 1)
      } else {
        var currLine = line + 1
        var count = 0
        while (!((lines(currLine).isInstanceOf[ElseStart] || lines(currLine).isInstanceOf[EndIf])
                 && count == 0)) {
          if(lines(currLine).isInstanceOf[IfStart]) {
            count = count + 1
          } else if(lines(currLine).isInstanceOf[EndIf]) {
            count = count - 1
          }
          currLine += 1
        }
        gotoLine(currLine + 1)
      }
    }

    lines(line) match {
      // Print to console: if debug=false, then stdout, iif true, then stderr
      case PrintString(_, s: String, debug: Boolean) => {
        if(debug){
          Console.err.println(Console.RED + s + Console.RESET)
        }else{
          println(s)
        }
        gotoLine(line + 1)
      }
      case PrintVariable(_, s: Symbol, debug: Boolean) => {
        if(debug){
          Console.err.println(Console.RED + binds.any(s) + Console.RESET)
        }else{
          println(binds.any(s))
        }
        gotoLine(line + 1)
      }
      case PrintNumber(_, s: Int, debug: Boolean) => {
        if(debug){
          Console.err.println(Console.RED + s + Console.RESET)
        }else{
          println(s)
        }
        gotoLine(line + 1)
      }
      case PrintFunction(_, s: Function0[Any], debug: Boolean) => {
        if(debug){
          Console.err.println(Console.RED + s() + Console.RESET)
        }else{
          println(s())
        }
        gotoLine(line + 1)
      }
      case PrintConcat(_, s: Vector[Any], debug: Boolean) => {
        if(debug){
          Console.err.println(Console.RED + s.map(e => e match {
            case v: Symbol => binds.any(v)
            case v: Function0[Any] => v()
            case _ => e
          }).mkString(" ") + Console.RESET)
        }else{
          println(s.map(e => e match {
            case v: Symbol => binds.any(v)
            case v: Function0[Any] => v()
            case _ => e
          }).mkString(" "))
        }
        gotoLine(line + 1)
      }

      // Waits for input on stdin
      case Input(_, s: Symbol) => {
        val value: Any = tryInt(readLine())
        binds.set(s, value)
        gotoLine(line + 1)
      }

      // Starting an if statement
      case IfStart(_, fun: Function0[Boolean]) => {
        MasterIf(fun())
      }

      case IfExpr(_, sym: Symbol) => {
        MasterIf(binds.any(sym).asInstanceOf[Boolean])
      }

      // Starting an else statement, as part of an if statement
      case ElseStart(_) => {
        // Only reach this if true was executed
        var ln = line
        while (!lines(ln).isInstanceOf[EndIf]) {
          ln = ln + 1;
        }
        gotoLine(ln + 1);
      }

      // Closing an If/Else
      case EndIf(_) => {
        gotoLine(line + 1);
      }


      case Assign(_, fn: Function0[Unit]) =>{
        fn()
        gotoLine(line + 1)
      }


      case LoopStart() => {
        gotoLine(line + 1)
      }


      case Break() => {
        var ln = line
        var loopStartCount = 0
        while (!lines(ln).isInstanceOf[LoopEnd] ||
          loopStartCount > 0) {
          if (lines(ln).isInstanceOf[LoopStart])
            loopStartCount += 1
          if (lines(ln).isInstanceOf[LoopEnd])
            loopStartCount -= 1
          ln += 1
        }
        gotoLine(ln + 1)
      }


      case LoopEnd(loopStartLine: Int) => {
        gotoLine(loopStartLine + 1)
      }


      case FnStart(name: Symbol) => {
        var ln = line
        while (!lines(ln).isInstanceOf[FnEnd]) {
          ln += 1
        }
        gotoLine(ln + 1)
      }


      case FnEnd() => {
        // Leaving function -- destroy current scope and return to last place
        val temp: Any = returnStack.pop()
        binds.destroyScope()

        // TODO add more options
        temp match {
          case t: Function0[Any] => {
            if (returnStack.length > 0) {
              returnStack.pop() match {
                case v: Symbol => {
                  // set our return variable
                  binds.set(v, t())
                }
                case v => {
                  v match {
                    case None => // throw both away
                    case _ => {
                      // oops both were good values
                      returnStack.push(v)
                      returnStack.push(t)
                    }
                  }
                }
              }
            }
          }
          case t: Int => {
            if (returnStack.length > 0) {
              returnStack.pop() match {
                case v: Symbol => {
                  // set our return variable
                  binds.set(v, t)
                }
                case v => {
                  v match {
                    case None => // throw both away
                    case _ => {
                      // oops both were good values
                      returnStack.push(v)
                      returnStack.push(t)
                    }
                  }
                }
              }
            }
          }
          case t: Symbol => {
            // oops we popped a symbol
            returnStack.push(t)
          }
          case None =>
          case _ => throw new RuntimeException(f"Something bad has happened! $temp")
        }

        gotoLine(pcStack.pop())
      }


      case FnRet(value: Any) => {
        // check and evaluate the types
        value match {
          case v: Function0[Any] => returnStack.push(v())
          case v: Symbol => returnStack.push(binds.any(v))
          case v => returnStack.push(v)
        }

        // actually need to go to end of function
        var ln = line
        while (!lines(ln).isInstanceOf[FnEnd]) {
          ln += 1
        }
        gotoLine(ln)
      }


      case FnCall(fnName: Symbol) => {
        // push trash onto the return stack
        returnStack.push(None)
        pcStack.push(line + 1)
        binds.createScope()
        gotoLine(fnStartLines.get(fnName) match {
          case Some(s) => s + 1 //go beyond the start of the function
          case None => -1
        })
      }


      case FnCallRet(fnName: Symbol, variable: Symbol) => {
        // push the return variable onto the return stack
        returnStack.push(variable)
        pcStack.push(line + 1)
        binds.createScope()
        gotoLine(fnStartLines.get(fnName) match {
          case Some(s) => s + 1 // go beyond the start of the function
          case None => -1
        })
      }


      case End(_) =>


      case GenerateFile(_) => {
        gotoLine(line + 1)
        try{
          //Save to a MIDI file
          val newFile = new File("mixtape.midi")
          MidiSystem.write(sequence, 0, newFile)
        } catch {
          case ioe : IOException => Console.err.println(Console.RED + 
            "File Generation: No Midi device was found, so files cannot generated!" + Console.RESET)
          case e : Exception => Console.err.println(Console.RED +
            "File Generation: An unknown error has occurred" + Console.RESET)
        }
      }

      case GenerateNote(note: Int, start: Int, duration: Int, volume: Int) => {
        var msg = new ShortMessage()
        msg.setMessage(ShortMessage.NOTE_ON,0,60,127)
        var event = new MidiEvent(msg,0)
        track.add(event)

        msg = new ShortMessage()
        msg.setMessage(ShortMessage.NOTE_OFF,0,60)
        event = new MidiEvent(msg,0)
        track.add(event)
      }

      // Catch all
      case _ => 
    }
  }

  /*
   *  OPERATORS
   */
  // Prefix
  //Random function: gives a random note within the specified range (midi standard)
  //Mashkeys starts generating random notes at time i for duration j (returns number of generated notes)
  def Random(i: Int, j: Int): Int = { random.nextInt(j + 1 - i) + i }
  def MashKeys(i: Int, j: Int): Int = { 
    // Get number of notes to generate
    var numNotes = random.nextInt(30) + 1
    var x = 0
    // Generate the notes!
    for(x <- 0 to numNotes){
      var note = random.nextInt(84) + 12
      var start = random.nextInt(j + 1) + i
      var duration = random.nextInt(j) + 1
      var volume = random.nextInt(128)
      GenerateNote(note, start, duration, volume)
    }
    // Could not think of a way to do this with out return
    return numNotes
  }

  // Infix
  implicit def operator_any(i: Any) = new {
    def Add(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i + _j
                case _j: Double => _i + _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i + _j
                case _j: Double => _i + _j
              }
            }
          }
        }
    }

    def Sub(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i - _j
                case _j: Double => _i - _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i - _j
                case _j: Double => _i - _j
              }
            }
          }
        }
    }

    def Mul(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i * _j
                case _j: Double => _i * _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i * _j
                case _j: Double => _i * _j
              }
            }
          }
        }
    }

    def Div(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i / _j
                case _j: Double => _i / _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i / _j
                case _j: Double => _i / _j
              }
            }
          }
        }
    }

    def Mod(j: Any): Function0[Any] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i % _j
                case _j: Double => _i % _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i % _j
                case _j: Double => _i % _j
              }
            }
          }
        }
    }

    def HigherThan(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i > _j
                case _j: Double => _i > _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i > _j
                case _j: Double => _i > _j
              }
            }
          }
        }
    }

    def LowerThan(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i < _j
                case _j: Double => _i < _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i < _j
                case _j: Double => _i < _j
              }
            }
          }
        }
    }

    def Equals(j: Any): Function0[Boolean] = {
      () =>
        {
          val base_i = i match {
            case _i: Symbol => binds.anyval(_i)
            case _i: Function0[Any] => _i()
            case _ => i
          }

          val base_j = j match {
            case _j: Symbol => binds.anyval(_j)
            case _j: Function0[Any] => _j()
            case _ => j
          }

          base_i match {
            case _i: Int => {
              base_j match {
                case _j: Int => _i == _j
                case _j: Double => _i == _j
              }
            }
            case _i: Double => {
              base_j match {
                case _j: Int => _i == _j
                case _j: Double => _i == _j
              }
            }
          }
        }
    }

  }

  //User input
  object Ask {
    def apply(s: Symbol) = {
      lines(current) = Input(current, s)
      current += 1
    }
  }

  /**
   * attempt to convert String to an Integer
   * if not possible, return the original String
   */
  def tryInt(s: String): Any = {
    try {
      s.toInt
    } catch {
      case e: Exception => s
    }
  }

  // Prints to console: multiple functions avoids match errors
  // Last parameter is false to indicate no debug
  object Display {
    def apply(s: String) = {
      lines(current) = PrintString(current, s, false)
      current += 1
    }
    def apply(s: Any*) = {
      lines(current) = PrintConcat(current, s.toVector, false)
      current += 1
    }
    def apply(s: Symbol) = {
      lines(current) = PrintVariable(current, s, false)
      current += 1
    }
    def apply(s: Int) = {
      lines(current) = PrintNumber(current, s, false)
      current += 1
    }
    def apply(s: Function0[Any]) = {
      lines(current) = PrintFunction(current, s, false)
      current += 1
    }
  }

  // Prints to console: multiple functions avoids match errors
  // Last parameter is true to indicate debug printing
  object Debug {
    def apply(s: String) = {
      lines(current) = PrintString(current, s, true)
      current += 1
    }
    def apply(s: Any*) = {
      lines(current) = PrintConcat(current, s.toVector, true)
      current += 1
    }
    def apply(s: Symbol) = {
      lines(current) = PrintVariable(current, s, true)
      current += 1
    }
    def apply(s: Int) = {
      lines(current) = PrintNumber(current, s, true)
      current += 1
    }
    def apply(s: Function0[Any]) = {
      lines(current) = PrintFunction(current, s, true)
      current += 1
    }
  }

  // Sound is the keyword to replace 'var' -- for declaring variables
  object Sound {
    def apply(s: Symbol) = Assignment(s)
  }

  // MXTStudio specific comments; normal scala comments work as well
  object Mute {
    def apply(s: Any) = {}
  }

  // Conditionals
  object If {
    def apply(s: Function0[Boolean]) = {
      lines(current) = IfStart(current, s)
      current += 1
    }
    def apply(s: Symbol) = {
      lines(current) = IfExpr(current, s)
      current += 1
    }
  }

  def Else() = {
    lines(current) = ElseStart(current)
    current += 1
  }

  def Close() = {
    lines(current) = EndIf(current)
    current += 1
  }

  // Looping
  def Loop {
    lines(current) = LoopStart()
    loopStartLines.push(current)
    current += 1
  }

  def EndLoop {
    lines(current) = LoopEnd(loopStartLines.pop())
    current += 1
  }

  // Breaking out of loops
  def Stop {
    lines(current) = Break()
    current += 1
  }

  // Functions
  object Riff {
    def apply(fnName: Symbol) = {
      fnStartLines += (fnName -> current)
      lines(current) = FnStart(fnName)
      current += 1
    }
  }

  object Return {
    def apply(value: Any) = {
      lines(current) = FnRet(value)
      current += 1
    }
  }

  def EndRiff {
    lines(current) = FnEnd()
    current += 1
  }

  // Function Calls
  object Reprise {
    def apply(fnName: Symbol) = {
      lines(current) = FnCall(fnName)
      current += 1
    }
    def apply(fnName: Symbol, variable: Symbol) = {
      lines(current) = FnCallRet(fnName, variable)
      current += 1
    }
  }

  /* Note/Sound Generation
   * Format: note, start time, duration
   *    msg.setMessage(ShortMessage.NOTE_ON, channel,note,vol)
   *    msg.setMessage(ShortMessage.NOTE_OFF, channel,note)
   * For now, only use channel 0
   */
  object Note{
    def apply(note: Int, start: Int, duration: Int, volume: Int) = GenerateNote(note, start, duration, volume)
  }


  class Bindings {
    val bindingsStack = Stack[HashMap[Symbol, Any]]()
    val bindings = HashMap[Symbol, Any]()

    /*
     * Create a new scope.
     * Call whenever doing a function call.
     */
    def createScope() {
      bindingsStack.push(new HashMap[Symbol, Any])
    }

    /*
     * Destroy topmost scope.
     * Call whenever leaving a function.
     */
    def destroyScope() {
      bindingsStack.pop()
    }

    /**
     * get correct HashMap for your scope
     */
    def getMap(sym: Symbol): HashMap[Symbol, Any] = {
      val bindingsStackCopy = Stack[HashMap[Symbol, Any]]()
      val bindingsStackTop = bindingsStack.top
      while (!bindingsStack.isEmpty && !bindingsStack.top.contains(sym)) {
        bindingsStackCopy.push(bindingsStack.pop())
      }
      //bindingsStackCopy.push(bindingsStack.pop())
      var map = bindingsStackTop
      if (!bindingsStack.isEmpty) {
        map = bindingsStack.top
      }
      while (!bindingsStackCopy.isEmpty) {
        bindingsStack.push(bindingsStackCopy.pop())
      }
      map
    }

    /**
     * set a value in our map
     */
    def set(k: Symbol, v: Any): Unit = {
      val map = getMap(k)
      map(k) = v;
    }

    /**
     * only returns integers
     */
    def num(k: Symbol): Int = {
      any(k) match {
        case n: Int => n
        case _ => throw new RuntimeException(f"Variable $k does not exist or is not an integer")
      }
    }

    /**
     * WARNING: don't use yet
     * returns ints and doubles
     */
    /* DID NOT TOUCH THIS WHILE IMPLEMENTING SCOPE */
    def anyval(k: Symbol): AnyVal = {
      any(k) match {
        case n: Int => n
        case n: Double => n
        case _ => throw new RuntimeException(f"Variable $k does not exist as type AnyVal")
      }
    }

    /**
     * returns anything
     */
    def any(k: Symbol): Any = {
      val map = getMap(k)
      map.get(k) match {
        case Some(x) => x
        case None => None
      }
    }

    override def toString: String = {
      bindingsStack.top.toString
    }
  }
}