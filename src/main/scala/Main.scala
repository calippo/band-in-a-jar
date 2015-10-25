import javax.sound.midi.{ShortMessage, MidiEvent, Sequence, MidiSystem}

case class Chord(key: Int, notes: Seq[Int] = Seq.empty)
case class ChordType(notes: Seq[Int] = Seq.empty)

//C, C#, D, D#, E, F, F#, G, G#,  A, A#,  B
//1,  2, 3,  4, 5, 6,  7, 8,  9, 10, 11, 12
object Minor extends ChordType(Seq(1, 5, 7))
object Major extends ChordType(Seq(1, 5, 8))
object Seven extends ChordType(Seq(11))

object A extends Chord(key = 10)
object D extends Chord(key = 3)
object E extends Chord(key = 5)

case class Progression(chords: List[Chord])

object ProgressionOps {
  implicit class ProgressionOp(p: Progression) {
    def |(c: Chord): Progression = Progression(p.chords :+ c)

    def sequencify(strumming: List[Int]): Sequence = {
      import MIDIFuncs._

      val events: Seq[MidiEvent] = p.chords.foldLeft((Seq[MidiEvent](), 0)) { (acc, c) =>
        val (chords, time)= acc
        (chords ++ strum(c.key, c.notes, time, strumming), time + strumming.sum)
      }._1

      val sequence = new Sequence(Sequence.PPQ, 1)
      val track = sequence.createTrack()
      events.foreach(track.add(_))
      sequence
    }

    def play(strumming: List[Int]): Unit = {
      val cl = classOf[javax.sound.midi.MidiSystem].getClassLoader
      Thread.currentThread.setContextClassLoader(cl)

      val sequencer = MidiSystem.getSequencer()
      sequencer.open()
      sequencer.setSequence(p.sequencify(strumming))
      sequencer.start()
    }
  }
}

object MIDIFuncs {
  def noteONMessage(key: Int) = noteMessage(key, true)
  def noteOFFMessage(key: Int) = noteMessage(key, false)

  def noteMessage(key: Int, on: Boolean) = {
    val msg = new ShortMessage()
    val t = if (on) ShortMessage.NOTE_ON else ShortMessage.NOTE_OFF
    msg.setMessage(t, 0, 48 + key, 127)
    msg
  }

  def note(key: Int, up: Int, down: Int): (MidiEvent, MidiEvent) =
    (new MidiEvent(noteONMessage(key), up), new MidiEvent(noteOFFMessage(key), down))

  def strum(key: Int, notes: Seq[Int], time: Int, strumming: Seq[Int]): Seq[MidiEvent] = {
    strumming.foldLeft((Seq[MidiEvent](), time)) { (acc, v) =>
      val (events, startingTime) = acc
      (events ++ chord(key, notes, startingTime, v), v + startingTime)
    }._1
  }

  def chord(key: Int, notes: Seq[Int], up: Int, duration: Int): Seq[MidiEvent] =
    notes.foldLeft(Seq[MidiEvent]()) { (acc, v) =>
      val (noteUp, noteDown) = note(key + v, up, up + duration)
      acc ++ Seq(noteUp, noteDown)
    }
}

object ChordsOps {
  implicit class ChordOp(c: Chord) {
    def |(c2: Chord): Progression = {
      Progression(List(c, c2))
    }

    def + (ct: ChordType): Chord = {
      Chord(c.key, c.notes ++ ct.notes)
    }
  }
}

object Main extends App {
  import ChordsOps._
  import ProgressionOps._

  val Am7 = A + Minor + Seven
  val Dm7 = D + Minor + Seven
  val Em7 = E + Minor + Seven

  val progression = Am7 | Dm7 | Am7 | Am7 | Dm7 | Dm7 | Am7 | Am7 | Em7 | Dm7 | Am7 | Am7

//  val strumming = 1  ͜ 2 • 1  ͜ 2 • 1

  val strumming = List(2,1,2,1)
  progression.play(strumming)
}
