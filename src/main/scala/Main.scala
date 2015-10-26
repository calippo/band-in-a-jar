import javax.sound.midi.{ShortMessage, MidiEvent, Sequence, MidiSystem}

case class Chord(key: Int, notes: Seq[Int] = Seq.empty)
case class ChordType(notes: Seq[Int] = Seq.empty)

//C, C#, D, D#, E, F, F#, G, G#,  A, A#,  B
//1,  2, 3,  4, 5, 6,  7, 8,  9, 10, 11, 12
object Minor extends ChordType(Seq(1, 4, 8))
object Major extends ChordType(Seq(1, 5, 8))
object Seven extends ChordType(Seq(11))
object MajorSeven extends ChordType(Seq(12))

object A extends Chord(key = 10)
object G extends Chord(key = 8)
object D extends Chord(key = 3)
object E extends Chord(key = 5)
object C extends Chord(key = 1)


case class DynamicType(velocity: Int)
object DynamicTypes {
  object Staccato extends DynamicType(96)
  object Legato extends DynamicType(72)
}

case class DynamicNote(tempo: Int, tpe: DynamicType)

object base {
  type Strumming = List[DynamicNote]
  type Progression = List[Chord]
}

import base._

object ProgressionOps {
  implicit class ProgressionOp(p: Progression) {
    def | (c: Chord): Progression = p :+ c

    def sequencify(strumming: Strumming): Sequence = {
      import MIDIFuncs._

      val events: Seq[MidiEvent] = p.foldLeft((Seq[MidiEvent](), 0)) { (acc, c) =>
        val (chords, time)= acc
        val newTime = time + strumming.map(_.tempo).sum
        (chords ++ strum(c.key, c.notes, time, strumming), newTime)
      }._1

      val sequence = new Sequence(Sequence.PPQ, 1)
      val track = sequence.createTrack()

      events.foreach(track.add(_))
      sequence
    }

    def play(strumming: Strumming): Unit = {
      val cl = classOf[javax.sound.midi.MidiSystem].getClassLoader
      Thread.currentThread.setContextClassLoader(cl)

      val sequencer = MidiSystem.getSequencer()
      MidiSystem.getSynthesizer().getAvailableInstruments().toList.map(println(_))
      sequencer.open()
      sequencer.setSequence(p.sequencify(strumming))
      sequencer.start()
    }
  }
}

object MIDIFuncs {
  private[this] def noteONMessage(key: Int, velocity: Int) = noteMessage(key, velocity, true)
  private[this] def noteOFFMessage(key: Int, velocity: Int) = noteMessage(key, velocity, false)

  private[this] def noteMessage(key: Int, velocity: Int, on: Boolean) = {
    val msg = new ShortMessage()
    val t = if (on) ShortMessage.NOTE_ON else ShortMessage.NOTE_OFF
    msg.setMessage(t, 0, 48 + key, velocity)
    msg
  }

  def note(key: Int, up: Int, down: Int, velocity: Int): (MidiEvent, MidiEvent) =
    (new MidiEvent(noteONMessage(key, velocity), up), new MidiEvent(noteOFFMessage(key, velocity), down))

  def strum(key: Int, notes: Seq[Int], time: Int, strumming: Seq[DynamicNote]): Seq[MidiEvent] = {
    strumming.foldLeft((Seq[MidiEvent](), time)) { (acc, v) =>
      val (events, startingTime) = acc
      (events ++ chord(key, notes, startingTime, v.tempo, v.tpe), v.tempo + startingTime)
    }._1
  }

  def chord(key: Int, notes: Seq[Int], up: Int, duration: Int, tpe: DynamicType): Seq[MidiEvent] =
    notes.foldLeft(Seq[MidiEvent]()) { (acc, v) =>
      val (noteUp, noteDown) = note(key + v, up, up + duration, tpe.velocity)
      acc ++ Seq(noteUp, noteDown)
    }
}

object ChordsOps {
  implicit class ChordOp(c: Chord) {
    def | (c2: Chord): Progression = {
      List(c, c2)
    }

    def + (ct: ChordType): Chord = {
      Chord(c.key, c.notes ++ ct.notes)
    }
  }
}

object setup {
  import DynamicTypes._
  import ChordsOps._

  val Am7 = A + Minor + Seven
  val Dm7 = D + Minor + Seven
  val Em7 = E + Minor + Seven

  val A7 = A + Major + Seven
  val D7 = D + Major + Seven
  val E7 = E + Major + Seven

  val S2 = DynamicNote(2, Staccato)
  val S1 = DynamicNote(1, Staccato)
  val L2 = DynamicNote(2, Legato)
  val L1 = DynamicNote(1, Legato)
}

object Main extends App {
  import ProgressionOps._
  import ChordsOps._
  import setup._

  val progression = A7 | D7 | A7 | A7 |
                    D7 | D7 | A7 | A7 |
                    E7 | D7 | A7 | A7

  val strumming = L2 :: S1 :: L2 :: L1 :: Nil

  progression.play(strumming)
}
