package band
import javax.sound.midi.{ShortMessage, MidiEvent, Sequence, MidiSystem}

case class BandException(msg: String) extends RuntimeException(msg)

sealed abstract class Genre {
  import base._
  val midiName: String
  val strumming: Strumming
}

object Genre {
  import strumSetup._
  case class TwelveBarBlues(midiName: String) extends Genre {
    val strumming =
      S1 :: P1 :: L1 ::
      S1 :: P1 :: S1 ::
      L1 :: P1 :: S1 ::
      L1 :: L1 :: S1 :: Nil
  }
}

case class DynamicType(velocity: Int)
object DynamicTypes {
  object Staccato extends DynamicType(96)
  object Legato extends DynamicType(72)
  object Pause extends DynamicType(0)
}

case class DynamicNote(tempo: Int, tpe: DynamicType)

object base {
  import chords._
  type Strumming = List[DynamicNote]
  type Progression = List[Chord]

  trait DrumsPattern
  case object SlowDrums extends DrumsPattern
  case object MediumDrums extends DrumsPattern
  case object FastDrums extends DrumsPattern

  trait Instrument

  case class Band(progression: Progression,
    drumsPattern: DrumsPattern,
    instrumentsPatterns: Map[Instrument, Strumming])
}

import base._

object ProgressionOps {
  implicit class ProgressionOp(p: Progression) {
    import midi._
    import chords._

    def | (c: Chord): Progression = p :+ c

    def events(strumming: Strumming): Seq[MidiEvent] = p.foldLeft((Seq[MidiEvent](), 0)) { (acc, c) =>
      val (chords, time)= acc
      val newTime = time + strumming.map(_.tempo).sum
      (chords ++ MIDI.strumEvents(c.key, c.notes, time, strumming), newTime)
    }._1


    def sequencify(strumming: Strumming): Sequence = {
      val sequence = new Sequence(Sequence.PPQ, 4)
      val track = sequence.createTrack()
      val events = p.events(strumming)

      events.foreach(track.add(_))
      sequence
    }

    def play(genre: Genre, bpm: Bpm, _strumming: Option[Strumming] = None): Unit = {
      val cl = classOf[javax.sound.midi.MidiSystem].getClassLoader
      Thread.currentThread.setContextClassLoader(cl)

      val sequencer = MidiSystem.getSequencer()

      val strumming = _strumming.getOrElse(genre.strumming)
      val is = new java.io.FileInputStream(genre.midiName)
      val s = MidiSystem.getSequence(is)
      sequencer.open()
      sequencer.setTempoInBPM(bpm.v)
      sequencer.setLoopCount(5)
      val track = s.createTrack()
      val events = p.events(strumming)
      events.foreach(track.add(_))
      sequencer.setSequence(s)
      sequencer.start()
    }
  }
}

package midi {
  object MIDI {
    private[midi] def noteONMessage(key: Int, velocity: Int) = noteMessage(key, velocity, true)
    private[midi] def noteOFFMessage(key: Int, velocity: Int) = noteMessage(key, velocity, false)

    private[midi] def noteMessage(key: Int, velocity: Int, on: Boolean) = {
      val command = if (on) ShortMessage.NOTE_ON else ShortMessage.NOTE_OFF
      new ShortMessage(command, 0, 48 + key, velocity)
    }

    def noteEvents(key: Int, up: Int, down: Int, velocity: Int): (MidiEvent, MidiEvent) =
      (new MidiEvent(noteONMessage(key, velocity), up),
        new MidiEvent(noteOFFMessage(key, velocity), down))

    def strumEvents(key: Int, notes: Seq[Int], time: Int, strumming: Seq[DynamicNote])
    : Seq[MidiEvent] = {
      strumming.foldLeft((Seq[MidiEvent](), time)) { (acc, v) =>
        val (events, startingTime) = acc
        (events ++ chordEvents(key, notes, startingTime, v.tempo, v.tpe), v.tempo + startingTime)
      }._1
    }

    def chordEvents(key: Int, notes: Seq[Int], up: Int, duration: Int, tpe: DynamicType)
    : Seq[MidiEvent] =
      notes.foldLeft(Seq[MidiEvent]()) { (acc, v) =>
        val (noteUp, noteDown) = noteEvents(key + v, up, up + duration, tpe.velocity)
        acc ++ Seq(noteUp, noteDown)
      }
  }
}

package chords {
  case class Chord(key: Int, notes: Seq[Int] = Seq.empty)
  case class ChordType(notes: Seq[Int] = Seq.empty)

  //C, C#, D, D#, E, F, F#, G, G#,  A, A#,  B
  //1,  2, 3,  4, 5, 6,  7, 8,  9, 10, 11, 12
  object Minor extends ChordType(Seq(1, 4, 8))
  object Major extends ChordType(Seq(1, 5, 8))
  object Seven extends ChordType(Seq(11))
  object MajorSeven extends ChordType(Seq(12))

  object A extends Chord(key = 10)
  object B extends Chord(key = 12)
  object G extends Chord(key = 8)
  object D extends Chord(key = 3)
  object E extends Chord(key = 5)
  object C extends Chord(key = 1)

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

  object basic {
    import ChordsOps._

    val Am7 = A + Minor + Seven
    val Dm7 = D + Minor + Seven
    val Em7 = E + Minor + Seven

    val B7 = B + Major + Seven
    val A7 = A + Major + Seven
    val D7 = D + Major + Seven
    val E7 = E + Major + Seven

    val G7 = G + Major + Seven

    val Cmaj7 = C + Major + MajorSeven
  }
}

case class Bpm(v: Int) extends AnyVal

object dsl {
  sealed abstract class MusicKind
  object MusicKind {
    case object jazz extends MusicKind
    case object blues extends MusicKind
  }

  implicit class IntPimp(i: Int) {
    import MusicKind._
    import Genre._
    def bar(musicKind: MusicKind): Genre = (i, musicKind) match {
      case (12, blues) => TwelveBarBlues("blues.mid")
      case _ => throw new BandException("Genre not found")
    }

    def bpm = Bpm(i)
  }
}

object strumSetup {
  import DynamicTypes._
  val S4 = DynamicNote(4*80, Staccato)
  val S2 = DynamicNote(2*80, Staccato)
  val S1 = DynamicNote(1*80, Staccato)
  val L4 = DynamicNote(4*80, Legato)
  val L2 = DynamicNote(2*80, Legato)
  val L1 = DynamicNote(1*80, Legato)
  val P1 = DynamicNote(1*80, Pause)
}

object Main extends App {
  import dsl._
  import chords.basic._
  import ProgressionOps._
  import chords.ChordsOps._
  import MusicKind._

  val progression =
    E7 | A7 | E7 | E7 |
    A7 | A7 | A7 | A7 |
    B7 | A7 | E7 | E7

  progression play (12 bar blues, 60 bpm)
}
