package band

case class DynamicNote(tempo: Int, tpe: DynamicType)

case class DynamicType(velocity: Int)
object DynamicTypes {
  object Staccato extends DynamicType(96)
  object Legato extends DynamicType(72)
  object Pause extends DynamicType(0)
}

case class BandException(msg: String) extends RuntimeException(msg)

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

package object base {
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

case class Bpm(v: Int) extends AnyVal
case class Times(v: Int) extends AnyVal
object dsl {
  sealed abstract class MusicKind
  case object jazz extends MusicKind
  case object blues extends MusicKind

  implicit class IntPimp(i: Int) {
    import Genre._
    def bar(musicKind: MusicKind): Genre = (i, musicKind) match {
      case (12, blues) => TwelveBarBlues("drums-patterns/blues.mid")
      case _ => throw new BandException("Genre not found")
    }

    def bpm = Bpm(i)

    def times = Times(i)
  }
}

import chords.basic._
