package band.chords

import band.base._
case class Chord(key: Int, notes: Seq[Int] = Seq.empty)
case class ChordType(notes: Seq[Int] = Seq.empty)

//C, C#, D, D#, E, F, F#, G, G#,  A, A#,  B
//1,  2, 3,  4, 5, 6,  7, 8,  9, 10, 11, 12
object Minor extends ChordType(Seq(1, 4, 8))
object Major extends ChordType(Seq(1, 5, 8))
object Seven extends ChordType(Seq(11))
object MajorSeven extends ChordType(Seq(12))

object A extends Chord(key = 10)
object Bb extends Chord(key = 11)
object B extends Chord(key = 12)
object G extends Chord(key = 8)
object D extends Chord(key = 3)
object F extends Chord(key = 6)
object E extends Chord(key = 5)
object C extends Chord(key = 1)

object ChordsOps {
  implicit class ChordOp(c: Chord) {
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

  val C7 = C + Major + Seven
  val B7 = B + Major + Seven
  val Bb7 = Bb + Major + Seven
  val A7 = A + Major + Seven
  val D7 = D + Major + Seven
  val E7 = E + Major + Seven
  val G7 = G + Major + Seven
  val F7 = F + Major + Seven

  val Cmaj7 = C + Major + MajorSeven
}
