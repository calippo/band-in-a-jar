package band

import band.base._

object ops {
  import chords._
  implicit class ChordOp(c: Chord) {
    def | (c2: Chord): Progression = {
      List(c, c2)
    }
  }

  implicit class ProgressionOp(p: Progression) {
    def | (c: Chord): Progression = p :+ c

    def play(genre: Genre, bpm: Bpm, times: Times, _strumming: Option[Strumming] = None): Unit = {
      import midi.ProgressionMidiOps._
      p playMidi (genre, bpm, times, _strumming)
    }
  }
}
