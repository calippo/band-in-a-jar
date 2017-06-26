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
    import midi.ProgressionMidiOps._

    def | (c: Chord): Progression = p :+ c

    def play(genre: Genre, bpm: Bpm, times: Times, _strumming: Option[Strumming] = None): Unit =
      p playMidi (genre, bpm, times, _strumming)

    def write(filename: String, genre: Genre, bpm: Bpm, times: Times, _strumming: Option[Strumming] = None): Unit =
      p writeMidi (filename, genre, bpm, times, _strumming)
  }
}
