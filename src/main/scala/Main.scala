package band

object Main extends App {
  import dsl._

  import chords.basic._
  import ops._

  val progression =
    F7  | Bb7 | F7 | F7 |
    Bb7 | Bb7 | F7 | F7 |
    C7  | Bb7 | F7 | F7

  progression play (12 bar blues, 80 bpm, 5 times)
}
