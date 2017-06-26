package band

object Main extends App {
  import dsl._

  import chords.basic._
  import ops._

  val progression =
    A7  | D7 | A7 | A7 |
    D7 | D7 | A7 | A7 |
    E7  | D7 | A7 | E7

  progression write ("out.mid", 12 bar blues, 80 bpm, 2 times)
  System.exit(1)
}
