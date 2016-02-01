package band.midi

import javax.sound.midi.{ShortMessage, MidiEvent, Sequence, MidiSystem}
import band.base._
import band._

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

object ProgressionMidiOps {
  implicit class ProgressionMidiOps(p: Progression) {
    def events(strumming: Strumming): Seq[MidiEvent] = p.foldLeft((Seq[MidiEvent](), 0)) { (acc, c) =>
      val (chords, time)= acc
      val newTime = time + strumming.map(_.tempo).sum
      (chords ++ MIDI.strumEvents(c.key, c.notes, time, strumming), newTime)
    }._1


    private[this] def sequencify(strumming: Strumming): Sequence = {
      val sequence = new Sequence(Sequence.PPQ, 4)
      val track = sequence.createTrack()
      val events = p.events(strumming)

      events.foreach(track.add(_))
      sequence
    }

    def playMidi(genre: Genre, bpm: Bpm, times: Times, _strumming: Option[Strumming] = None): Unit = {
      val cl = classOf[javax.sound.midi.MidiSystem].getClassLoader
      Thread.currentThread.setContextClassLoader(cl)

      val sequencer = MidiSystem.getSequencer()

      val strumming = _strumming.getOrElse(genre.strumming)
      val is = new java.io.FileInputStream(genre.midiName)
      val s = MidiSystem.getSequence(is)
      sequencer.open()
      sequencer.setTempoInBPM(bpm.v)
      sequencer.setLoopCount(times.v)
      val track = s.createTrack()
      val events = p.events(strumming)
      events.foreach(track.add(_))
      sequencer.setSequence(s)
      sequencer.start()
    }
  }
}
