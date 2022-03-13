package net.soundmining

import de.sciss.osc.UDP.Receiver
import de.sciss.osc.{Message, Packet, PacketCodec, UDP}
import net.soundmining.Generative.{MarkovChain, WeightedRandom}
import net.soundmining.modular.ModularInstrument.ControlInstrument
import net.soundmining.modular.ModularSynth.{lineControl, relativePercControl, relativeThreeBlockcontrol, staticControl}
import net.soundmining.modular.SynthPlayer
import net.soundmining.synth.{Instrument, SuperColliderClient}
import net.soundmining.synth.SuperColliderClient.loadDir

import java.net.SocketAddress
import scala.util.Random

/**
 * Mix noise and pitched tones. Maybe with a MarkovChain between noise and pitch
 * and a second layer of MarkovChain to choose which noise and pitch.
 *
 */
object AmbientMusic4 {

  implicit val client: SuperColliderClient = SuperColliderClient()
  val SYNTH_DIR = "/Users/danielstahl/Documents/Projects/soundmining-modular/src/main/sc/synths"
  val synthPlayer = SynthPlayer(soundPlays = Map.empty, numberOfOutputBuses = 64)
  var midiServer: Receiver.Undirected = _
  implicit val random: Random = new Random()
  val STATIC_MIDI_DELAY_TIME = 1900
  
  def init(): Unit = {
    println("Starting up SuperCollider client")
    client.start
    Instrument.setupNodes(client)
    client.send(loadDir(SYNTH_DIR))
    synthPlayer.init()

    val cfg = UDP.Config()
    cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
    cfg.localPort = 57111
    this.midiServer = UDP.Receiver(cfg)
    this.midiServer.connect()
    this.midiServer.action = midiReply
  }

  def randomRange(min: Double, max: Double)(implicit random: Random): Double =
    min + (max - min) * random.nextDouble()

  def stop(): Unit = {
    println("Stopping SuperCollider client")
    client.stop
    this.midiServer.close()
  }

  def makeAttackCurve(min: Double, max: Double)(implicit random: Random): Seq[Double] = {
    val curve = randomRange(min, max)
    Seq(curve, curve * -1)
  }

  trait Patch {
    def noteHandle(start: Double, key: Int, velocity: Int): Unit;
  }

  var middlePitchFundamental: Double = Note.noteToHertz("aiss1")
  var middlePitchSecond: Double = Note.noteToHertz("e3")

  def spikeVolume(volume: Double): (() => ControlInstrument, Double) = {
    val attackTime = randomRange(0.5, 0.8)
    val attackCurve = makeAttackCurve(1, 3)
    val duration = randomRange(5.0, 8.0)
    println(s"Spike volume attack $attackTime curve $attackCurve, duration $duration")
    (() => relativePercControl(0.0001, volume, attackTime, Left(attackCurve)), duration)
  }

  def plainVolume(volume: Double): (() => ControlInstrument, Double) = {

    val attackTime = randomRange(0.1, 0.2)
    val decayTime = randomRange(0.1, 0.2)
    val attackDecayCurve = makeAttackCurve(0, 1)
    val volumeDecay = volume * randomRange(0.8, 1.0)
    val duration = randomRange(13.0, 21.0)

    println(s"Plain time $attackTime ${1 - attackTime - decayTime} $decayTime values $volume $volumeDecay, duration $duration")
    (() => relativeThreeBlockcontrol(0.0001, attackTime, volume, volumeDecay, decayTime, 0.0001,
      Left(Seq(attackDecayCurve.head, 0, attackDecayCurve(1)))), duration)
  }

  val lowVolumeChoose: WeightedRandom[Double => (() => ControlInstrument, Double)] =
    WeightedRandom(Seq((spikeVolume, 0.4), (plainVolume, 0.6)))

  val highVolumeChoose: WeightedRandom[Double => (() => ControlInstrument, Double)] =
    WeightedRandom(Seq((spikeVolume, 0.6), (plainVolume, 0.4)))

  val triangleSpectrumPatch1 = TriangleSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5)
  val triangleSpectrumPatch2 = TriangleSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10)
  val triangleSpectrumPatch3 = TriangleSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 20, 30)
  val triangleSpectrumPatch4 = TriangleSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 30, 20)

  val triangleSpectrumPatch5 = TriangleSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5)
  val triangleSpectrumPatch6 = TriangleSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10)
  val triangleSpectrumPatch7 = TriangleSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 20, 30)
  val triangleSpectrumPatch8 = TriangleSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 30, 20)

  val noiseSpectrumPatch1 = NoiseSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5)
  val noiseSpectrumPatch2 = NoiseSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10)
  val noiseSpectrumPatch3 = NoiseSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 20, 30)
  val noiseSpectrumPatch4 = NoiseSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 20, 30)

  val dustSpectrumPatch1 = DustSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5)
  val dustSpectrumPatch2 = DustSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10)
  val dustSpectrumPatch3 = DustSpectrumInstrument(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 20, 30)
  val dustSpectrumPatch4 = DustSpectrumInstrument(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 20, 30)

  val panChooser: () => Double = () => randomRange(-0.9, 0.9)

  val spectrumChooser = WeightedRandom(pairs = Seq(((true, true), 0.4), ((false, true), 0.3), ((true, false), 0.3)))
  val dustChooser = WeightedRandom(pairs = Seq((true, 0.5), (false, 0.5)))
  val lowChooseSpectrumPatch1 = ChooseSpectrumInstrument(
    triangleSpectrumPatch1, noiseSpectrumPatch1, dustSpectrumPatch1,
    spectrumChooser,
    dustChooser, panChooser, lowVolumeChoose)

  val highChooseSpectrumPatch1 = ChooseSpectrumInstrument(
    triangleSpectrumPatch3, noiseSpectrumPatch3, dustSpectrumPatch3,
    spectrumChooser,
    dustChooser, panChooser, highVolumeChoose)

  val lowChooseSpectrumPatch2 = ChooseSpectrumInstrument(
    triangleSpectrumPatch2, noiseSpectrumPatch2, dustSpectrumPatch2,
    spectrumChooser,
    dustChooser, panChooser, lowVolumeChoose)

  val highChooseSpectrumPatch2 = ChooseSpectrumInstrument(
    triangleSpectrumPatch4, noiseSpectrumPatch4, dustSpectrumPatch4,
    spectrumChooser,
    dustChooser, panChooser, highVolumeChoose)


  def makeChooseSpectrumPatch(fundamental: Double, second: Double, ring1: Int, ring2: Int, volumeChoose: WeightedRandom[Double => (() => ControlInstrument, Double)] = lowVolumeChoose): ChooseSpectrumInstrument =
    ChooseSpectrumInstrument(
      TriangleSpectrumInstrument(fundamental, second, ring1, ring2),
      NoiseSpectrumInstrument(fundamental, second, ring1, ring2),
      DustSpectrumInstrument(fundamental, second, ring1, ring2),
      spectrumChooser, dustChooser, panChooser, volumeChoose)


  val octavePatch = OctavePatch(Map(2 -> lowChooseSpectrumPatch1, 3 -> highChooseSpectrumPatch1, 4 -> lowChooseSpectrumPatch2, 5 -> highChooseSpectrumPatch2))


  val octaveHarmonyPatch = OctavePatch(Map(
    2 -> makeChooseSpectrumPatch(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5),
    3 -> makeChooseSpectrumPatch(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10),
    4 -> makeChooseSpectrumPatch(Note.noteToHertz("h0"), Note.noteToHertz("aiss2"), 10, 5),
    5 -> makeChooseSpectrumPatch(Note.noteToHertz("d1"), Note.noteToHertz("fiss2"), 5, 10)))

  // https://www.youtube.com/watch?v=W2D_PzOVfT0
  case class OctavePatch(patches: Map[Int, Patch]) extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int): Unit = {
      val octave = (key / 12) - 1
      println(s"octave $octave")
      patches.get(octave).foreach(_.noteHandle(start, key, velocity))
    }
  }


  case class SpectrumInstrumentWrapperPatch(spectrumInstrument: SpectrumInstrument, panChooser: () => Double,
                                            volumeChooser: WeightedRandom[Double => (() => ControlInstrument, Double)]) extends Patch {
    override def noteHandle(start: Double, key: Int, velocity: Int): Unit = {
      val volume = volumeChooser.choose()
      val pan = (panChooser(), panChooser())
      spectrumInstrument.noteHandle(start, key, velocity, pan, volume)
    }
  }

  val trianglePatch = SpectrumInstrumentWrapperPatch(triangleSpectrumPatch1, panChooser, lowVolumeChoose)

  case class ChooseSpectrumInstrument(trianglePatch: TriangleSpectrumInstrument, noisePatch: NoiseSpectrumInstrument, dustPatch: DustSpectrumInstrument,
                                      spectrumChooser: WeightedRandom[(Boolean, Boolean)],
                                      dustChooser: WeightedRandom[Boolean],
                                      panChooser: () => Double,
                                      volumeChooser: WeightedRandom[Double => (() => ControlInstrument, Double)]) extends SpectrumInstrument with Patch {

    override def noteHandle(start: Double, key: Int, velocity: Int): Unit = {
      val volume = volumeChooser.choose()
      val pan = (panChooser(), panChooser())
      noteHandle(start, key, velocity, pan, volume)
    }

    def noteHandle(start: Double, key: Int, velocity: Int, pan: (Double, Double), volumeDurationController: Double => (() => ControlInstrument, Double)): Unit = {
      val (triangle, noise) = spectrumChooser.choose()
      val dust = dustChooser.choose()

      val invPan = (pan._2, pan._1)

      val dustPan = pan
      if(triangle) trianglePatch.noteHandle(start, key, velocity, pan, volumeDurationController)
      if(noise) noisePatch.noteHandle(start, key, velocity, invPan, volumeDurationController)
      if(dust) dustPatch.noteHandle(start, key, velocity, dustPan, volumeDurationController)
    }
  }

  trait SpectrumInstrument {
    def noteHandle(start: Double, key: Int, velocity: Int, pan: (Double, Double), volumeDurationController: Double => (() => ControlInstrument, Double)): Unit
  }

  trait AbstractSpectrumInstrument extends SpectrumInstrument {
    val fundamental: Double
    val second: Double
    val ring1: Int
    val ring2: Int

    val fact = Spectrum.makeFact(fundamental, second)

    val spectrum = Spectrum.makeSpectrum2(fundamental, fact, 50)
  }

  case class TriangleSpectrumInstrument(fundamental: Double, second: Double, ring1: Int, ring2: Int) extends AbstractSpectrumInstrument {

    override def noteHandle(start: Double, key: Int, velocity: Int, pan: (Double, Double), volumeDurationController: Double => (() => ControlInstrument, Double)): Unit = {
      val note = key % 12

      val spectrumNote = spectrum(note)

      val (volume, duration) = volumeDurationController(velocity / 127.0)

      println(s"Triangle start $start note $note duration $duration ring $ring1 $ring2 pan $pan" )

      synthPlayer()
        .triangle(staticControl(spectrumNote), volume())
        .ring(staticControl(spectrum(ring1)))
        .pan(lineControl(pan._1 - 0.1, pan._2 + 0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .triangle(staticControl(spectrumNote), volume())
        .ring(staticControl(spectrum(ring2)))
        .pan(lineControl(pan._1 + 0.1, pan._2 - 0.1))
        .playWithDuration(start, duration)
    }
  }

  case class NoiseSpectrumInstrument(fundamental: Double, second: Double, ring1: Int, ring2: Int) extends AbstractSpectrumInstrument {
    override def noteHandle(start: Double, key: Int, velocity: Int, pan: (Double, Double), volumeDurationController: Double => (() => ControlInstrument, Double)): Unit = {
      val note = key % 12

      val spectrumNote = spectrum(note)

      val rq = 10
      val velocityFactor = (1 / math.sqrt(rq / spectrumNote)) * 10

      val (volume, duration) = volumeDurationController((velocity / 127.0) * velocityFactor)

      println(s"Noise note start $start $note duration $duration ring $ring1 $ring2 pan $pan" )

      synthPlayer()
        .pinkNoise(volume())
        .bandPass(staticControl(spectrumNote), staticControl(rq / spectrumNote))
        .ring(staticControl(spectrum(ring1)))
        .pan(lineControl(pan._1 - 0.1, pan._2 + 0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .pinkNoise(volume())
        .bandPass(staticControl(spectrumNote), staticControl(rq / spectrumNote))
        .ring(staticControl(spectrum(ring2)))
        .pan(lineControl(pan._1 + 0.1, pan._2 - 0.1))
        .playWithDuration(start, duration)
    }
  }

  case class DustSpectrumInstrument(fundamental: Double, second: Double, ring1: Int, ring2: Int) extends AbstractSpectrumInstrument {
    override def noteHandle(start: Double, key: Int, velocity: Int, pan: (Double, Double), volumeDurationController: Double => (() => ControlInstrument, Double)): Unit = {
      val note = key % 12

      val spectrumNote = spectrum(note)

      val rq = 20
      val velocityFactor = (1 / math.sqrt(rq / spectrumNote)) * 100

      val (volume, duration) = volumeDurationController((velocity / 127.0) * velocityFactor)

      println(s"Dust start $start note $note duration $duration ring $ring1 $ring2 pan $pan" )

      synthPlayer()
        .dust(volume(), staticControl(spectrumNote / 100))
        .bandPass(staticControl(spectrumNote), staticControl(rq / spectrumNote))
        .ring(staticControl(spectrum(ring1)))
        .pan(lineControl(pan._1 - 0.1, pan._2 + 0.1))
        .playWithDuration(start, duration)

      synthPlayer()
        .dust(volume(), staticControl(spectrumNote / 100))
        .bandPass(staticControl(spectrumNote), staticControl(rq / spectrumNote))
        .ring(staticControl(spectrum(ring2)))
        .pan(lineControl(pan._1 + 0.1, pan._2 - 0.1))
        .playWithDuration(start, duration)
    }
  }

  val patch: Patch = octaveHarmonyPatch

  val totalTime = 13.0 * 60.0

  def evenMarkovChain[T](values: Seq[T], startValue: T): MarkovChain[T] = {
    val rate = 1.0 / (values.size - 1)
    val nodes = values.map(value => (value, values.filter(_ != value).map((_, rate)))).toMap
    MarkovChain(nodes, startValue)
  }

  val notes = evenMarkovChain(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8 ,9, 10, 11), 0)

  val allHarmonyPatch = OctavePatch(Map(
    0 -> makeChooseSpectrumPatch(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 10, 5, lowVolumeChoose),
    1 -> makeChooseSpectrumPatch(Note.noteToHertz("c1"), Note.noteToHertz("giss2"), 20, 30, highVolumeChoose),
    2 -> makeChooseSpectrumPatch(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 5, 10, lowVolumeChoose),
    3 -> makeChooseSpectrumPatch(Note.noteToHertz("f1"), Note.noteToHertz("a2"), 30, 20, highVolumeChoose),
    4 -> makeChooseSpectrumPatch(Note.noteToHertz("h0"), Note.noteToHertz("aiss2"), 10, 5, lowVolumeChoose),
    5 -> makeChooseSpectrumPatch(Note.noteToHertz("h0"), Note.noteToHertz("aiss2"), 20, 30, highVolumeChoose),
    6 -> makeChooseSpectrumPatch(Note.noteToHertz("d1"), Note.noteToHertz("fiss2"), 5, 10, lowVolumeChoose),
    7 -> makeChooseSpectrumPatch(Note.noteToHertz("d1"), Note.noteToHertz("fiss2"), 30, 20, highVolumeChoose)))

  def playPiece(start: Double = 0, reset: Boolean = true): Unit = {
    if(reset) client.resetClock

    val partDuration = (13.0 * 60.0) / 7.0
    val parts = Melody.absolute(start, Seq.fill(7)(partDuration))
      .map(partStart => (partStart, partStart + partDuration))
      .zipWithIndex.map {
      case ((partStart, partEnd), 0) =>
        (partStart, partEnd, allHarmonyPatch.patches(0), allHarmonyPatch.patches(1))
      case ((partStart, partEnd), 1) =>
        (partStart, partEnd, allHarmonyPatch.patches(2), allHarmonyPatch.patches(3))
      case ((partStart, partEnd), 2) =>
        (partStart, partEnd, allHarmonyPatch.patches(4), allHarmonyPatch.patches(5))
      case ((partStart, partEnd), 3) =>
        (partStart, partEnd, allHarmonyPatch.patches(6), allHarmonyPatch.patches(7))
      case ((partStart, partEnd), 4) =>
        (partStart, partEnd, allHarmonyPatch.patches(4), allHarmonyPatch.patches(5))
      case ((partStart, partEnd), 5) =>
        (partStart, partEnd, allHarmonyPatch.patches(2), allHarmonyPatch.patches(3))
      case ((partStart, partEnd), 6) =>
        (partStart, partEnd, allHarmonyPatch.patches(0), allHarmonyPatch.patches(1))
    }

    var lowTime = start

    while (lowTime < totalTime) {
      println(s"finding time $lowTime out of $totalTime")
      val (_, _, lowPatch, _) = parts.find {
        case (partStart, partEnd, _, _) => lowTime >= partStart && lowTime < partEnd
      }.get

      val key = notes.next
      val velocity = randomRange(15, 110).toInt
      lowPatch.noteHandle(lowTime, key, velocity)
      val nextTime = randomRange(3, 5)
      lowTime += nextTime
    }

    var highTime = start
    while (highTime < totalTime) {
      parts.find {
        case (partStart, partEnd, _, _) => highTime >= partStart + (partDuration / 2) && highTime < partEnd
      }.foreach {
        case (_, _, _, highPatch) =>
          val key = notes.next
          val velocity = randomRange(50, 110).toInt
          highPatch.noteHandle(highTime, key, velocity)
      }
      val nextTime = randomRange(13, 21)
      highTime += nextTime
    }

    val partTimes = parts.map {
      case (start, end, _, _) => (start / 60, (start + (partDuration / 2)) / 60, end / 60)
    }
    println(s"All times $partTimes")
  }

  def midiReply(packet: Packet, socketAddress: SocketAddress): Unit = {
    packet match {
      case Message("/noteOn", key: Int, velocity: Int) =>
        if (client.clockTime <= 0) client.resetClock
        val start = (System.currentTimeMillis() - (client.clockTime + STATIC_MIDI_DELAY_TIME)) / 1000.0
        patch.noteHandle(start, key, velocity)
      case _ =>
    }
  }
}
