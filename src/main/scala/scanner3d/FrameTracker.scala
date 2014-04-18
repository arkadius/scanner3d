package scanner3d

import java.nio.ByteOrder
import org.openni.{VideoFrameRef, VideoStream}
import org.openni.VideoStream.NewFrameListener
import java.util.concurrent.{RejectedExecutionException, TimeUnit, Executors}
import scala.concurrent._
import java.util.concurrent.atomic.AtomicInteger

class FrameTracker {
  import FrameTracker._
  
  implicit val executionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(MAX_THREADS))

  def start(stream: VideoStream)(handleFrame: (Iterable[Short], Int, Int) => Unit) {
    val rdr = new FrameListener(handleFrame)
    stream.addNewFrameListener(rdr)
    stream.start()
  }

  def stop() {
    executionContext.shutdownNow()
    executionContext.awaitTermination(3, TimeUnit.SECONDS)
  }
}

object FrameTracker {
  val MAX_THREADS = 8
  val DELAY = 5000 / MAX_THREADS // zmierzone empirycznie
}

class FrameListener(handleFrame: (Iterable[Short], Int, Int) => Unit)(implicit executionContext: ExecutionContext) extends NewFrameListener {

  private val threadRun = new AtomicInteger(FrameTracker.MAX_THREADS)

  override def onFrameReady(stream: VideoStream) {
    try {
      future {
        delayExecutionIfNeed()

        var frame: VideoFrameRef = null
        try {
          frame = stream.readFrame()
          if (frame != null) {
            val frameData = frame.getData.order(ByteOrder.LITTLE_ENDIAN)
            val iter = new Iterable[Short]{
              override def iterator = new Iterator[Short]{
                override def next() = frameData.getShort

                override def hasNext = frameData.remaining() > 0
              }
            }

            val before = System.currentTimeMillis()
            handleFrame(iter, frame.getWidth, frame.getHeight)
            println(s"handle frame took ${System.currentTimeMillis() - before}")
          }
        } finally {
          Option(frame).map(_.release())
        }
      }
    } catch {
      case ex:RejectedExecutionException => // czekaj na następną
    }
  }

  private def delayExecutionIfNeed() {
    val currThreadRun = threadRun.getAndDecrement
    if (currThreadRun > 0) {
      Thread.sleep((FrameTracker.MAX_THREADS - currThreadRun) * FrameTracker.DELAY)
    }
  }
}