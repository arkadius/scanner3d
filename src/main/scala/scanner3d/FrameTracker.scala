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

  def start(stream: VideoStream)(handleFrame: (Array[Short], Int, Int) => Unit) {
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
  val MAX_THREADS = 7
  val DELAY = 1000 // zmierzone empirycznie
}

class FrameListener(handleFrame: (Array[Short], Int, Int) => Unit)(implicit executionContext: ExecutionContext) extends NewFrameListener {

  private var mDepthPixels: Array[Short] = _
  private var mLastFrame: VideoFrameRef = _

  private val threadRun = new AtomicInteger(FrameTracker.MAX_THREADS)

  override def onFrameReady(stream: VideoStream) {
    if (mLastFrame != null) {
      mLastFrame.release()
      mLastFrame = null
    }
    mLastFrame = stream.readFrame()
    if (mLastFrame != null) {
      val frameData = mLastFrame.getData.order(ByteOrder.LITTLE_ENDIAN)
      if (mDepthPixels == null || mDepthPixels.length < mLastFrame.getWidth * mLastFrame.getHeight) {
        mDepthPixels = new Array[Short](mLastFrame.getWidth * mLastFrame.getHeight)
      }
      var pos = 0
      while (frameData.remaining > 0) {
        mDepthPixels(pos) = frameData.getShort
        pos += 1
      }
      try {
        future {
          delayExecutionIfNeed()
          val before = System.currentTimeMillis()
          handleFrame(mDepthPixels, mLastFrame.getWidth, mLastFrame.getHeight)
          println(s"handle frame took ${System.currentTimeMillis()-before}")
        }
      } catch {
        case ex:RejectedExecutionException => // czekaj na następną
      }
    }
  }

  private def delayExecutionIfNeed() {
    val currThreadRun = threadRun.getAndDecrement
    if (currThreadRun >= 0) {
      Thread.sleep((FrameTracker.MAX_THREADS - currThreadRun) * FrameTracker.DELAY)
    }
  }
}