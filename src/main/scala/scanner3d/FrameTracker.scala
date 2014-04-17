package scanner3d

import java.nio.ByteOrder
import com.primesense.nite._

object FrameTracker {

  def start(mTracker: UserTracker)(handleFrame: (Array[Short], Int, Int) => Unit) {
    val rdr = new FrameTracker(handleFrame)
    mTracker.addNewFrameListener(rdr)    
  }

}

class FrameTracker(handleFrame: (Array[Short], Int, Int) => Unit) extends UserTracker.NewFrameListener {

  private var mDepthPixels: Array[Short] = _
  private var mLastFrame: UserTrackerFrameRef = _

  def onNewFrame(tracker: UserTracker) {
    if (mLastFrame != null) {
      mLastFrame.release()
      mLastFrame = null
    }
    mLastFrame = tracker.readFrame
    val depthFrame = mLastFrame.getDepthFrame
    if (depthFrame != null) {
      val frameData = depthFrame.getData.order(ByteOrder.LITTLE_ENDIAN)
      if (mDepthPixels == null || mDepthPixels.length < depthFrame.getWidth * depthFrame.getHeight) {
        mDepthPixels = new Array[Short](depthFrame.getWidth * depthFrame.getHeight)
      }
      var pos = 0
      while (frameData.remaining > 0) {
        mDepthPixels(pos) = frameData.getShort
        pos += 1
      }
      handleFrame(mDepthPixels, depthFrame.getWidth, depthFrame.getHeight)
    }
  }  
}