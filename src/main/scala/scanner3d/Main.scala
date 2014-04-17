package scanner3d

import com.jme3.app.SimpleApplication
import com.jme3.material.Material
import com.jme3.math._
import com.jme3.renderer.RenderManager
import com.jme3.scene._
import com.jme3.scene.shape.Box
import org.openni._
import com.jme3.light.DirectionalLight
import com.jme3.system.AppSettings
import com.jme3.input.ChaseCamera

object Main {
  def main(args: Array[String]) {
    OpenNI.initialize()
    val devicesInfo = OpenNI.enumerateDevices
    if (devicesInfo.size == 0) {
      throw new RuntimeException("No device found")
    }
    val device = Device.open(devicesInfo.get(0).getUri)
    val stream = VideoStream.create(device, SensorType.DEPTH)

    val app = new Main(stream)
    app.start
  }
}

class Main(stream: VideoStream) extends SimpleApplication {

  @volatile var appInited = false
  var first = true
  var geomOpt = Option.empty[Geometry]
  var material: Material = _
  var meshOpt = Option.empty[Mesh]

  initSettings()

  val frameTracker = new FrameTracker()
  frameTracker.start(stream) {  (depthPixels, width, height) =>
    if (appInited) {
      first = false
      meshOpt = Some(MeshGenerator.generate(depthPixels, width, height))
    }
  }

  private def initSettings() {
    setShowSettings(false)
    val settings = new AppSettings(true)
    settings.setResolution(1024, 786)
    setSettings(settings)
  }

  def simpleInitApp {
    initLight()

    val geom = initGeometry()
    geomOpt = Some(geom)
    
    initCamera(geom)

    appInited = true
  }

  private def initLight() = {
    val sun = new DirectionalLight()
    sun.setDirection(new Vector3f(1,0,-2).normalizeLocal())
    sun.setColor(ColorRGBA.White)
    rootNode.addLight(sun)
  }

  private def initGeometry() = {
    material = new Material(assetManager, "Common/MatDefs/Light/Lighting.j3md")
    material.setColor("Diffuse",ColorRGBA.White)

    val initialMesh = new Box(0, 0, 0)
    val geom = new Geometry("mesh", initialMesh)
    geom.setMaterial(material)
    rootNode.attachChild(geom)
    geom
  }

  private def initCamera(geom: Geometry) {
    flyCam.setEnabled(false)
    val chaseCam = new ChaseCamera(cam, geom, inputManager)
    chaseCam.setDefaultDistance(10)
    chaseCam.setDefaultHorizontalRotation(FastMath.PI / 2)
    chaseCam.setDefaultVerticalRotation(0)
    chaseCam.setMinVerticalRotation(Float.MinValue)
    chaseCam.setMaxVerticalRotation(Float.MaxValue)
    chaseCam.setRotationSpeed(3)
  }

  override def simpleUpdate(tpf: Float) {
    meshOpt = meshOpt.flatMap { mesh =>
      geomOpt.map(_.setMesh(mesh))
      None
    }
  }

  override def simpleRender(rm: RenderManager) {
  }

  override def stop(): Unit = {
    super.stop()
    frameTracker.stop()
  }
}