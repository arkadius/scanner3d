package scanner3d

import com.jme3.scene.Mesh
import com.jme3.math.{Vector2f, Vector3f}
import com.jme3.scene.VertexBuffer.Type
import com.jme3.util.BufferUtils

/**
 * Created by arkadius on 15.04.14.
 */
object MeshGenerator {

  val WIDTH  = 4f
  val HEIGHT = 3f
  val DEPTH  = 3f

  def generate(depthPixels: Seq[Short], width: Int, height: Int): Mesh = {
    val mesh = new Mesh()

    val filtered = depthPixels.filter(_ > 0)
    val sum = filtered.foldLeft(0L) { _ + _ }
    val mean = sum.asInstanceOf[Float] / filtered.size
    val max  = filtered.max
    println(s"mean: $mean, max: $max")
    val constMap = Math.max(max, 6000) // empirycznie sprawdzone

    val vertices = for {
      y <- Range(0, height)
      x <- Range(0, width)
      z = depthPixels((height-1-y)*width + (width-1-x))
      zPrim = if (z > 0) z else mean
    } yield new Vector3f(
      x.asInstanceOf[Float] / (width-1) * WIDTH - WIDTH/2,
      y.asInstanceOf[Float] / (height-1) * HEIGHT - HEIGHT/2,
//      Math.sin(x.asInstanceOf[Float] / (width-1) * Math.PI).asInstanceOf[Float] // walec
      (1- zPrim/constMap) * DEPTH
    )

//    for {
//      y <- Range(0, height)
//    } {
//      for {
//        x <- Range(0, width)
//      } {
//        printf("%4d ", depthPixels(y * width + x))
//      }
//      println()
//    }

    def vertex(x: Int, y: Int) = vertices(y*width + x)

    def vector(xFrom: Int, yFrom: Int, xTo: Int, yTo: Int) = {
      val to   = vertex(xTo, yTo)
      val from = vertex(xFrom, yFrom)
      to.subtract(from)
    }

    def adjacenciesVectors(x: Int, y: Int) = for {
      deltaXY <- Seq((0, 1), (-1, 0), (0, -1), (1, 0))
      adjX = x + deltaXY._1
      adjY = y + deltaXY._2
      if adjX >= 0 && adjX < width && adjY >= 0 && adjY < height
    } yield vector(x, y, adjX, adjY)

    def normal(x: Int, y: Int) = {
      val adjs = adjacenciesVectors(x, y)
      val normals = for {
        (v1, v2) <- adjs.take(adjs.length-1) zip adjs.drop(1)
      } yield v1.cross(v2)
      normals.reduce(_.addLocal(_)).divideLocal(normals.size) //.normalizeLocal()
    }

    val normals = for {
      y <- Range(0, height)
      x <- Range(0, width)
      n = normal(x, y)
      i <- Seq(n.x, n.y, n.z)
    } yield i

    val indexes = for {
      y <- Range(0, height-1)
      x <- Range(0, width-1)
      i <- Seq(width, 0, 1, 1, width+1, width)
    } yield y*width + x + i

    val textures = for {
      y <- Range(0, height)
      x <- Range(0, width)
    } yield new Vector2f(
      x.asInstanceOf[Float] / (width-1),
      y.asInstanceOf[Float] / (height-1)
    )

    mesh.setBuffer(Type.Position, 3, BufferUtils.createFloatBuffer(vertices : _*))
    mesh.setBuffer(Type.Normal,   3, BufferUtils.createFloatBuffer(normals: _*))
    mesh.setBuffer(Type.Index,    3, BufferUtils.createIntBuffer(indexes : _*))
    mesh.setBuffer(Type.TexCoord, 2, BufferUtils.createFloatBuffer(textures: _*))
    mesh.updateBound()

    mesh
  }


}