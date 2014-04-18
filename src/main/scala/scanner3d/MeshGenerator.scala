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

  private var cachedXYOpt       = Option.empty[IndexedSeq[(Int, Int)]]
  private var cachedIndexesOpt  = Option.empty[IndexedSeq[Int]]
  private var cachedTexturesOpt = Option.empty[IndexedSeq[Vector2f]]

  def generate(depthPixelsIter: Iterable[Short], width: Int, height: Int): Mesh = {
    val mesh = new Mesh()

    val depthPixels = depthPixelsIter.toIndexedSeq
    val filtered = depthPixels.filter(_ > 0)
    val sum = filtered.foldLeft(0L) { _ + _ }
    val mean = sum.asInstanceOf[Float] / filtered.size
    val max  = 6000 // filtered.max // żeby się nie przesuwało - empirycznie sprawdzona wartość
    println(s"mean: $mean, max: $max")

    cachedXYOpt = Some(cachedXYOpt.getOrElse(for {
      y <- Range(0, height)
      x <- Range(0, width)
    } yield (x, y)))
    val cachedXY = cachedXYOpt.get

    val vertices = for {
      ((x, y), z) <- cachedXY zip depthPixels
      zPrim = Math.min(if (z > 0) z else mean, max)
    } yield new Vector3f(
      (1 - x.asInstanceOf[Float] / (width-1)) * WIDTH - WIDTH/2,
      (1 - y.asInstanceOf[Float] / (height-1)) * HEIGHT - HEIGHT/2,
//      Math.sin(x.asInstanceOf[Float] / (width-1) * Math.PI).asInstanceOf[Float] // walec
      (1 - zPrim/max) * DEPTH
    )

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
      (x, y) <- cachedXY
      n = normal(x, y)
      i <- Seq(n.x, n.y, n.z)
    } yield i

    cachedIndexesOpt = Some(cachedIndexesOpt.getOrElse(for {
      y <- Range(0, height-1)
      x <- Range(0, width-1)
      i <- Seq(width, 0, 1, 1, width+1, width)
    } yield y*width + x + i))

    cachedTexturesOpt = Some(cachedTexturesOpt.getOrElse(for {
      (x, y) <- cachedXY
    } yield new Vector2f(
      x.asInstanceOf[Float] / (width-1),
      y.asInstanceOf[Float] / (height-1)
    )))

    mesh.setBuffer(Type.Position, 3, BufferUtils.createFloatBuffer(vertices : _*))
    mesh.setBuffer(Type.Normal,   3, BufferUtils.createFloatBuffer(normals: _*))
    mesh.setBuffer(Type.Index,    3, BufferUtils.createIntBuffer(cachedIndexesOpt.get : _*))
    mesh.setBuffer(Type.TexCoord, 2, BufferUtils.createFloatBuffer(cachedTexturesOpt.get : _*))
    mesh.updateBound()

    mesh
  }


}