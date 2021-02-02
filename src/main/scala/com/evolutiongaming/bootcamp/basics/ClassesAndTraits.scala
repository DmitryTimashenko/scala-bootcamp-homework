package com.evolutiongaming.bootcamp.basics

class ClassesAndTraits {

  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
    def area: Double
  }

  sealed trait Located2D {
    def x: Double

    def y: Double
  }

  sealed trait Bounded2D {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)

    override def area: Double = 0
  }

  final case class Triangle2d(firstVertex: Point2D, secondVertex: Point2D, thirdVertex: Point2D)
    extends Shape2D {
    override def x: Double = (firstVertex.x + secondVertex.x + thirdVertex.x) / 3

    override def y: Double = (firstVertex.y + secondVertex.y + thirdVertex.y) / 3

    override def minX: Double = firstVertex.x min secondVertex.x min thirdVertex.x

    override def maxX: Double = firstVertex.x max secondVertex.x max thirdVertex.x

    override def minY: Double = firstVertex.y min secondVertex.y min thirdVertex.y

    override def maxY: Double = firstVertex.y max secondVertex.y max thirdVertex.y

    override def move(dx: Double, dy: Double): Triangle2d =
      Triangle2d(firstVertex.move(dx, dy), secondVertex.move(dx, dy), thirdVertex.move(dx, dy))

    override def area: Double = Math.abs(
      (secondVertex.x - firstVertex.x) * (thirdVertex.y - firstVertex.y) -
        (thirdVertex.x - firstVertex.x) * (secondVertex.y - firstVertex.y)
    ) / 2
  }

  final case class Square2d(center: Point2D, weight: Double)
    extends Shape2D {
    override def x: Double = center.x

    override def y: Double = center.y

    override def minX: Double = center.x - (weight / 2)

    override def maxX: Double = center.x + (weight / 2)

    override def minY: Double = center.y - (weight / 2)

    override def maxY: Double = center.y + (weight / 2)

    override def move(dx: Double, dy: Double): Square2d =
      Square2d(center.move(dx, dy), weight)

    override def area: Double = weight * weight
  }


  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def volume: Double

    def surfaceArea: Double
  }

  sealed trait Located3D {
    def x: Double

    def y: Double

    def z: Double
  }

  sealed trait Bounded3D {
    def minX: Double

    def minY: Double

    def minZ: Double

    def maxX: Double

    def maxY: Double

    def maxZ: Double
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D {
    override def minX: Double = x

    override def minY: Double = y

    override def minZ: Double = z

    override def maxX: Double = x

    override def maxY: Double = y

    override def maxZ: Double = z

    override def volume: Double = 0

    override def surfaceArea: Double = 0

    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
  }

  final case class Sphere(center: Point3D, radius: Double) extends Shape3D {
    override def x: Double = center.x

    override def y: Double = center.y

    override def z: Double = center.z

    override def minX: Double = center.x - radius

    override def minY: Double = center.y - radius

    override def minZ: Double = center.z - radius

    override def maxX: Double = center.x + radius

    override def maxY: Double = center.y + radius

    override def maxZ: Double = center.z + radius

    override def volume: Double = 4 * Math.PI * radius * radius * radius / 3

    override def surfaceArea: Double = 4 * Math.PI * radius * radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(Point3D(x + dx, y + dy, z + dz), radius
      )
  }

  final case class Cube(center: Point3D, weight: Double) extends Shape3D {
    override def x: Double = center.x

    override def y: Double = center.y

    override def z: Double = center.z

    override def minX: Double = center.x - weight

    override def minY: Double = center.y - weight

    override def minZ: Double = center.z - weight

    override def maxX: Double = center.x + weight

    override def maxY: Double = center.y + weight

    override def maxZ: Double = center.z + weight

    override def volume: Double = weight * weight * weight

    override def surfaceArea: Double = 6 * weight * weight

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(Point3D(x + dx, y + dy, z + dz), weight
      )

  }

  final case class Triangle3d(firstVertex: Point3D, secondVertex: Point3D, thirdVertex: Point3D)
    extends Shape3D {
    override def x: Double = (firstVertex.x + secondVertex.x + thirdVertex.x) / 3

    override def y: Double = (firstVertex.y + secondVertex.y + thirdVertex.y) / 3

    override def z: Double = (firstVertex.z + secondVertex.z + thirdVertex.z) / 3

    override def minX: Double = firstVertex.x min secondVertex.x min thirdVertex.x

    override def minY: Double = firstVertex.y min secondVertex.y min thirdVertex.y

    override def minZ: Double = firstVertex.z min secondVertex.z min thirdVertex.z

    override def maxX: Double = firstVertex.x max secondVertex.x max thirdVertex.x

    override def maxY: Double = firstVertex.y max secondVertex.y max thirdVertex.y

    override def maxZ: Double = firstVertex.z max secondVertex.z max thirdVertex.z

    override def volume: Double = 0

    override def surfaceArea: Double = Math.sqrt(
      Math.pow(((secondVertex.y - firstVertex.y) * (thirdVertex.z - firstVertex.z) - (secondVertex.z - firstVertex.z) * (thirdVertex.y - firstVertex.y)), 2) +
        Math.pow(((secondVertex.z - firstVertex.z) * (thirdVertex.x- firstVertex.x) - (secondVertex.x - firstVertex.x) * (thirdVertex.z - firstVertex.z)), 2) +
        Math.pow(((secondVertex.x - firstVertex.x) * (thirdVertex.y - firstVertex.y) - (secondVertex.y - firstVertex.y) * (thirdVertex.x - firstVertex.x)), 2)
    ) / 2

    override def move(dx: Double, dy: Double, dz: Double): Triangle3d =
      Triangle3d(firstVertex.move(dx, dy, dz), secondVertex.move(dx, dy, dz), thirdVertex.move(dx, dy, dz))
  }

}
