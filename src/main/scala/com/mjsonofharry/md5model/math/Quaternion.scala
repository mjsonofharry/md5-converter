package com.mjsonofharry.md5model.math

import breeze.linalg._

/*
  The MIT License

  Copyright (c) 2009-2019 Matthew Brett <matthew.brett@gmail.com>
  Copyright (c) 2010-2013 Stephan Gerhard <git@unidesign.ch>
  Copyright (c) 2006-2014 Michael Hanke <michael.hanke@gmail.com>
  Copyright (c) 2011 Christian Haselgrove <christian.haselgrove@umassmed.edu>
  Copyright (c) 2010-2011 Jarrod Millman <jarrod.millman@gmail.com>
  Copyright (c) 2011-2019 Yaroslav Halchenko <debian@onerussian.com>
  Copyright (c) 2015-2019 Chris Markiewicz <effigies@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
 */

case class Quaternion(w: Double, x: Double, y: Double, z: Double)

object Quaternion {
  def from_matrix(matrix: List[Double]): Quaternion = {
    val List(xx, yx, zx, xy, yy, zy, xz, yz, zz): List[Double] = matrix

    val k: DenseMatrix[Double] = DenseMatrix(
      (xx - yy - zz, 0.0, 0.0, 0.0),
      (yx + xy, yy - xx - zz, 0.0, 0.0),
      (zx + xz, zy + yz, zz - xx - yy, 0.0),
      (yz - zy, zx - xz, xy - yx, xx + yy + zz)
    ) / 3.0

    val e = eigSym2(k)
    val q: List[Double] =
      e.eigenvectors(::, argmax(e.eigenvalues)).toArray.toList
    val List(x, y, z, w) = if (q.last < 0) q.map(_ * -1) else q
    Quaternion(w, x, y, z)
  }

  // from https://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm
  def from_euler2(yaw: Double, pitch: Double, roll: Double): Quaternion = {
    val r = Math.PI / 180
    val heading = yaw * r
    val attitude = pitch * r
    val bank = roll * r
    val c1 = Math.cos(heading / 2);
    val s1 = Math.sin(heading / 2);
    val c2 = Math.cos(attitude / 2);
    val s2 = Math.sin(attitude / 2);
    val c3 = Math.cos(bank / 2);
    val s3 = Math.sin(bank / 2);
    val c1c2 = c1 * c2;
    val s1s2 = s1 * s2;
    val w = (c1c2 * c3) - (s1s2 * s3);
  	val x = (c1c2 * s3) + (s1s2 * c3);
    val y = (s1 * c2 * c3) + (c1 * s2 * s3);
    val z = (c1 * s2 * c3) - (s1 * c2 * s3);
    Quaternion(w, x, y, z)
  }

  // from https://www.euclideanspace.com/maths/geometry/rotations/conversions/eulerToQuaternion/index.htm
  def from_euler3(yaw: Double, pitch: Double, roll: Double): Quaternion = {
    val r = Math.PI / 180
    val heading = yaw * r
    val attitude = pitch * r
    val bank = roll * r
    val c1 = Math.cos(heading)
    val s1 = Math.sin(heading)
    val c2 = Math.cos(attitude)
    val s2 = Math.sin(attitude)
    val c3 = Math.cos(bank)
    val s3 = Math.sin(bank)
    val w = Math.sqrt(1.0 + (c1 * c2) + (c1 * c3) - (s1 * s2 * s3) + (c2 * c3)) / 2.0
    val w4 = (4.0 * w)
    val x = (c2 * s3 + c1 * s3 + s1 * s2 * c3) / w4
    val y = (s1 * c2 + s1 * c3 + c1 * s2 * s3) / w4
    val z = (-s1 * s3 + c1 * s2 * c3 +s2) / w4
    Quaternion(w, x, y, z)
  }

  def normalized(q: Quaternion): Quaternion = {
    val d = norm(q)
    Quaternion(q.w / d, q.x / d, q.y / d, q.z / d)
  }

  def norm(q: Quaternion): Double =
    math.sqrt((q.x * q.x) + (q.y * q.y) + (q.z * q.z) + (q.w * q.w))
}
