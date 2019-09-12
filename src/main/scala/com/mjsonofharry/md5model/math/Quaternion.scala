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
  def from_matrix(matrix: List[Double]): List[Double] = {
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
    if (q.last < 0) q.map(_ * -1) else q
  }

  // from https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
  def from_euler(yaw: Double, pitch: Double, roll: Double): Quaternion = {
    val cy = math.cos(yaw * 0.5)
    val sy = math.sin(yaw * 0.5)
    val cp = math.cos(pitch * 0.5)
    val sp = math.sin(pitch * 0.5)
    val cr = math.cos(roll * 0.5)
    val sr = math.sin(roll * 0.5)
    Quaternion(
      w = cy * cp * cr + sy * sp * sr,
      x = cy * cp * sr - sy * sp * cr,
      y = sy * cp * sr + cy * sp * cr,
      z = sy * cp * cr - cy * sp * sr
    )
  }
}
