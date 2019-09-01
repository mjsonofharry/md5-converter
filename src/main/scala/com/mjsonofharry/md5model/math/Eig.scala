package com.mjsonofharry.md5model.math

import breeze.linalg._
import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}

/*
  Copyright 2012 David Hall

  Licensed under the Apache License, Version 2.0 (the "License")
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 */

object eigSym2 extends UFunc {
  case class EigSym[V, M](eigenvalues: V, eigenvectors: M)
  type DenseEigSym = EigSym[DenseVector[Double], DenseMatrix[Double]]
  implicit object EigSym_DM_Impl
      extends Impl[DenseMatrix[Double], DenseEigSym] {
    def apply(X: DenseMatrix[Double]): DenseEigSym = {
      doEigSym(X, true) match {
        case (ev, Some(rev)) => EigSym(ev, rev)
        case _               => throw new RuntimeException("Shouldn't be here!")
      }

    }
  }

  private def doEigSym(
      X: Matrix[Double],
      rightEigenvectors: Boolean
  ): (DenseVector[Double], Option[DenseMatrix[Double]]) = {
    if (X.rows != X.cols)
      throw new MatrixNotSquareException
    if (X.cols == 0 || X.rows == 0)
      throw new MatrixEmptyException

    val A = lowerTriangular(X)

    val N = X.rows
    val evs = DenseVector.zeros[Double](N)
    val lwork = scala.math.max(1, 3 * N - 1)
    val work = Array.ofDim[Double](lwork)
    val info = new intW(0)
    lapack.dsyev(
      if (rightEigenvectors) "V" else "N",
      "L",
      N,
      A.data,
      scala.math.max(1, N),
      evs.data,
      work,
      lwork,
      info
    )
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    (evs, if (rightEigenvectors) Some(A) else None)
  }
}
