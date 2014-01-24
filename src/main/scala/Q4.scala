package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object Q4 {
  def _main(args: Array[String]): Unit = {
    Util.argsCheck(args, 4)
    val TRAIN_DAT = args(0)
    val TEST_DAT  = args(1)
    var lambda    = args(2).toDouble
    val K         = args(3).toInt

    val testU2M = UMR.getU2M(UMR.file2List(TEST_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])

    val sijMap  = UMR.file2Map(TRAIN_DAT)
    val sMatrix = Array.ofDim[Double](sijMap.maxBy(_._1._1)._1._1+1, sijMap.maxBy(_._1._2)._1._2+1)
    sijMap.foreach { r => sMatrix(r._1._1)(r._1._2) = r._2.toDouble }
    var uMatrix = getRandomMatrix(K, sMatrix.size)
    var vMatrix = getRandomMatrix(K, sMatrix(0).size)
    var t       = 0
    val ETA     = 0.01d

    val mseNoLambda   = getNoLambda(sijMap, uMatrix, vMatrix, testU2M, ETA, K)
    println("No   lambda => " + mseNoLambda)
    val mseWithLambda = getWithLambda(sijMap, uMatrix, vMatrix, testU2M, ETA, K, lambda)
    println("With lambda => " + mseWithLambda)
  }

  def getNoLambda(
      sijMap:  mutable.Map[Tuple2[Int, Int], Int],
      uMatrix: Array[Array[Double]],
      vMatrix: Array[Array[Double]],
      testU2M: TreeMap[Int, TreeMap[Int, Int]],
      ETA: Double,
      K: Int
      ): Double = {
    var mseOld      =  0.0d
    var mse         = 99.9d
    var testU2MSize = 0

    while (math.abs(mseOld - mse) >= 0.01) {
      mseOld = mse
      sijMap.foreach { s =>
        val (i, j) = s._1
        val sij = s._2
        for (k <- 0 to K-1) {
          val uT = uMatrix.transpose
          val vT = vMatrix.transpose
          uMatrix(k)(i) += ETA * (sij - getUtiVj(uT(i), vT(j))) * vMatrix(k)(j)
          vMatrix(k)(j) += ETA * (sij - getUtiVj(uT(i), vT(j))) * uMatrix(k)(i)
        }
      }

      var _mse = 0.0d
      testU2M.foreach { testU => 
        testU._2.foreach { testU2M2R =>
          testU2MSize += 1
          val testUserId  = testU._1
          val testMovieId = testU2M2R._1
          val testRating  = testU2M2R._2
          var sijSum = 0.0d
          for (k <- 0 to K-1) {
            _mse += math.pow(
                ((uMatrix(k)(testUserId-1)) * (vMatrix(k)(testMovieId-1)) -
                testU2M(testUserId)(testMovieId)).toDouble,
                2.0)
          }
        }
      }
      mse = math.sqrt(_mse/testU2MSize)
      //println(mse)
    }
    return mse
  }

  def getWithLambda(
      sijMap:  mutable.Map[Tuple2[Int, Int], Int],
      uMatrix: Array[Array[Double]],
      vMatrix: Array[Array[Double]],
      testU2M: TreeMap[Int, TreeMap[Int, Int]],
      ETA: Double,
      K: Int,
      lambda: Double
      ): Double = {
    var mseOld      =  0.0d
    var mse         = 99.9d
    var testU2MSize = 0

    while (math.abs(mseOld - mse) >= 0.01) {
      mseOld = mse
      sijMap.foreach { s =>
        val (i, j) = s._1
        val sij = s._2
        for (k <- 0 to K-1) {
          val uT = uMatrix.transpose
          val vT = vMatrix.transpose
          uMatrix(k)(i) += ETA * ((sij - getUtiVj(uT(i), vT(j))) * vMatrix(k)(j)-lambda*uMatrix(k)(i))
          vMatrix(k)(j) += ETA * ((sij - getUtiVj(uT(i), vT(j))) * uMatrix(k)(i)-lambda*vMatrix(k)(j))
        }
      }

      var _mse = 0.0d
      testU2M.foreach { testU => 
        testU._2.foreach { testU2M2R =>
          testU2MSize += 1
          val testUserId  = testU._1
          val testMovieId = testU2M2R._1
          val testRating  = testU2M2R._2
          var sijSum = 0.0d
          for (k <- 0 to K-1) {
            _mse += math.pow(
                ((uMatrix(k)(testUserId-1)) * (vMatrix(k)(testMovieId-1)) -
                testU2M(testUserId)(testMovieId)).toDouble,
                2.0)
          }
        }
      }
      mse = math.sqrt(_mse/testU2MSize)
      //println(mse)
    }
    return mse
  }

  def getRandomMatrix(i: Int, j: Int): Array[Array[Double]] = {
    val result = Array.ofDim[Double](i, j)
    for (i <- 0 to result.size-1; j <- 0 to result(0).size-1) {
        result(i)(j) = math.random
    }
    return result
  }

  def getUtiVj(uti: Array[Double], vj: Array[Double]): Double = {
    var result = 0.0d
    for (k <- 0 to uti.length-1) { result += (uti(k) * vj(k)) }
    return result
  }
}

