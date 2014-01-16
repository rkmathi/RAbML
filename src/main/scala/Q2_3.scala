package net.rkmathi.rabml

import java.io.{BufferedReader,FileInputStream,InputStreamReader,PrintWriter}
import java.lang.System
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object Q2_3 {
  def _main(args: Array[String]): Unit = {
    Util.argsCheck(args, 3)
    val TRAIN_DAT = args(0)
    val TEST_DAT  = args(1)
    val SIMIL_DAT = args(2)

    val rij     = UMR.file2RijData(SIMIL_DAT)
    val trainU2M= UMR.getU2M(UMR.file2List(TRAIN_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])
    val trainM2U= UMR.getM2U(UMR.file2List(TRAIN_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])
    val simAvgTM= UMR.getSimAvgTM(trainU2M)

    val testU2M = UMR.getU2M(UMR.file2List(TEST_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])

    var mse = 0.0d
    var testU2MSize = 0
    testU2M.foreach { testU => 
      testU._2.foreach { testU2M2R =>
        testU2MSize += 1
        val testUserId  = testU._1
        val testMovieId = testU2M2R._1
        val testRating  = testU2M2R._2
        if (getSik(rij, simAvgTM, trainU2M, trainM2U, testUserId, testMovieId) != 0.0d) {
          mse += math.pow((testRating - getSik(rij, simAvgTM, trainU2M, trainM2U, testUserId, testMovieId)), 2.0)
        }
      }
    }
    println(mse/testU2MSize)
  }

  def getSik(
      rij: mutable.HashMap[Tuple2[Int, Int], Double],
      simAvgTM: TreeMap[Int, Double],
      u2mTM: TreeMap[Int, TreeMap[Int, Int]],
      m2uTM: TreeMap[Int, TreeMap[Int, Int]],
      userID: Int,
      movieID: Int): Double = {
    val siH = simAvgTM(userID)

    val _userIDs = mutable.ListBuffer.empty[Int]
    //println(userID, movieID, m2uTM.get(movieID))
    m2uTM.get(movieID) match {
      case Some(m) => m.foreach { kv => _userIDs += kv._1 }
      case _       => //println("  error")
    }
    if (_userIDs.isEmpty) { return 0.0 }
    val userIDs = _userIDs.toList

    var numer = 0.0d
    var denom = 0.0d
    userIDs.foreach { _userID =>
      if (_userID != userID && math.abs(rij(Tuple2(userID, _userID))) != 0.0d) {
        numer += rij(Tuple2(userID, _userID)) * (u2mTM(_userID)(movieID) - simAvgTM(_userID))
        denom += math.abs(rij(Tuple2(userID, _userID)))
      }
    }

    return siH + (numer / denom)
  }
}

