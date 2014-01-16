package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader, PrintWriter}
import java.lang.System
import scala.collection.immutable.TreeMap
import scala.collection.mutable

object Q2_3 {
  def _main(args: Array[String]): Unit = {
    argsCheck(args, 3)
    val TRAIN_DAT = args(0)
    val TEST_DAT  = args(1)
    val SIMIL_DAT = args(2)

    val rij     = file2RijData(SIMIL_DAT)
    val trainU2M= getU2M(file2List(TRAIN_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])
    val trainM2U= getM2U(file2List(TRAIN_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])
    val simAvgTM= getSimAvgTM(trainU2M)

    val testU2M = getU2M(file2List(TEST_DAT), TreeMap.empty[Int, TreeMap[Int, Int]])

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

  def argsCheck(args: Array[String], len: Int): Unit = {
    if (args.length != len) {
      printf("Args error: Need %d argument(s)\n", len)
      sys.exit(1)
    }
  }

  def file2List(fpath: String): List[Tuple3[Int, Int, Int]] = {
    val br = new BufferedReader(
      new InputStreamReader(new FileInputStream(fpath), "UTF-8"))
    val dataArray = mutable.ListBuffer.empty[Tuple3[Int, Int, Int]]
    try {
      Iterator.continually(br.readLine()).takeWhile(_ != null).foreach { line =>
        val datas = line.split("::")
        dataArray += Tuple3(datas(0).toInt, datas(1).toInt, datas(2).toInt)
      }
    } finally {
      br.close()
    }
    return dataArray.toList
  }

  def file2RijData(fpath: String): mutable.HashMap[Tuple2[Int, Int], Double] = {
    val br = new BufferedReader(
      new InputStreamReader(new FileInputStream(fpath), "UTF-8"))
    val dataHM = mutable.HashMap.empty[Tuple2[Int, Int], Double]
    try {
      Iterator.continually(br.readLine()).takeWhile(_ != null).foreach { line =>
        // (1,1,0.1234) => 1, 1, 0.1234
        val datas = line.init.tail.split(",")
        dataHM(Tuple2(datas(0).trim.toInt, datas(1).trim.toInt)) = datas(2).toDouble
      }
    } finally {
      br.close()
    }
    return dataHM
  }

  def getU2M(
      dataL: List[Tuple3[Int, Int, Int]],
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, TreeMap[Int, Int]] = {
    val dataH = dataL.head
    if (dataL.tail.isEmpty) {
      val _tm = tm(dataH._1).updated(dataH._2, dataH._3)
      return tm.updated(dataH._1, _tm)
    } else if (tm.contains(dataH._1)) {
      val _tm = tm(dataH._1).updated(dataH._2, dataH._3)
      getU2M(dataL.tail, tm.updated(dataH._1, _tm))
    } else {
      getU2M(dataL.tail, tm.updated(dataH._1, TreeMap(dataH._2 -> dataH._3)))
    }
  }

  def getM2U(
      dataL: List[Tuple3[Int, Int, Int]],
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, TreeMap[Int, Int]] = {
    val dataH = dataL.head
    if (dataL.tail.isEmpty) {
      val _tm = tm(dataH._2).updated(dataH._1, dataH._3)
      return tm.updated(dataH._2, _tm)
    } else if (tm.contains(dataH._2)) {
      val _tm = tm(dataH._2).updated(dataH._1, dataH._3)
      getM2U(dataL.tail, tm.updated(dataH._2, _tm))
    } else {
      getM2U(dataL.tail, tm.updated(dataH._2, TreeMap(dataH._1 -> dataH._3)))
    }
  }

  def getSimAvgTM(
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, Double] = {
    val ary = mutable.ArrayBuffer.empty[Tuple2[Int, Double]]
    tm.foreach{ u2m =>
      var sum = 0.0d
      var count = 0.0d
      u2m._2.foreach{ m2r =>
        sum += m2r._2
        count += 1.0d
      }
      ary += Tuple2(u2m._1, sum/count)
    }

    val result = {
      val b = TreeMap.newBuilder[Int, Double]
      ary.foreach(b += _)
      b.result
    }
    return result
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


