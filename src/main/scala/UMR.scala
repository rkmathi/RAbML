package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import scala.collection.immutable.TreeMap
import scala.collection.mutable

/**
 * UMR => UserID & MovieID & Rating object
 */
object UMR {
  /** Create Tuple3 (UserID, MovieID, Rating)
   * @param fpath file path
   * @return List[Tuple3(UserID, MovieID, Rating)]
   */
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

  /** Create HashMap(Tuple2(UserID, UserID) -> Similality)
   * @param fpath file path
   * @return HashMap(Tuple2(UserID, UserID) -> Similality)
   */
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

  /** Create TreeMap (UserID -> (MovieID -> Rating))
   * @param dataL List[Tuple3(UserID, MovieID, Rating)]
   * @param tm empty TreeMap
   * @return TreeMap(UserID -> TreeMap(MovieID -> Rating))
   */
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

  /** Create TreeMap (MovieID -> (UserID -> Rating))
   * @param dataL List[Tuple3(UserID, MovieID, Rating)]
   * @param tm empty TreeMap
   * @return TreeMap(MovieID -> TreeMap(UserID -> Rating))
   */
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

  /** Create TreeMap (UserID -> Similality)
   * @param tm empty TreeMap
   * @return TreeMap (UserID -> Similality)
   */
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
}

