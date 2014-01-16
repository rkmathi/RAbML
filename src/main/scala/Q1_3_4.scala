package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.lang.System
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer

object Q1_3_4 {
  def _main(args: Array[String]): Unit = {
    if (args.length != 3) {
      printf("Args error: Need 2 argument(s)\n")
      sys.exit(1)
    }

    val FPATH     = args(0).toString
    val MOVIE_ID1  = args(1).toInt
    val MOVIE_ID2  = args(2).toInt
    val dataList = file2List(FPATH)
    val m2uTM = getM2U(dataList, TreeMap.empty[Int, TreeMap[Int, Int]])

    var totalTime = 0.toLong
    printf("Movie1 => %d, Movie2 => %d\n", MOVIE_ID1, MOVIE_ID2)
    printf("UserID (Movie1) (Movie2)\n")
    val rand1 = (Math.random*10).toInt+1
    val rand2 = (Math.random*10).toInt+1
    val start = System.nanoTime().toLong
    for (data <- m2uTM(rand1)) {
      if (m2uTM(rand2).contains(data._1)) {
        printf("%4d    %4d     %4d\n", data._1, data._2, m2uTM(MOVIE_ID2)(data._1))
        val data1 = data._1
        val data2 = data._2
      }
    }

    /*
    val start = System.nanoTime().toLong
    val tookTime = System.nanoTime().toLong - start
    printf("Time: %d[ms]\n", tookTime)
    */
  }

  def file2List(fpath: String): List[Tuple3[Int, Int, Int]] = {
    val br = new BufferedReader(
        new InputStreamReader(new FileInputStream(fpath), "UTF-8"))
    val dataArray = ArrayBuffer.empty[Tuple3[Int, Int, Int]]
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

  def getM2U(
      dataL: List[Tuple3[Int, Int, Int]],
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, TreeMap[Int, Int]] = {
    val dataH = dataL.head
    if (dataL.tail.isEmpty) {
      return tm
    } else if (tm.contains(dataH._1)) {
      val _tm = tm(dataH._1).updated(dataH._2, dataH._3)
      getM2U(dataL.tail, tm.updated(dataH._1, _tm))
    } else {
      getM2U(dataL.tail, tm.updated(dataH._1, TreeMap(dataH._2 -> dataH._3)))
    }
  }
}

