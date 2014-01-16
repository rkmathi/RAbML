package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.lang.System
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer

object Q1_3_2 {
  def _main(args: Array[String]): Unit = {
    if (args.length != 2) {
      printf("Args error: Need 2 argument(s)\n")
      sys.exit(1)
    }

    val FPATH     = args(0).toString
    val TRY_TIMES = args(1).toInt
    val dataList = file2List(FPATH)
    val u2mTM = getU2M(dataList, TreeMap.empty[Int, TreeMap[Int, Int]])

    var totalTime = 0.toLong
    (1 to TRY_TIMES).foreach({ i =>
      //printf("UserID => %d\n", USER_ID)
      //printf("MovieID Rating\n")
      val rand = (Math.random*10).toInt+1
      val start = System.nanoTime().toLong
      for (data <- u2mTM(rand)) {
        //printf(" %6d %6d\n", data._1, data._2)
        val data1 = data._1
        val data2 = data._2
      }
      val tookTime = System.nanoTime().toLong - start
      totalTime += tookTime
    })
    printf("Time: %d[ns]\n", totalTime)

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

  def getU2M(
      dataL: List[Tuple3[Int, Int, Int]],
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, TreeMap[Int, Int]] = {
    val dataH = dataL.head
    if (dataL.tail.isEmpty) {
      return tm
    } else if (tm.contains(dataH._2)) {
      val _tm = tm(dataH._2).updated(dataH._1, dataH._3)
      getU2M(dataL.tail, tm.updated(dataH._2, _tm))
    } else {
      getU2M(dataL.tail, tm.updated(dataH._2, TreeMap(dataH._1 -> dataH._3)))
    }
  }
}

