package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader, PrintWriter}
import java.lang.System
import scala.collection.immutable.TreeMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Q2_1 {
  def _main(args: Array[String]): Unit = {
    argsCheck(args, 2)

    val INPATH  = args(0)
    val OUTPATH = args(1)

    val u2mTM   = getU2M(file2List(INPATH), TreeMap.empty[Int, TreeMap[Int, Int]])
    val avgTM  = getSimilarityAvgTM(u2mTM)

    val start = System.currentTimeMillis()
    val simAry = getSimilarityTM(u2mTM, avgTM)
    val end = System.currentTimeMillis()
    println(end-start)

//    /*
    val out = new PrintWriter(OUTPATH, "utf8")
    simAry.zipWithIndex.foreach { case (x, i) =>
      x.zipWithIndex.foreach { case (y, j) =>
        out.println(i+1, j+1, y)
      }
    }
    out.close
//    */
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
    val dataArray = ListBuffer.empty[Tuple3[Int, Int, Int]]
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
      val _tm = tm(dataH._1).updated(dataH._2, dataH._3)
      return tm.updated(dataH._1, _tm)
    } else if (tm.contains(dataH._1)) {
      val _tm = tm(dataH._1).updated(dataH._2, dataH._3)
      getU2M(dataL.tail, tm.updated(dataH._1, _tm))
    } else {
      getU2M(dataL.tail, tm.updated(dataH._1, TreeMap(dataH._2 -> dataH._3)))
    }
  }

  def getSimilarityAvgTM(
      tm: TreeMap[Int, TreeMap[Int, Int]]): TreeMap[Int, Double] = {
    val ary = ArrayBuffer.empty[Tuple2[Int, Double]]
    tm.foreach { u2m =>
      var sum = 0.0d
      var count = 0.0d
      u2m._2.foreach { m2r =>
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

  def getSimilarityTM(
      u2mTM: TreeMap[Int, TreeMap[Int, Int]],
      avgTM: TreeMap[Int, Double]): Array[Array[Double]] = {
    val result = Array.ofDim[Double](u2mTM.size, u2mTM.size)
    u2mTM.foreach { u1 =>
      u2mTM.foreach { u2 =>
        var numer = 0.0d
        var denom1 = 0.0d
        var denom2 = 0.0d
        for (data1 <- u1._2) {
          for (data2 <- u2._2 if data1._1 == data2._1) {
            val sik = data1._2
            val sjk = data2._2
            numer  += (sik - avgTM(u1._1)) * (sjk - avgTM(u2._1))
            denom1 += (sik - avgTM(u1._1)) * (sik - avgTM(u1._1))
            denom2 += (sjk - avgTM(u2._1)) * (sjk - avgTM(u2._1))
          }
        }
        if (numer == 0.0d || denom1 == 0.0d || denom2 == 0.0d || checkSameKV(u1._2, u2._2) <= 1) {
          result(u1._1-1)(u2._1-1) = 0.0d
        }
        else {
          result(u1._1-1)(u2._1-1) = numer / (math.sqrt(denom1) * math.sqrt(denom2))
        }
      }
    }
    return result
  }

  def checkSameKV(tm1: TreeMap[Int, Int], tm2: TreeMap[Int, Int]): Int = {
    var result = 0
    tm1.foreach { t1 =>
      if (tm2.contains(t1._1)) result += 1
    }
    return result
  }
}

