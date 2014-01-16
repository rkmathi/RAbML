package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import scala.collection.mutable

object Q4 {
  def _main(args: Array[String]): Unit = {
    argsCheck(args, 2)
    val TRAIN_DAT = args(0)
    val K = args(1).toInt

    val ETA     = 0.01
    val sijMap  = file2Map(TRAIN_DAT)
    val sMatrix = Array.ofDim[Double](sijMap.maxBy(_._1._1)._1._1+1, sijMap.maxBy(_._1._2)._1._2+1)
    sijMap.foreach { r => sMatrix(r._1._1)(r._1._2) = r._2.toDouble }
    val uMatrix = getRandomMatrix(K, sMatrix.size)
    val vMatrix = getRandomMatrix(K, sMatrix(0).size)
    var t = 0

//    step2(sijMap, K, uMatrix, vMatrix)

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

    println("---Debug start---")
    println("uMatrix")
    for (i <- 0 to uMatrix.size-1) {
      for(j <- 0 to uMatrix(i).size-1) {
        printf("%f ", uMatrix(i)(j))
      }
      printf("\n")
    }
    println("vMatrix")
    for (i <- 0 to vMatrix.size-1) {
      for(j <- 0 to vMatrix(i).size-1) {
        printf("%f ", vMatrix(i)(j))
      }
      printf("\n")
    }
    println("---Debug end---")
  }

  def argsCheck(args: Array[String], len: Int): Unit = {
    if (args.length != len) {
      printf("Args error: Need %d argument(s)\n", len)
      sys.exit(1)
    }
  }

  def file2Map(fpath: String): mutable.Map[Tuple2[Int, Int], Int] = {
    val br = new BufferedReader(
        new InputStreamReader(new FileInputStream(fpath), "UTF-8"))
    val result = mutable.Map.empty[Tuple2[Int, Int], Int]
    try {
      Iterator.continually(br.readLine()).takeWhile(_ != null).foreach { line =>
        val datas = line.split("::")
        result(Tuple2(datas(0).toInt-1, datas(1).toInt-1)) = datas(2).toInt
      }
    } finally {
      br.close()
    }
    return result
  }

  def getRandomMatrix(i: Int, j: Int): Array[Array[Double]] = {
    val result = Array.ofDim[Double](i, j)
    for (i <- 0 to result.size-1; j <- 0 to result(0).size-1) {
        result(i)(j) = math.random
    }
    return result
  }

  def getUtiVj(uti: Array[Double], vj: Array[Double]): Int = {
    return 1
  }
}

