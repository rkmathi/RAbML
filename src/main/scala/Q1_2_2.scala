package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.lang.System
import scala.collection.mutable.LinkedHashMap

object Q1_2_2 {
  def _main(args: Array[String]): Unit = {
    if (args.length != 1) {
      printf("Args error: Need 1 argument\n")
      sys.exit(1)
    }

    val filePath = args(0)
    val br = new BufferedReader(
        new InputStreamReader(new FileInputStream(filePath), "UTF-8"))
    val dataHM = LinkedHashMap.empty[Int, String]

    val tryTime = 1000
    var totalTime = 0.toLong
    try {
      Iterator.continually(br.readLine()).takeWhile(_ != null).foreach { line =>
        val datas = line.split("::")
        dataHM(datas(0).toInt) = datas(1).toString
      }
      (1 to tryTime).foreach({ i =>
        val start = System.nanoTime().toLong

        val rand = (Math.random*100).toInt
        //printf("%d => %s\n", rand, dataHM.get(rand).toString)
        dataHM.get(rand).toString

        val end = System.nanoTime().toLong
        val currentTime = end - start
        totalTime += currentTime
        //printf("  Time: %d[ns]\n", currentTime)
      })
    } finally {
        br.close()
    }

    printf("Avg time => %d[ns]\n", totalTime / tryTime)
  }
}


