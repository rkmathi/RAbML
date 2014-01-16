package net.rkmathi.rabml

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.lang.System
import scala.collection.mutable.HashMap

object Q1_2_1 {
  def _main(args: Array[String]): Unit = {
    if (args.length != 2) {
      printf("Args error: Need 2 argument\n")
      sys.exit(1)
    }

    val filePath = args(0)
    val searchMovieId = args(1).toInt
    val br = new BufferedReader(
        new InputStreamReader(new FileInputStream(filePath), "UTF-8"))
    val dataHM = HashMap.empty[Int, String]

    val start = System.currentTimeMillis()
    try {
      Iterator.continually(br.readLine()).takeWhile(_ != null).foreach { line =>
        val datas = line.split("::")
        dataHM(datas(0).toInt) = datas(1).toString
      }
    } finally {
      br.close()
    }
    val end = System.currentTimeMillis()
    printf("Time: %d[ms]\n", end - start)

    if (dataHM.contains(searchMovieId))
      printf("MovieID %d => %s\n", searchMovieId, dataHM(searchMovieId))
    else
      printf("MovieID %d is bad MovieID\n", searchMovieId)
  }
}


