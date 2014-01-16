package net.rkmathi.rabml

import scala.collection.mutable.HashMap
import scala.io.Source

object Q1_1_1 {
  def _main(args: Array[String]): Unit = {
    if (args.length != 1) {
      throw new IllegalArgumentException("Args error: Need 1 argument")
    }

    val filePath = args(0)
    val source = Source.fromFile(filePath)
    val lines = source.getLines
    val dataHM = HashMap.empty[Tuple2[Int, Int], Int]

    lines.foreach( { line =>
      val datas = line.split("::")
      dataHM(Tuple2(datas(0).toInt, datas(1).toInt)) =  datas(2).toInt
    })

    source.close

    printf(" x|1|2|3|4|5|6|7|8|9|10\n")
    Range(1, 11).foreach( { userId =>
      printf("%2d|", userId)
      Range(1, 11).foreach( { movieId =>
        if (dataHM.contains(Tuple2(userId, movieId))) {
          printf("%d ", dataHM(Tuple2(userId, movieId)))
        } else {
          printf("- ")
        }
      })
      printf("\n")
    })
  }
}
