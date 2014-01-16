package net.rkmathi.rabml

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      args.head match {
        case "Q1_1_1" => Q1_1_1._main(args.tail)

        case "Q1_2_1" => Q1_2_1._main(args.tail)
        case "Q1_2_2" => Q1_2_2._main(args.tail)

        case "Q1_3_1" => Q1_3_1._main(args.tail)
        case "Q1_3_2" => Q1_3_2._main(args.tail)
        case "Q1_3_3" => Q1_3_3._main(args.tail)
        case "Q1_3_4" => Q1_3_4._main(args.tail)

        case _        => throw new IllegalArgumentException("Invalid args(0)")
      }
    } else {
      throw new IllegalArgumentException("Main.scala Need at least 1 args")
    }
  }
}

