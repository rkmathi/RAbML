/**
 * Util => Utilities object
 */
object Util {
  /** Arguments check
   * @param args arguments
   * @param len length num
   */
  def argsCheck(args: Array[String], len: Int): Unit = {
    if (args.length != len) {
      printf("Args error: Need %d argument(s)\n", len)
      sys.exit(1)
    }
  }
}

