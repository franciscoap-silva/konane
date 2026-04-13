package models

trait RandomWithState {
  def nextInt(n: Int): (Int, RandomWithState)
}

case class MyRandom(seed: Long) extends RandomWithState {
  def nextInt(n: Int): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val nn = ((newSeed >>> 16).toInt) % n
    (if (nn < 0) -nn else nn, nextRandom)
  }
}
