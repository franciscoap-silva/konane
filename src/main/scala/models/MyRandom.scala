package models

case class MyRandom(seed: Long) {
  def nextInt(n: Int): (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val value = ((newSeed >>> 16) % n).toInt
    (value, nextRandom)
  }
}
