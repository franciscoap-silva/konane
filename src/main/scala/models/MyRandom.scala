package models

// RandomWithState is a purely functional interface for random number generation.
// Instead of mutating hidden global state (like scala.util.Random does),
// every call to nextInt returns BOTH the generated number AND the next
// generator state. This makes randomness explicit, testable, and thread-safe.
trait RandomWithState {
  // Returns a pair: (random integer in [0, n), next generator state).
  // The caller is responsible for keeping and threading the new state forward.
  def nextInt(n: Int): (Int, RandomWithState)
}

// MyRandom is a Linear Congruential Generator (LCG) — one of the simplest
// and fastest pseudo-random algorithms.
//
// Formula:  newSeed = (seed * a + c) mod m
//   a = 0x5DEECE66D  (multiplier, same as Java's java.util.Random)
//   c = 0xB           (increment)
//   m = 2^48          (modulus, enforced by the 48-bit mask 0xFFFFFFFFFFFFL)
//
// The upper 32 bits of newSeed are used as the output (>>> 16 shifts out the
// lower 16 bits, which have poor statistical properties in LCGs).
//
// Being a case class makes MyRandom immutable and gives structural equality
// for free, which is useful in tests.
case class MyRandom(seed: Long) extends RandomWithState {
  def nextInt(n: Int): (Int, MyRandom) = {
    // Advance the LCG state by one step and keep only the lower 48 bits.
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)

    // Extract the result: shift right 16 bits, then take modulo n.
    // The absolute value guards against the rare case where the cast to Int
    // produces a negative number before the modulo.
    val nn = ((newSeed >>> 16).toInt) % n
    (if (nn < 0) -nn else nn, nextRandom)
  }
}
