package models

// Stone represents a game piece on the Konane board.
// There are exactly two kinds of stone: Black (plays first) and White.
// Using a Scala 3 enum gives exhaustive pattern matching for free —
// the compiler will warn if any case is unhandled.
enum Stone:
  case Black, White
