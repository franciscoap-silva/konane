package game_logic

import models.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

// ─── Type aliases ────────────────────────────────────────────────────────────
// Coord2D is a board position expressed as (row, column), both 0-indexed.
// Using a type alias keeps the signatures readable without defining a full class.
type Coord2D = (Int, Int)

// Board maps every occupied cell to the stone sitting on it.
// ParMap (parallel immutable map) is used so that operations like filter, map,
// and foldLeft can exploit multiple CPU cores when the board is large.
// Cells that are empty (open) are NOT stored in the map; they live in the
// separate lstOpenCoords list instead.
type Board = ParMap[Coord2D, Stone]

object GameLogic {

  // The four orthogonal directions a stone can jump in Konane:
  // right (0,+1), left (0,-1), down (+1,0), up (-1,0).
  // Konane does NOT allow diagonal moves.
  val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))

  // Returns the stone colour that belongs to the opposing player.
  // Exhaustive pattern matching on the Stone enum guarantees a compile error
  // if a new stone type is ever added without updating this function.
  def opponent(player: Stone): Stone = player match {
    case Stone.Black => Stone.White
    case Stone.White => Stone.Black
  }

  // ─── Board initialisation ─────────────────────────────────────────────────

  // Builds the list of (position, stone) pairs for the full board using
  // tail recursion so the call does not grow the JVM stack (Ficha 2).
  // The colouring rule: cells where (row + col) is even get Black,
  // odd cells get White — this guarantees the alternating checkerboard pattern.
  // 'acc' is the accumulator that collects results as the recursion unwinds.
  @tailrec
  private def generatePositions(rows: Int, cols: Int, r: Int, c: Int, acc: List[(Coord2D, Stone)]): List[(Coord2D, Stone)] = {
    if (r >= rows) acc                                        // all rows done
    else if (c >= cols) generatePositions(rows, cols, r + 1, 0, acc)  // next row
    else {
      val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
      generatePositions(rows, cols, r, c + 1, ((r, c), stone) :: acc)
    }
  }

  // Initialises a Konane board of size rows × cols.
  //
  // Konane starts with TWO adjacent stones removed from the centre so that
  // the first player has somewhere to jump. The standard opening removes one
  // Black and one White stone. Here we choose the cell just above-left of
  // centre (centerR, centerC) for Black and its right neighbour for White,
  // which always satisfies the alternating colour rule because:
  //   (centerR + centerC) is even  → Black
  //   (centerR + centerC + 1) is odd → White
  //
  // Returns (board, openCoords):
  //   board      – ParMap with all stones except the two removed ones
  //   openCoords – the two freed cells; this list grows throughout the game
  //                as captured stones and vacated origin cells become open
  def initBoard(rows: Int, cols: Int): (Board, List[Coord2D]) = {
    val positions = generatePositions(rows, cols, 0, 0, Nil)
    val fullMap   = positions.toMap

    val centerR = rows / 2 - 1
    val centerC = cols / 2 - 1
    val blackCoord = (centerR, centerC)
    val whiteCoord = (centerR, centerC + 1)

    // Remove the two centre stones and convert to a parallel map.
    val board = (fullMap - blackCoord - whiteCoord).par
    val openCoords: List[Coord2D] = List(blackCoord, whiteCoord)
    (board, openCoords)
  }

  // ─── T1: randomMove ───────────────────────────────────────────────────────

  // Picks a random element from lstOpenCoords using the pure LCG generator.
  // Returns both the chosen coordinate AND the new generator state so the
  // caller can keep threading the state forward (no hidden mutable state).
  // This function is passed as a Higher-Order Function (HOF) to playRandomly (T3).
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (index, nextRand) = rand.nextInt(lstOpenCoords.size)
    (lstOpenCoords(index), nextRand)
  }

  // ─── T2: play ─────────────────────────────────────────────────────────────

  // Attempts to move 'player's stone from coordFrom to coordTo.
  //
  // Konane move rules (all must hold for the move to be legal):
  //   1. The displacement must be exactly 2 cells in one orthogonal direction.
  //   2. coordFrom must contain 'player's stone.
  //   3. The cell halfway between coordFrom and coordTo (enemyCoord) must
  //      contain the opponent's stone — that is the stone being captured.
  //   4. coordTo must be an open (empty) cell, i.e. in lstOpenCoords.
  //
  // On success: returns (Some(newBoard), newOpenCoords) where
  //   - the player's stone has moved from coordFrom to coordTo
  //   - the captured enemy stone has been removed
  //   - coordFrom and enemyCoord have been added to the open list
  //   - coordTo has been removed from the open list (it is now occupied)
  //
  // On failure: returns (None, unchanged lstOpenCoords).
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D,
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {

    val (r1, c1) = coordFrom
    val (r2, c2) = coordTo
    val dr = r2 - r1
    val dc = c2 - c1

    // Rule 1: exactly 2 cells along one axis, 0 along the other.
    val isValidDirection = (dr == 0 && math.abs(dc) == 2) || (dc == 0 && math.abs(dr) == 2)

    // The cell between origin and destination — must hold an enemy stone.
    val enemyCoord: Coord2D = (r1 + dr / 2, c1 + dc / 2)

    val isValid = isValidDirection &&
      board.get(coordFrom).contains(player) &&          // rule 2
      board.get(enemyCoord).contains(opponent(player)) && // rule 3
      lstOpenCoords.contains(coordTo)                   // rule 4

    if (isValid) {
      // Remove the moving stone and the captured stone; place the moving stone
      // at the destination.
      val newBoard = board - coordFrom - enemyCoord + (coordTo -> player)
      // coordFrom (vacated) and enemyCoord (captured) become open.
      // coordTo (just occupied) is no longer open.
      val newOpen  = coordFrom :: enemyCoord :: lstOpenCoords.filterNot(_ == coordTo)
      (Some(newBoard), newOpen)
    } else {
      (None, lstOpenCoords)
    }
  }

  // ─── Move validation helpers ──────────────────────────────────────────────

  // Returns every legal single-jump move available to 'player' on the current board.
  // Uses flatMap (a Higher-Order Function / monadic list comprehension) to iterate
  // over all player stones and all open cells, keeping only the pairs where
  // play() succeeds (Ficha 3).
  // The result is sorted by (fromRow, fromCol, toRow, toCol) for a stable
  // display order in the UI.
  def validMoves(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {
    val playerStones = board.filter(_._2 == player).keys.toList

    playerStones.flatMap { from =>
        lstOpenCoords.flatMap { to =>
          val (optBoard, _) = play(board, player, from, to, lstOpenCoords)
          if (optBoard.isDefined) List((from, to)) else Nil
        }
      }
      .sortBy { case (from, to) => (from._1, from._2, to._1, to._2) }
  }

  // Returns only the legal continuation moves from a specific 'from' position.
  // Used during multi-jump turns: after a first jump lands on 'from', the player
  // may jump again with the same stone. This filters lstOpenCoords down to only
  // those destinations reachable from 'from' in one jump (Ficha 3).
  def validMovesFrom(board: Board, player: Stone, from: Coord2D,
                     lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] =
    lstOpenCoords.filter { to =>
        val (optBoard, _) = play(board, player, from, to, lstOpenCoords)
        optBoard.isDefined
      }.map(to => (from, to))

  // ─── T3: playRandomly ─────────────────────────────────────────────────────

  // Executes a single random jump for 'player'.
  //
  // 'f' is a Higher-Order Function (HOF) that selects one destination from the
  // list of valid destinations. Passing it as a parameter makes playRandomly
  // strategy-agnostic: in production, 'f' is randomMove; in tests it can be
  // any deterministic selector (Ficha 3 / T3 requirement).
  //
  // Returns (Option[Board], nextRand, newOpenCoords, Option[landedCoord]):
  //   - None board  → player has no moves (game over for this player)
  //   - Some board  → the move was executed; landedCoord is Some(to) so the
  //                   caller knows where the stone ended up (needed for multi-jump)
  def playRandomly(board: Board,
                   r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    val moves = validMoves(board, player, lstOpenCoords)

    if (moves.isEmpty) {
      // No legal moves: signal game-over for this player.
      (None, r, lstOpenCoords, None)
    } else {
      // Extract the set of reachable destinations and pick one with 'f'.
      val validTos = moves.map(_._2).distinct
      val (selectedTo, nextR) = f(validTos, r)

      // Find any move that ends at selectedTo (there might be several origins).
      val (from, to) = moves.find(_._2 == selectedTo).getOrElse(moves.head)

      // Execute the move; exhaustive match avoids a partial/refutable pattern (Ficha 5).
      play(board, player, from, to, lstOpenCoords) match {
        case (Some(newBoard), newOpen) => (Some(newBoard), nextR, newOpen, Some(to))
        case (None, _)                 => (None, nextR, lstOpenCoords, None)
      }
    }
  }

  // ─── T4: displayBoard ────────────────────────────────────────────────────

  // Prints the column-index header row (e.g. "  0 1 2 3 ...").
  // Implemented with tail recursion so it runs in constant stack space (Ficha 2).
  @tailrec
  private def printHeader(c: Int, cols: Int): Unit =
    if (c < cols) {
      print(s"$c ")
      printHeader(c + 1, cols)
    }

  // Prints one row of board cells, left to right.
  // Each cell is looked up in the ParMap and rendered as "B " (Black),
  // "W " (White), or ". " (empty/open). Tail recursive (Ficha 2).
  @tailrec
  private def printCols(board: Board, r: Int, cols: Int, c: Int): Unit =
    if (c < cols) {
      board.get((r, c)) match {
        case Some(Stone.Black) => print("B ")
        case Some(Stone.White) => print("W ")
        case None              => print(". ")
      }
      printCols(board, r, cols, c + 1)
    }

  // Prints all rows of the board, top to bottom, each prefixed by its row index.
  // Tail recursive (Ficha 2).
  @tailrec
  def printRows(board: Board, rows: Int, cols: Int, r: Int): Unit =
    if (r < rows) {
      print(s"$r ")
      printCols(board, r, cols, 0)
      println()
      printRows(board, rows, cols, r + 1)
    }

  // Entry point for board display (T4).
  // Prints the column header then delegates each row to printRows.
  // Example output for a 4×4 board:
  //   0 1 2 3
  // 0 B W B W
  // 1 W B W B
  // 2 B . . B    ← two centre stones removed
  // 3 W B W B
  def printBoard(board: Board, rows: Int, cols: Int): Unit = {
    print("  ")
    printHeader(0, cols)
    println()
    printRows(board, rows, cols, 0)
  }

  // Counts how many stones belonging to 'player' remain on the board.
  // Uses foldLeft (a fold / higher-order function) to accumulate the count
  // in a single left-to-right pass over the board map (Ficha 4).
  def countStones(board: Board, player: Stone): Int =
    board.foldLeft(0) { case (acc, (_, stone)) =>
      if (stone == player) acc + 1 else acc
    }
}
