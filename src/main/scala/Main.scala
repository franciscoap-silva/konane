import game_logic.*
import game_logic.GameLogic.*
import models.*

import scala.annotation.tailrec
import scala.io.StdIn.readLine

// Main is the application entry point and owns all user-facing I/O.
// It is kept separate from GameLogic so that pure game rules (GameLogic)
// never depend on console I/O — a clean separation of concerns.
object Main {

  // Program entry point.
  // Flow:
  //   1. Ask the player for board size.
  //   2. Build and display the initial board.
  //   3. Ask the player which stone colour they want.
  //   4. Seed the random generator from the system clock.
  //   5. Hand off to the main game loop (Black always moves first in Konane).
  def main(args: Array[String]): Unit = {
    println("=== Jogo Konane ===")
    println()

    val (rows, cols) = chooseSize()
    val (board, openCoords) = initBoard(rows, cols)

    println()
    println(s"Tabuleiro ${rows}x${cols} inicializado:")
    println()
    printBoard(board, rows, cols)
    println()

    val playerStone   = chooseStone()
    val computerStone = opponent(playerStone)
    println(s"Jogador: ${stoneName(playerStone)} | Computador: ${stoneName(computerStone)}")
    println("Black começa a jogar.")
    println()

    // Seed MyRandom with the current wall-clock time so each run is different.
    val rand = MyRandom(System.currentTimeMillis())

    // Black is always the first player in Konane, regardless of what colour
    // the human chose.
    gameLoop(board, openCoords, Stone.Black, playerStone, rand, rows, cols)
  }

  // Reads and validates the desired board size from the console.
  // Accepts integers in [4, 10]; rejects everything else and retries.
  // @tailrec ensures the compiler turns the retry into a loop, not recursion,
  // so invalid input cannot overflow the stack (Ficha 2).
  @tailrec
  def chooseSize(): (Int, Int) = {
    print("Board Size (4-10): ")
    val input = readLine().trim
    input.toIntOption match {
      case Some(size) if size >= 4 && size <= 10 =>
        (size, size)   // square board: same value for rows and columns
      case _ =>
        println("Tamanho inválido. Introduza um número inteiro entre 4 e 10.")
        chooseSize()
    }
  }

  // Main game loop — runs until a player has no legal moves.
  //
  // State threaded through each recursive call (functional style, no vars):
  //   board          – current ParMap of stones
  //   lstOpenCoords  – list of currently empty cells
  //   currentPlayer  – whose turn it is (alternates each call)
  //   playerStone    – the human's colour (constant throughout the game)
  //   rand           – current random generator state (advances on each computer move)
  //   rows / cols    – board dimensions (constant)
  //
  // The loop terminates naturally when doPlayerTurn or computerTurn find no
  // legal moves, at which point the functions return the unchanged state and
  // we recurse no further (the game-over check is implicit: if validMoves is
  // empty, the turn functions print a message and the loop stops).
  //
  // @tailrec is safe here because every branch ends with a direct recursive call
  // or with nothing (game over inside the turn helpers).
  @tailrec
  def gameLoop(board: Board, lstOpenCoords: List[Coord2D], currentPlayer: Stone,
               playerStone: Stone, rand: MyRandom, rows: Int, cols: Int): Unit = {

    printBoard(board, rows, cols)
    println()
    println(s"Black: ${countStones(board, Stone.Black)} pedras | White: ${countStones(board, Stone.White)} pedras")
    println()

    if (currentPlayer == playerStone) {
      // ── Human turn ──────────────────────────────────────────────────────
      println(s"--- Sua vez (${stoneName(playerStone)}) ---")
      println()
      // None as last argument means "first jump — player may pick any stone".
      val (newBoard, newOpen) = doplayerTurn(board, lstOpenCoords, playerStone, rows, cols, None)
      gameLoop(newBoard, newOpen, opponent(playerStone), playerStone, rand, rows, cols)

    } else {
      // ── Computer turn ────────────────────────────────────────────────────
      println(s"--- Vez do Computador (${stoneName(currentPlayer)}) ---")
      println()
      // None as last argument means "first jump of this turn".
      val (newBoard, newOpen, nextRand) = computerTurn(board, lstOpenCoords, currentPlayer, rand, rows, cols, None)
      gameLoop(newBoard, newOpen, opponent(currentPlayer), playerStone, nextRand, rows, cols)
    }
  }

  // Executes the computer's turn, which may consist of multiple jumps with
  // the same stone (multi-jump / chain capture).
  //
  // 'fromOpt' encodes which phase we are in:
  //   None       – first jump: pick any stone using playRandomly (T3, HOF)
  //   Some(pos)  – continuation jump: must move the stone that is now at 'pos'
  //
  // The function calls itself recursively until no more jumps are available
  // from the current position. @tailrec keeps stack depth constant (Ficha 2).
  @tailrec
  def computerTurn(board: Board, openCoords: List[Coord2D], computerStone: Stone, rand: MyRandom, rows: Int, cols: Int,
                   fromOpt: Option[Coord2D]): (Board, List[Coord2D], MyRandom) = {

    fromOpt match {

      // ── First jump: delegate to playRandomly with randomMove as the HOF (T3) ──
      case None =>
        val (optBoard, nextRand, newOpen, optTo) = playRandomly(board, rand, computerStone, openCoords, randomMove)
        optBoard match {
          case None =>
            // No legal moves at all: computer's turn ends, board unchanged.
            (board, openCoords, rand)

          case Some(newBoard) =>
            val to = optTo.get

            // Recover 'from': after the jump, the origin and the captured cell
            // were both added to newOpen. The origin is the open cell that is
            // exactly 2 steps away from 'to' (the only newly freed cell at
            // distance 2; the captured cell is at distance 1).
            val newlyFreed = newOpen.filterNot(openCoords.contains)
            val from = newlyFreed.find { c =>
              val dr = math.abs(c._1 - to._1)
              val dc = math.abs(c._2 - to._2)
              (dr == 2 && dc == 0) || (dr == 0 && dc == 2)
            }.getOrElse(to)

            println(s"Computador moveu: $from -> $to")
            println()

            // Check whether a continuation jump is possible from 'to'.
            val contMoves = validMovesFrom(newBoard, computerStone, to, newOpen)
            if (contMoves.isEmpty) (newBoard, newOpen, nextRand)
            else computerTurn(newBoard, newOpen, computerStone, nextRand, rows, cols, Some(to))
        }

      // ── Continuation jump: use randomMove directly to pick the next destination ──
      case Some(fromPos) =>
        val contMoves = validMovesFrom(board, computerStone, fromPos, openCoords)
        if (contMoves.isEmpty) {
          // No further jumps from this position: turn is over.
          (board, openCoords, rand)
        } else {
          // Pick a random destination among the valid ones.
          val validTos = contMoves.map(_._2).distinct
          val (selectedTo, nextRand) = randomMove(validTos, rand)
          val (from, to) = contMoves.find(_._2 == selectedTo).getOrElse(contMoves.head)

          play(board, computerStone, from, to, openCoords) match {
            case (Some(newBoard), newOpen) =>
              println(s"Computador moveu: $from -> $to")
              println()
              val nextContMoves = validMovesFrom(newBoard, computerStone, to, newOpen)
              if (nextContMoves.isEmpty) (newBoard, newOpen, nextRand)
              else computerTurn(newBoard, newOpen, computerStone, nextRand, rows, cols, Some(to))

            case (None, _) =>
              // Should never happen because the move came from validMovesFrom.
              (board, openCoords, rand)
          }
        }
    }
  }

  // Executes the human player's turn with multi-jump support.
  //
  // 'fromOpt' encodes the phase (same convention as computerTurn):
  //   None       – first jump: player may choose any of their stones
  //   Some(pos)  – continuation: player may only move the stone at 'pos'
  //
  // After each jump the function asks whether the player wants to continue
  // capturing with the same stone. If yes it recurses (Ficha 2); if no (or
  // no further captures are available) it returns the updated state.
  def doplayerTurn(board: Board, lstOpenCoords: List[Coord2D], player: Stone,
                   rows: Int, cols: Int, fromOpt: Option[Coord2D]): (Board, List[Coord2D]) = {

    // Gather legal moves depending on the phase (Ficha 5 — Option pattern match).
    val moves = fromOpt match {
      case None       => validMoves(board, player, lstOpenCoords)       // any stone
      case Some(from) => validMovesFrom(board, player, from, lstOpenCoords) // fixed stone
    }

    if (moves.isEmpty) {
      // No moves available: end this player's turn without changing state.
      (board, lstOpenCoords)
    } else {
      fromOpt match {
        case None    => println("Jogadas validas:")
        case Some(_) => println("Continuacao - escolha a proxima captura:")
      }
      showValidMoves(moves, 1)
      val (from, to) = chooseMove(moves)

      // Execute the chosen move; exhaustive match on Option (Ficha 5).
      play(board, player, from, to, lstOpenCoords) match {
        case (Some(newBoard), newOpen) =>
          println()
          val contMoves = validMovesFrom(newBoard, player, to, newOpen)
          if (contMoves.isEmpty) {
            (newBoard, newOpen)   // no chain possible — turn ends
          } else {
            println("Pode continuar a capturar com a mesma pedra! Deseja continuar? (S/N)")
            val ans = readLine().trim.toUpperCase
            // Recurse for the continuation jump if the player agrees (Ficha 2).
            if (ans == "S") doplayerTurn(newBoard, newOpen, player, rows, cols, Some(to))
            else (newBoard, newOpen)
          }

        case (None, _) =>
          // move came from validMoves, so (None, _) should not occur in practice.
          println("Jogada invalida. Tente novamente.")
          doplayerTurn(board, lstOpenCoords, player, rows, cols, fromOpt)
      }
    }
  }

  // Reads the player's colour choice from the console and retries on bad input.
  // @tailrec + pattern matching on the input string (Fichas 2, 6).
  @tailrec
  def chooseStone(): Stone = {
    println("Escolha a sua pedra (B = Black / W = White):")
    readLine().trim.toUpperCase match {
      case "B" =>
        println("Escolheu Black!")
        Stone.Black
      case "W" =>
        println("Escolheu White!")
        Stone.White
      case _ =>
        println("Opção invalida. Introduza B ou W.")
        chooseStone()
    }
  }

  // Prints the numbered list of valid moves shown to the player before they choose.
  // Implemented as structural recursion on the list (Ficha 2).
  def showValidMoves(moves: List[(Coord2D, Coord2D)], index: Int): Unit = moves match {
    case Nil => ()
    case (from, to) :: tail =>
      println(s"  $index) $from -> $to")
      showValidMoves(tail, index + 1)
  }

  // Safe list indexing — returns None if the index is out of bounds.
  // Avoids lst(n) which throws IndexOutOfBoundsException.
  // Implemented with tail recursion (Ficha 2) and returns an Option (Ficha 5).
  @tailrec
  def getAt[A](lst: List[A], index: Int): Option[A] = lst match {
    case Nil                       => None
    case head :: _ if index == 0  => Some(head)
    case _ :: tail                 => getAt(tail, index - 1)
  }

  // Reads the player's move selection (1-based index into the displayed list).
  // Validates that the input is an integer within the valid range; retries otherwise.
  // Uses getAt (tail recursive, Option-returning) instead of direct list indexing (Fichas 2, 5, 6).
  @tailrec
  def chooseMove(validMovesList: List[(Coord2D, Coord2D)]): (Coord2D, Coord2D) = {
    print(s"Escolha o numero da jogada (1-${validMovesList.length}): ")
    val input = readLine().trim
    input.toIntOption match {
      case Some(n) if n >= 1 && n <= validMovesList.length =>
        getAt(validMovesList, n - 1) match {
          case Some(move) => move
          case None =>
            println("Opcao invalida. Tente novamente.")
            chooseMove(validMovesList)
        }
      case _ =>
        println("Opcao invalida. Introduza um numero valido.")
        chooseMove(validMovesList)
    }
  }

  // Converts a Stone value to a human-readable label for console output.
  def stoneName(s: Stone): String = s match {
    case Stone.Black => "Black (B)"
    case Stone.White => "White (W)"
  }
}
