import game_logic.*
import game_logic.GameLogic.*
import models.*

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    val rows = 6
    val cols = 6

    // Inicializar tabuleiro
    val (board, openCoords) = initBoard(rows, cols)

    println("=== Jogo Konane ===")
    println(s"Tabuleiro ${rows}x${cols}")
    println()
    printBoard(board, rows, cols)
    println()

    // Jogador escolhe a sua pedra
    val humanStone = chooseStone()
    val computerStone = opponent(humanStone)

    val humanName = stoneName(humanStone)
    val computerName = stoneName(computerStone)
    println(s"Jogador: $humanName | Computador: $computerName")
    println(s"Black comeca sempre a jogar.")
    println()

    // Seed baseada no tempo para resultados diferentes a cada execucao (Ficha 6)
    val rand = MyRandom(System.currentTimeMillis())

    // Black comeca sempre
    gameLoop(board, rand, Stone.Black, openCoords, humanStone, rows, cols, 1)
  }

  // ---------- Escolher pedra (IO + tail recursion - Ficha 6) ----------

  def stoneName(s: Stone): String = s match {
    case Stone.Black => "Black (B)"
    case Stone.White => "White (W)"
  }

  @tailrec
  def chooseStone(): Stone = {
    println("Escolha a sua pedra (B/W):")
    readLine().trim.toUpperCase match {
      case "B" =>
        println("Escolheu Black!")
        Stone.Black
      case "W" =>
        println("Escolheu White!")
        Stone.White
      case _ =>
        println("Opcao invalida. Introduza B ou W.")
        chooseStone()
    }
  }

  // ---------- Mostrar jogadas validas (recursao - Ficha 2) ----------

  def showValidMoves(moves: List[(Coord2D, Coord2D)], index: Int): Unit = {
    moves match {
      case Nil => ()
      case (from, to) :: tail =>
        println(s"  $index) $from -> $to")
        showValidMoves(tail, index + 1)
    }
  }

  // ---------- Jogador escolhe jogada (IO + tail recursion + Option - Fichas 5, 6) ----------

  // Aceder ao elemento na posicao index de uma lista (recursao - Ficha 2)
  @tailrec
  def getAt[A](lst: List[A], index: Int): Option[A] = {
    lst match {
      case Nil => None
      case head :: _ if index == 0 => Some(head)
      case _ :: tail => getAt(tail, index - 1)
    }
  }

  @tailrec
  def chooseMove(validMoves: List[(Coord2D, Coord2D)]): (Coord2D, Coord2D) = {
    println("Escolha o numero da jogada:")
    val input = readLine().trim
    input.toIntOption match {
      case Some(n) if n >= 1 && n <= validMoves.length =>
        getAt(validMoves, n - 1) match {
          case Some(move) => move
          case None =>
            println("Opcao invalida. Tente novamente.")
            chooseMove(validMoves)
        }
      case _ =>
        println("Opcao invalida. Tente novamente.")
        chooseMove(validMoves)
    }
  }

  // ---------- Loop principal do jogo (tail recursion - Ficha 3/6) ----------

  @tailrec
  def gameLoop(board: Board, r: MyRandom, currentPlayer: Stone,
               lstOpenCoords: List[Coord2D], humanStone: Stone,
               rows: Int, cols: Int, turn: Int): Unit = {

    val name = stoneName(currentPlayer)
    val validMoves = getValidMoves(board, currentPlayer, lstOpenCoords)

    // Se nao ha jogadas validas, o jogador atual perde
    validMoves match {
      case Nil =>
        val winnerName = stoneName(opponent(currentPlayer))
        println(s"--- Turno $turn: $name ---")
        println(s"$name nao tem jogadas validas!")
        println()
        println(s"=== $winnerName vence o jogo em $turn turnos! ===")

      case _ =>
        println(s"--- Turno $turn: $name ---")

        if (currentPlayer == humanStone) {
          // Vez do jogador humano
          println("As suas jogadas validas:")
          showValidMoves(validMoves, 1)
          val (from, to) = chooseMove(validMoves)

          play(board, currentPlayer, from, to, lstOpenCoords) match {
            case (Some(newBoard), newOpenCoords) =>
              println(s"Jogou: $from -> $to")
              printBoard(newBoard, rows, cols)
              println()
              gameLoop(newBoard, r, opponent(currentPlayer), newOpenCoords, humanStone, rows, cols, turn + 1)

            case (None, _) =>
              // Nao devera acontecer pois a jogada vem da lista de validas
              println("Erro: jogada invalida.")
              gameLoop(board, r, currentPlayer, lstOpenCoords, humanStone, rows, cols, turn)
          }

        } else {
          // Vez do computador (T3 - playRandomly com funcao de ordem superior)
          playRandomly(board, r, currentPlayer, lstOpenCoords, randomMove) match {
            case (Some(newBoard), newRand, newOpenCoords, Some(movedTo)) =>
              println(s"Computador jogou para $movedTo")
              printBoard(newBoard, rows, cols)
              println()
              gameLoop(newBoard, newRand, opponent(currentPlayer), newOpenCoords, humanStone, rows, cols, turn + 1)

            case _ =>
              println("Erro inesperado.")
          }
        }
    }
  }
}
