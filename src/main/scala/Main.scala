import game_logic.*
import game_logic.GameLogic.*
import models.*

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    println("=== Jogo Konane ===")
    println()

    val (rows, cols) = (4,4)
    val (board, openCoords) = initBoard(rows, cols)

    println()
    println(s"Tabuleiro ${rows}x${cols} inicializado:")
    println()
    printBoard(board, rows, cols)
    println()

    val playerStone = chooseStone()
    val computerStone = opponent(playerStone)
    println(s"Jogador: ${stoneName(playerStone)} | Computador: ${stoneName(computerStone)}")
    println("Black começa a jogar.")
    println()

    // Seed baseada no tempo para resultados diferentes a cada execucao (Ficha 6)
    val rand = MyRandom(System.currentTimeMillis())

    // Inicia o ciclo de jogo - Black joga primeiro
    gameLoop(board, openCoords, Stone.Black, playerStone, rand, rows, cols)
  }

  // ---------- Ciclo de jogo principal (tail recursion - Fichas 2, 6) ----------

  // Estado do jogo passa como parametros - imutabilidade (Ficha 5)
  @tailrec
  def gameLoop(board: Board, lstOpenCoords: List[Coord2D], currentPlayer: Stone,
               playerStone: Stone, rand: MyRandom, rows: Int, cols: Int): Unit = {

    println()
    printBoard(board, rows, cols)
    println(s"  Black: ${countStones(board, Stone.Black)} pedras | White: ${countStones(board, Stone.White)} pedras")
    println()

    if (isOver(board, currentPlayer, lstOpenCoords)) {
      // Jogador sem jogadas validas perde (T5)
      val winner = opponent(currentPlayer)
      println(s"${stoneName(currentPlayer)} nao tem jogadas validas!")
      println(s"=== ${stoneName(winner)} GANHOU o jogo! ===")

    } else if (currentPlayer == playerStone) {
      // Turno do jogador player
      println(s"--- Sua vez (${stoneName(playerStone)}) ---")
      val (newBoard, newOpen) = doplayerTurn(board, lstOpenCoords, playerStone, rows, cols, None)
      gameLoop(newBoard, newOpen, opponent(playerStone), playerStone, rand, rows, cols)

    } else {
      // Turno do computador (jogada aleatoria com playRandomly - T3)
      println(s"--- Vez do Computador (${stoneName(currentPlayer)}) ---")
      val (optBoard, nextRand, newOpen, optTo) =
        playRandomly(board, rand, currentPlayer, lstOpenCoords, randomMove)

      optBoard match {
        case Some(newBoard) =>
          println(s"Computador jogou para: ${optTo.get}")
          gameLoop(newBoard, newOpen, opponent(currentPlayer), playerStone, nextRand, rows, cols)
        case None =>
          // Situacao coberta pelo isOver acima, mas tratada por seguranca
          println(s"=== ${stoneName(playerStone)} GANHOU o jogo! ===")
      }
    }
  }

  // ---------- Turno do jogador playero com suporte a multi-salto (recursao - Ficha 2) ----------

  // fromOpt: None = primeiro salto (pode escolher qualquer pedra)
  //          Some(pos) = continuacao (tem de usar a mesma pedra em 'pos')
  // Recursao: chamada recursiva em posicao de cauda (Ficha 2)
  def doplayerTurn(board: Board, lstOpenCoords: List[Coord2D], player: Stone,
                  rows: Int, cols: Int, fromOpt: Option[Coord2D]): (Board, List[Coord2D]) = {

    // Determina jogadas possiveis usando pattern matching em Option (Ficha 5)
    val moves = fromOpt match {
      case None       => validMoves(board, player, lstOpenCoords)
      case Some(from) => validMovesFrom(board, player, from, lstOpenCoords)
    }

    if (moves.isEmpty) {
      (board, lstOpenCoords) // sem continuacao possivel - devolve estado actual
    } else {
      fromOpt match {
        case None    => println("Jogadas validas:")
        case Some(_) => println("Continuacao - escolha a proxima captura:")
      }
      showValidMoves(moves, 1)
      val (from, to) = chooseMove(moves)

      // Efectua a jogada usando pattern matching em Option (Ficha 5)
      play(board, player, from, to, lstOpenCoords) match {
        case (Some(newBoard), newOpen) =>
          println()
          printBoard(newBoard, rows, cols)

          // Verifica se pode continuar a saltar com a mesma pedra (multi-salto)
          val contMoves = validMovesFrom(newBoard, player, to, newOpen)
          if (contMoves.isEmpty) {
            (newBoard, newOpen) // nao ha continuacao - fim do turno
          } else {
            println("Pode continuar a capturar com a mesma pedra! Deseja continuar? (S/N)")
            val ans = readLine().trim.toUpperCase
            // Recursao para o multi-salto com a mesma pedra (Ficha 2)
            if (ans == "S") doplayerTurn(newBoard, newOpen, player, rows, cols, Some(to))
            else (newBoard, newOpen)
          }
        case (None, _) =>
          // Nao deve acontecer pois a jogada veio de validMoves
          println("Jogada invalida. Tente novamente.")
          doplayerTurn(board, lstOpenCoords, player, rows, cols, fromOpt)
      }
    }
  }


  // ---------- Escolha da pedra (IO + tail recursion + pattern matching - Fichas 2, 6) ----------

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

  // ---------- Apresentacao de jogadas validas (recursao + pattern matching - Ficha 2) ----------

  def showValidMoves(moves: List[(Coord2D, Coord2D)], index: Int): Unit = moves match {
    case Nil => ()
    case (from, to) :: tail =>
      println(s"  $index) $from -> $to")
      showValidMoves(tail, index + 1)
  }

  // ---------- Acesso a elemento numa lista por indice (tail recursion + Option - Fichas 2, 5) ----------

  @tailrec
  def getAt[A](lst: List[A], index: Int): Option[A] = lst match {
    case Nil                      => None
    case head :: _ if index == 0  => Some(head)
    case _ :: tail                => getAt(tail, index - 1)
  }

  // ---------- Escolha de jogada pelo utilizador (IO + tail recursion + Option - Fichas 2, 5, 6) ----------

  @tailrec
  def chooseMove(validMovesList: List[(Coord2D, Coord2D)]): (Coord2D, Coord2D) = {
    print(s"Escolha o numero da jogada (1-${validMovesList.length}): ")
    val input = readLine().trim
    input.toIntOption match {
      case Some(n) if n >= 1 && n <= validMovesList.length =>
        // Usa getAt com tail recursion em vez de acesso directo por indice (Ficha 2)
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

  // ---------- Auxiliar: nome da pedra (pattern matching - Ficha 2) ----------

  def stoneName(s: Stone): String = s match {
    case Stone.Black => "Black (B)"
    case Stone.White => "White (W)"
  }
}
