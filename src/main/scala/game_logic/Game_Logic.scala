package game_logic

import models.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

// Tipos obrigatorios do enunciado
type Coord2D = (Int, Int) // (row, column)
type Board = ParMap[Coord2D, Stone]

object GameLogic {

  // Direcoes ortogonais: direita, esquerda, baixo, cima
  val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))

  // Devolve a pedra adversaria - pattern matching (Ficha 2)
  def opponent(player: Stone): Stone = player match {
    case Stone.Black => Stone.White
    case Stone.White => Stone.Black
  }

  // ---------- inicializacao do tabuleiro (T2) ----------

  // Gera todas as posicoes do tabuleiro com pedras alternadas usando tail recursion (Ficha 2)
  @tailrec
  private def generatePositions(rows: Int, cols: Int, r: Int, c: Int,
                                acc: List[(Coord2D, Stone)]): List[(Coord2D, Stone)] = {
    if (r >= rows) acc
    else if (c >= cols) generatePositions(rows, cols, r + 1, 0, acc)
    else {
      val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
      generatePositions(rows, cols, r, c + 1, ((r, c), stone) :: acc)
    }
  }

  // Inicializa o tabuleiro removendo uma pedra Black e uma White adjacente do centro
  // Devolve (Board, lista de coordenadas livres iniciais)
  def initBoard(rows: Int, cols: Int): (Board, List[Coord2D]) = {
    val positions = generatePositions(rows, cols, 0, 0, Nil)
    val fullMap   = positions.toMap

    // Remove do centro: Black em (centerR, centerC) e White em (centerR, centerC+1)
    // Garante padrao alternado: soma par = Black, soma impar = White
    val centerR = rows / 2 - 1
    val centerC = cols / 2 - 1
    val blackCoord = (centerR, centerC)
    val whiteCoord = (centerR, centerC + 1)

    val board = (fullMap - blackCoord - whiteCoord).par
    val openCoords: List[Coord2D] = List(blackCoord, whiteCoord)
    (board, openCoords)
  }

  // ---------- T1: randomMove ----------

  // Seleciona uma coordenada aleatoria da lista de posicoes livres
  // Recebe e devolve MyRandom para manter pureza funcional (Ficha 6)
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (index, nextRand) = rand.nextInt(lstOpenCoords.size)
    (lstOpenCoords(index), nextRand)
  }

  // ---------- T2: play ----------

  // Move a pedra do jogador de coordFrom para coordTo (salto simples sobre uma pedra inimiga)
  // Devolve (Some(novoBoard), novasCoordLivres) se valido, (None, coordLivres) caso contrario
  // Funcao pura com dados imutaveis (Fichas 5, 6)
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D,
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {

    val (r1, c1) = coordFrom
    val (r2, c2) = coordTo
    val dr = r2 - r1
    val dc = c2 - c1

    // Salto valido: exactamente 2 casas numa direccao ortogonal
    val isValidDirection = (dr == 0 && math.abs(dc) == 2) || (dc == 0 && math.abs(dr) == 2)
    val enemyCoord: Coord2D = (r1 + dr / 2, c1 + dc / 2)

    val isValid = isValidDirection &&
      board.get(coordFrom).contains(player) &&
      board.get(enemyCoord).contains(opponent(player)) &&
      lstOpenCoords.contains(coordTo)

    if (isValid) {
      val newBoard = board - coordFrom - enemyCoord + (coordTo -> player)
      // coordFrom e enemyCoord ficam livres; coordTo fica ocupada
      val newOpen  = coordFrom :: enemyCoord :: lstOpenCoords.filterNot(_ == coordTo)
      (Some(newBoard), newOpen)
    } else {
      (None, lstOpenCoords)
    }
  }

  // ---------- Auxiliares de validacao de jogadas ----------

  // Devolve todos os movimentos validos (salto simples) para um jogador
  // Usa list comprehension monadica - funcao de ordem superior (Ficha 3)
  def validMoves(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {
    val playerStones = board.filter(_._2 == player).keys.toList
    for {
      from <- playerStones
      to   <- lstOpenCoords
      (optBoard, _) = play(board, player, from, to, lstOpenCoords)
      if optBoard.isDefined
    } yield (from, to)
  }

  // Devolve movimentos de continuacao validos a partir de uma posicao especifica (multi-salto)
  // Funcao de ordem superior com list comprehension (Ficha 3)
  def validMovesFrom(board: Board, player: Stone, from: Coord2D,
                     lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] =
    for {
      to <- lstOpenCoords
      (optBoard, _) = play(board, player, from, to, lstOpenCoords)
      if optBoard.isDefined
    } yield (from, to)

  // ---------- T3: playRandomly ----------

  // Funcao de ordem superior: joga aleatoriamente usando a funcao f para selecionar coordenadas
  // Recebe e devolve MyRandom (pureza funcional - Fichas 3, 4, 6)
  // Devolve Option[(from, to)] para que o chamador possa mostrar a jogada efectuada
  def playRandomly(board: Board,
                   r: MyRandom,
                   player: Stone,
                   lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[(Coord2D, Coord2D)]) = {

    val moves = validMoves(board, player, lstOpenCoords)

    if (moves.isEmpty) {
      (None, r, lstOpenCoords, None)
    } else {
      // Usa a funcao f (HOF) para selecionar aleatoriamente a pedra a mover
      val validFroms = moves.map(_._1).distinct
      val (selectedFrom, nextR) = f(validFroms, r)
      val possibleTos = moves.filter(_._1 == selectedFrom).map(_._2)
      val selectedTo = possibleTos.head

      // Efectua a jogada (sabemos que e valida) - match exaustivo para evitar pattern refutavel
      play(board, player, selectedFrom, selectedTo, lstOpenCoords) match {
        case (Some(newBoard), newOpen) => (Some(newBoard), nextR, newOpen, Some((selectedFrom, selectedTo)))
        case (None, _) => (None, nextR, lstOpenCoords, None)
      }
    }
  }

  // ---------- T4: displayBoard ----------

  // Imprime cabecalho de colunas usando tail recursion (Ficha 2)
  @tailrec
  private def printHeader(c: Int, cols: Int): Unit =
    if (c < cols) {
      print(s"$c ")
      printHeader(c + 1, cols)
    }

  // Imprime as celulas de uma linha usando tail recursion e pattern matching (Ficha 2)
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

  // Imprime todas as linhas do tabuleiro usando tail recursion (Ficha 2)
  @tailrec
  def printRows(board: Board, rows: Int, cols: Int, r: Int): Unit =
    if (r < rows) {
      print(s"$r ")
      printCols(board, r, cols, 0)
      println()
      printRows(board, rows, cols, r + 1)
    }

  // Representacao visual do tabuleiro na linha de comando (T4)
  def printBoard(board: Board, rows: Int, cols: Int): Unit = {
    print("  ")
    printHeader(0, cols)
    println()
    printRows(board, rows, cols, 0)
  }

  // ---------- T5: isOver ---------- //
  // Verifica se o jogador actual nao tem jogadas validas (perdeu o jogo)
  def isOver(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): Boolean =
    validMoves(board, player, lstOpenCoords).isEmpty

  // Conta o número de pedras de um jogador usando foldLeft (Ficha 4 - Folding)
  def countStones(board: Board, player: Stone): Int =
    board.foldLeft(0) { case (acc, (_, stone)) =>
      if (stone == player) acc + 1 else acc
    }
}
