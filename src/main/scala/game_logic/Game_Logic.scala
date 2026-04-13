package game_logic

import models.*
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec

// Tipos obrigatorios do enunciado
type Coord2D = (Int, Int) // (row, column)
type Board = ParMap[Coord2D, Stone]

object GameLogic {

  // Direcoes ortogonais possiveis (cima, baixo, esquerda, direita)
  val directions: List[(Int, Int)] = List((0, 1), (0, -1), (1, 0), (-1, 0))

  // Devolve a pedra adversaria (pattern matching - Ficha 2)
  def opponent(player: Stone): Stone = player match {
    case Stone.Black => Stone.White
    case Stone.White => Stone.Black
  }

  // ---------- inicializacao do tabuleiro ---------- //

  // Gera todas as posicoes com pedras alternadas
  @tailrec
  def generatePositions(rows: Int, cols: Int, r: Int, c: Int,
                        acc: List[(Coord2D, Stone)]): List[(Coord2D, Stone)] = {
    if (r >= rows) acc
    else if (c >= cols) generatePositions(rows, cols, r + 1, 0, acc)
    else {
      val stone = if ((r + c) % 2 == 0) Stone.Black else Stone.White
      generatePositions(rows, cols, r, c + 1, ((r, c), stone) :: acc)
    }
  }

  // Inicializa o board e devolve
  def initBoard(rows: Int, cols: Int): (Board, List[Coord2D]) = {
    val fullBoard: Map[Coord2D, Stone] = generatePositions(rows, cols, 0, 0, Nil).toMap

    // Remover uma preta e uma branca adjacentes do centro
    val centerR = rows / 2
    val centerC = cols / 2
    val blackCoord = if ((centerR + centerC) % 2 == 0) (centerR, centerC)
    else (centerR - 1, centerC)
    val whiteCoord = (blackCoord._1, blackCoord._2 + 1)

    val board = (fullBoard - blackCoord - whiteCoord).par
    val openCoords = List(blackCoord, whiteCoord)
    (board, openCoords)
  }

  // ---------- Funcoes auxiliares ---------- //

  // Verifica se uma coordenada esta na lista de posicoes livres
  @tailrec
  def isOpen(coord: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    lstOpenCoords match {
      case Nil => false
      case head :: tail =>
        if (head == coord) true
        else isOpen(coord, tail)
    }
  }

  // Remove a primeira ocorrencia de uma coordenada da lista

  def removeCoord(coord: Coord2D, lst: List[Coord2D]): List[Coord2D] = {
    lst match {
      case Nil => Nil
      case head :: tail =>
        if (head == coord) tail
        else head :: removeCoord(coord, tail)
    }
  }

  // Encontra jogadas validas para uma peca numa dada posicao
  // (filter + map - Ficha 3)
  def movesForPiece(coord: Coord2D, board: Map[Coord2D, Stone], player: Stone,
                    lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {
    directions.filter { case (dr, dc) =>
      val mid = (coord._1 + dr, coord._2 + dc)
      val dest = (coord._1 + 2 * dr, coord._2 + 2 * dc)
      board.get(mid) match {
        case Some(s) => s == opponent(player) && isOpen(dest, lstOpenCoords)
        case None => false
      }
    }.map { case (dr, dc) =>
      (coord, (coord._1 + 2 * dr, coord._2 + 2 * dc))
    }
  }

  // Devolve todas as jogadas validas para um jogador
  // (recursao com pattern matching + filter - Fichas 2, 3)
  def getValidMoves(board: Board, player: Stone,
                    lstOpenCoords: List[Coord2D]): List[(Coord2D, Coord2D)] = {
    val seqBoard = board.seq

    // Filtra apenas as pecas do jogador
    val playerPieces = seqBoard.toList.filter { case (_, stone) => stone == player }

    // Recolhe jogadas para cada peca (recursao explicita - Ficha 2)
    def collectMoves(pieces: List[(Coord2D, Stone)]): List[(Coord2D, Coord2D)] = {
      pieces match {
        case Nil => Nil
        case (coord: Coord2D, _) :: tail =>
          movesForPiece(coord, seqBoard.toMap, player, lstOpenCoords) ++ collectMoves(tail)
      }
    }

    collectMoves(playerPieces)
  }

  // ---------- T1: randomMove ----------
  // Gera uma coordenada aleatoria a partir da lista de posicoes livres
  // Recebe e devolve MyRandom para manter pureza funcional (Ficha 6)
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (index, nextRand) = rand.nextInt(lstOpenCoords.size)
    (lstOpenCoords(index), nextRand)
  }

  // ---------- T2: play ----------
  // Move a pedra de coordFrom para coordTo se for uma captura valida
  // Devolve (Some(novoBoard), novasCoordLivres) ou (None, coordLivres)
  // (pattern matching + Option - Fichas 2, 5)
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D,
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    val dr = coordTo._1 - coordFrom._1
    val dc = coordTo._2 - coordFrom._2
    val mid = (coordFrom._1 + dr / 2, coordFrom._2 + dc / 2)
    val seqBoard = board.seq

    // Validar: deve ser exatamente 2 casas numa direcao ortogonal
    val validDirection = ((dr == 2 || dr == -2) && dc == 0) ||
      (dr == 0 && (dc == 2 || dc == -2))

    // Validar: coordFrom tem a pedra do jogador
    val hasPlayerStone = seqBoard.get(coordFrom) match {
      case Some(s) => s == player
      case None => false
    }

    // Validar: posicao do meio tem pedra adversaria
    val hasOpponentMid = seqBoard.get(mid) match {
      case Some(s) => s == opponent(player)
      case None => false
    }

    // Validar: destino esta livre
    val destEmpty = isOpen(coordTo, lstOpenCoords)

    if (validDirection && hasPlayerStone && hasOpponentMid && destEmpty) {
      val newBoard = (seqBoard - coordFrom - mid + (coordTo -> player)).par
      val newOpenCoords = coordFrom :: mid :: removeCoord(coordTo, lstOpenCoords)
      (Some(newBoard), newOpenCoords)
    } else {
      (None, lstOpenCoords)
    }
  }

  // ---------- T3: playRandomly ----------
  // Funcao de ordem superior que joga aleatoriamente (Fichas 3, 4, 6)
  // Recebe funcao f para selecionar coordenada aleatoriamente
  def playRandomly(board: Board, r: MyRandom, player: Stone,
                   lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    val validMoves = getValidMoves(board, player, lstOpenCoords)
    validMoves match {
      case Nil => (None, r, lstOpenCoords, None)
      case _ =>
        // Obter lista de destinos e escolher um aleatoriamente via f
        val destinations = validMoves.map { case (_, to) => to }
        val (chosenTo, newRand) = f(destinations, r)

        // Encontrar a origem correspondente ao destino escolhido
        // (tail recursion com pattern matching - Fichas 2, 3)
        @tailrec
        def findFrom(moves: List[(Coord2D, Coord2D)]): Coord2D = moves match {
          case (from: Coord2D, to) :: tail =>
            if (to == chosenTo) from
            else findFrom(tail)
          case Nil => validMoves.head._1 // fallback
        }

        val chosenFrom = findFrom(validMoves)
        play(board, player, chosenFrom, chosenTo, lstOpenCoords) match {
          case (Some(newBoard), newOpenCoords) =>
            (Some(newBoard), newRand, newOpenCoords, Some(chosenTo))
          case (None, _) =>
            (None, newRand, lstOpenCoords, None)
        }
    }
  }

  // ---------- T4: displayBoard ----------
  // Representacao visual do tabuleiro (recursao + pattern matching - Ficha 2)
  def displayBoard(board: Board, rows: Int, cols: Int): String = {
    val seqBoard = board.seq

    def cellToString(r: Int, c: Int): String = seqBoard.get((r, c)) match {
      case Some(Stone.Black) => " B"
      case Some(Stone.White) => " W"
      case None              => " ."
    }

    // Constroi uma linha do tabuleiro (recursao - Ficha 2)
    def buildCells(r: Int, c: Int): String = {
      if (c >= cols) ""
      else cellToString(r, c) + " " + buildCells(r, c + 1)
    }

    // Constroi todas as linhas (recursao - Ficha 2)
    def buildRows(r: Int): List[String] = {
      if (r >= rows) Nil
      else {
        val rowLabel = if (r < 10) s" $r " else s"$r "
        (rowLabel + buildCells(r, 0)) :: buildRows(r + 1)
      }
    }

    // Cabecalho das colunas
    def buildHeader(c: Int): String = {
      if (c >= cols) ""
      else {
        val label = if (c < 10) s" $c" else s"$c"
        " " + label + buildHeader(c + 1)
      }
    }

    val header = "  " + buildHeader(0)
    (header :: buildRows(0)).mkString("\n")
  }

  def printBoard(board: Board, rows: Int, cols: Int): Unit = {
    println(displayBoard(board, rows, cols))
  }
}
