'REVERSI by Microsoft. It's an example in QB64.com
'Mods by Antoni Gual:
'  SCREEN 12 only
'  Removed black background in texts
'  Pieces flicker when set or flipped so better display
'  Expert mode relied short circuit eval that QB does'nt have so it crashed
'  Help updated
'
DEFINT A-Z
DECLARE SUB dobox (x%, y%, dx%, dy%)
DECLARE SUB viewscore ()
DECLARE FUNCTION CheckPath% (I, IBound, IStep, j, JBound, JStep, Opponent)
DECLARE FUNCTION ValidMove% (Opponent)
DECLARE SUB ComputerMove ()
DECLARE SUB DisplayHelp ()
DECLARE SUB displaymsg (a$)
DECLARE SUB DrawCursor (row, col)
DECLARE SUB DrawGamePiece (row, col, PieceColor)
DECLARE SUB GameOver ()
DECLARE SUB InitGame ()
DECLARE SUB TakeBlocks (row, col, player)
DECLARE SUB UpdateScore ()
DECLARE SUB UserMove ()
DECLARE SUB DrawGameBoard ()

OPTION BASE 1




CONST TRUE = -1
CONST FALSE = 0
CONST QUIT = 113
CONST UP = 72
CONST DOWN = 80
CONST LEFT = 75
CONST RIGHT = 77
CONST BBLOCK = 1
CONST EBLOCK = 8
CONST ENTER = 13
CONST ULEFT = 71
CONST URIGHT = 73
CONST DLEFT = 79
CONST DRIGHT = 81
CONST PASS = 112
CONST DIFF = 100
CONST START = 115
CONST HELP = 104
CONST FMOVE = 99
CONST SPACE = 32
CONST ESC = 27

TYPE GameGrid
    player AS INTEGER
    nTake  AS INTEGER
    cx     AS INTEGER
    cy     AS INTEGER
END TYPE

TYPE GameStatus
    curRow   AS INTEGER
    curCol   AS INTEGER
    stat     AS INTEGER
    rscore   AS INTEGER
    bscore   AS INTEGER
    mdisplay AS INTEGER
    dLevel   AS STRING * 6
    GColor   AS INTEGER
END TYPE

DIM SHARED GS AS GameStatus, smode AS INTEGER
DIM SHARED GG(8, 8) AS GameGrid, GBoard AS INTEGER
DIM SHARED COMP AS INTEGER, HUMAN AS INTEGER, BG AS INTEGER
DIM SHARED GP(8, 8, 8) AS INTEGER, GW(8, 8) AS INTEGER



  SCREEN 12
  PALETTE 0, &H202020
  PALETTE 7, &H0
 
  GS.stat = START
  GS.dLevel = "Novice"
  WHILE GS.stat <> QUIT
    IF GS.stat = START THEN
      InitGame
      DrawGameBoard
    END IF
    IF GS.stat <> COMP THEN
      IF ValidMove(COMP) THEN
        UserMove
      ELSEIF ValidMove(HUMAN) THEN
        DO
          displaymsg "You have no valid moves. Select (P)ass."
          DO
            a$ = INKEY$
          LOOP UNTIL a$ <> ""
        LOOP UNTIL ASC(RIGHT$(a$, 1)) = PASS
        displaymsg ("")
        GS.stat = COMP
        ComputerMove
      ELSE
        GameOver
      END IF
    ELSE
      IF ValidMove(HUMAN) THEN
        ComputerMove
      ELSEIF ValidMove(COMP) THEN
        displaymsg "Computer has no valid moves. Your Turn."
        GS.stat = HUMAN
        UserMove
        displaymsg ("")
      ELSE
        GameOver
      END IF
    END IF
  WEND
  displaymsg "Terminated by User. Press any key"
  SLEEP
END

FUNCTION CheckPath (I, IBound, IStep, j, JBound, JStep, Opponent)

  done = FALSE
  WHILE (I <> IBound OR j <> JBound) AND NOT done
    IF GG(I, j).player = GBoard THEN
      count = 0
      done = TRUE
    ELSEIF GG(I, j).player = Opponent THEN
      count = count + 1
      I = I + IStep
      j = j + JStep
      IF (I < 1 OR I > 8) OR (j < 1 OR j > 8) THEN
        count = 0
        done = TRUE
      END IF
    ELSE
      done = TRUE
    END IF
  WEND
  CheckPath = count
    
END FUNCTION

SUB ComputerMove
  BestMove = -99
  FOR row = 1 TO 8
    FOR col = 1 TO 8
      IF GG(row, col).nTake > 0 THEN
        IF GS.dLevel = "Novice" THEN
          value = GG(row, col).nTake + GW(row, col)
        ELSE
          value = GG(row, col).nTake + GW(row, col)
          SELECT CASE row
            CASE 1
              IF col < 5 THEN value = value + ABS(10 * GG(1, 1).player = COMP)
              IF col > 4 THEN value = value + ABS(10 * GG(1, 8).player = COMP)
            CASE 2
              IF GG(1, col).player <> COMP THEN value = value + 5 * (GG(1, col).player = HUMAN)
              IF col > 1 THEN IF GG(1, col - 1).player <> COMP THEN value = value + 5 * (GG(1, col - 1).player = HUMAN)
              IF col < 8 THEN IF GG(1, col + 1).player <> COMP THEN value = value + 5 * (GG(1, col + 1).player = HUMAN)
            CASE 7
              IF GG(8, col).player <> COMP THEN value = value + 5 * (GG(8, col).player = HUMAN)
              IF col > 1 THEN IF GG(8, col - 1).player <> COMP THEN value = value + 5 * (GG(8, col - 1).player = HUMAN)
              IF col < 8 THEN IF GG(8, col + 1).player <> COMP THEN value = value + 5 * (GG(8, col + 1).player = HUMAN)
            CASE 8
              IF col < 5 THEN value = value + ABS(10 * GG(8, 1).player = COMP)
              IF col > 4 THEN value = value + ABS(10 * GG(8, 8).player = COMP)
          END SELECT
          SELECT CASE col
            CASE 1
              IF row < 5 THEN value = value + ABS(10 * GG(1, 1).player = COMP)
              IF row > 4 THEN value = value + ABS(10 * GG(8, 1).player = COMP)
            CASE 2
              IF GG(row, 1).player <> COMP THEN value = value + 5 * (GG(row, 1).player = HUMAN)
              IF row > 1 THEN IF GG(row - 1, 1).player <> COMP THEN value = value + 5 * (GG(row - 1, 1).player = HUMAN)
              IF row < 8 THEN IF GG(row + 1, 1).player <> COMP THEN value = value + 5 * (GG(row + 1, 1).player = HUMAN)
            CASE 7
              IF GG(row, 8).player <> COMP THEN value = value + 5 * (GG(row, 8).player = HUMAN)
              IF row > 1 THEN IF GG(row - 1, 8).player <> COMP THEN value = value + 5 * (GG(row - 1, 8).player = HUMAN)
              IF row < 8 THEN IF GG(row + 1, 8).player <> COMP THEN value = value + 5 * (GG(row + 1, 8).player = HUMAN)
            CASE 8
              IF row < 5 THEN value = value + ABS(10 * GG(1, 8).player = COMP)
              IF row > 4 THEN value = value + ABS(10 * GG(8, 8).player = COMP)
          END SELECT
        END IF
        IF value > BestMove THEN
          BestMove = value
          bestrow = row
          bestcol = col
        END IF
      END IF
    NEXT col
  NEXT row

  TakeBlocks bestrow, bestcol, COMP
  GS.stat = HUMAN

END SUB

SUB DisplayHelp

  DIM a$(1 TO 18)

  a$(1) = "The object of Reversi is to finish the game with more of your red"
  a$(2) = "circles on the board than the computer has of blue "
  a$(3) = ""
  a$(4) = "1) You and the computer play by the same rules."
  a$(5) = "2) To make a legal move, at least one of the computer's circles"
  a$(6) = "   must lie in a horizontal, vertical, or diagonal line between"
  a$(7) = "   one of your existing circles and the square where you want to"
  
  a$(8) = "   move. Use the arrow keys to position the cursor on the square"
   a$(9) = "   (Cursor will be a circle in valid moves, a cross on invalid)"
  a$(10) = "   Hit Enter or the Space Bar to do the move."
  a$(11) = "3) You can choose Pass from the game controls menu on your first"
  a$(12) = "   move to force the computer to play first."
  a$(13) = "4) After your first move, you cannot pass if you can make a legal"
  a$(14) = "   move."
  a$(15) = "5) If you cannot make a legal move, you must choose Pass"
  a$(16) = "6) When neither you nor the computer can make a legal move, the"
  a$(17) = "   game is over."
  a$(18) = "7) The player that ends with the most circles is the winner."

  LINE (0, 0)-(640, 480), BG, BF  'fondo
  CALL dobox(30, 15, 560, 420)

  LOCATE 3, 35: PRINT "REVERSI HELP"
  FOR I = 1 TO 18
    LOCATE 5 + I, 7
    PRINT a$(I)
  NEXT I
  LOCATE 25, 25: PRINT "- Press any key to continue -"
  SLEEP: a$ = INKEY$
  DrawGameBoard
  DrawCursor GS.curRow, GS.curCol

END SUB

SUB displaymsg (a$)
  IF LEN(a$) THEN
      LOCATE 28, (80 - LEN(a$)) / 2: PRINT a$;
      GS.mdisplay = TRUE
  ELSE
      LOCATE 28, 10: PRINT SPACE$(61);
      GS.mdisplay = FALSE
  END IF
END SUB

SUB dobox (x, y, dx, dy)
  LINE (x + 10, y + 10)-STEP(dx, dy), 7, BF
  LINE (x, y)-STEP(dx, dy), 0, BF
  LINE (x, y)-STEP(dx, dy), 7, B

END SUB

SUB DrawCursor (row, col)
  IF GG(row, col).nTake > 0 THEN
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 15, HUMAN
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 14, HUMAN
  ELSE
    lc = 0
    IF GG(row, col).player = 0 THEN lc = 7
    LINE (GG(row, col).cx, GG(row, col).cy - 15)-STEP(1, 30), lc, B
    LINE (GG(row, col).cx - 15, GG(row, col).cy)-STEP(30, 1), lc, B
  END IF
END SUB

SUB DrawGameBoard
  LINE (0, 0)-(640, 480), 1, BF
  FOR I = 0 TO 478
  LINE (0, I)-STEP(639, 1), BG, , &H5555  'fondo
  NEXT
  CALL dobox(35, 10, 560, 25) 'titulo
  CALL dobox(269, 70, 320, 320) 'tablero
  CALL dobox(35, 260, 190, 130)  'inferior
  CALL dobox(35, 70, 190, 160)  'superior
  CALL dobox(35, 425, 560, 27) 'mensajes

  'grid
  FOR I = 0 TO 8
    LINE (270, 70 + I * 40)-STEP(320, 1), 7, B
    LINE (270 + I * 40, 70)-STEP(1, 320), 7, B
 NEXT I
  
  LOCATE 2, 30: COLOR 1: PRINT "R";
   COLOR 4: PRINT "  E  V  E  R  S  ";
   COLOR 1: PRINT "I";
   COLOR 7
  LOCATE 6, 11: COLOR 1: PRINT "Game Controls": COLOR 7
  LOCATE 8, 7: PRINT "S = Start New Game"
  LOCATE 9, 7: PRINT "P = Pass Turn"
  LOCATE 10, 7: PRINT "D = Set Difficulty"
  LOCATE 11, 7: PRINT "H = Display Help"
  LOCATE 12, 7: PRINT "Q = Quit"
  LOCATE 13, 7: PRINT "cursor kys = Position"
  LOCATE 14, 7: PRINT "Enter = your move"

  LOCATE 18, 12: COLOR 1: PRINT "Game Status": COLOR 7
  viewscore
  

  FOR row = 1 TO 8
    FOR col = 1 TO 8
      IF GG(row, col).player <> GBoard THEN
        DrawGamePiece row, col, GG(row, col).player
      END IF
    NEXT col
  NEXT row
END SUB

SUB DrawGamePiece (row, col, GpColor)
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 15, GpColor
    CIRCLE (GG(row, col).cx, GG(row, col).cy), 14, GpColor
     PAINT (GG(row, col).cx, GG(row, col).cy), 15, GpColor
     WAIT &H3DA, 8
     WAIT &H3DA, 8, 8
    PAINT (GG(row, col).cx, GG(row, col).cy), GpColor, GpColor
  

END SUB

SUB GameOver
  Scorediff = GS.rscore - GS.bscore
  IF Scorediff = 0 THEN
    b$ = "Tie Game "
  ELSEIF Scorediff < 0 THEN
    b$ = "You lost by"
    b$ = b$ + STR$(ABS(Scorediff))
  ELSE
    b$ = "You won by"
    b$ = b$ + STR$(Scorediff)
  END IF
  b$ = b$ + ". Select (Q)uit or (S)tart"
  displaymsg (b$)
  DO
  DO
    a$ = INKEY$
  LOOP UNTIL LEN(a$)
    GS.stat = ASC(RIGHT$(a$, 1))
  LOOP UNTIL GS.stat = QUIT OR GS.stat = START
  displaymsg ("")
END SUB

SUB InitGame
      HUMAN = 4
      COMP = 1
      BG = 3
      GBoard = 0

  
  GS.curCol = 5
  GS.curRow = 3
  GS.stat = FMOVE
  GS.bscore = 2
  GS.rscore = 2
  GS.mdisplay = FALSE

  FOR row = 1 TO 8
    FOR col = 1 TO 8
      GG(row, col).player = GBoard
      GG(row, col).nTake = 0
      GG(row, col).cx = 270 + (col - .5) * 40
      GG(row, col).cy = 70 + (row - .5) * 40
      GW(row, col) = 2
    NEXT col
  NEXT row
  GW(1, 1) = 99
  GW(1, 8) = 99
  GW(8, 1) = 99
  GW(8, 8) = 99
  FOR I = 3 TO 6
    FOR j = 1 TO 8 STEP 7
      GW(I, j) = 5
      GW(j, I) = 5
    NEXT j
  NEXT I
  GG(4, 4).player = HUMAN
  GG(5, 4).player = COMP
  GG(4, 5).player = COMP
  GG(5, 5).player = HUMAN
END SUB

SUB TakeBlocks (row, col, player)

  GG(row, col).player = player
  FOR I = 1 TO 3
     
     DrawGamePiece row, col, player
     WAIT &H3DA, 8
     
  NEXT

  FOR I = 1 TO GP(row, col, 1)
    GG(row, col - I).player = player
    DrawGamePiece row, col - I, player
  NEXT I
  FOR I = 1 TO GP(row, col, 2)
    GG(row, col + I).player = player
    DrawGamePiece row, col + I, player
  NEXT I
  FOR I = 1 TO GP(row, col, 3)
    GG(row - I, col).player = player
    DrawGamePiece row - I, col, player
  NEXT I
  FOR I = 1 TO GP(row, col, 4)
    GG(row + I, col).player = player
    DrawGamePiece row + I, col, player
  NEXT I
  FOR I = 1 TO GP(row, col, 5)
    GG(row - I, col - I).player = player
    DrawGamePiece row - I, col - I, player
  NEXT I
  FOR I = 1 TO GP(row, col, 6)
    GG(row + I, col + I).player = player
    DrawGamePiece row + I, col + I, player
  NEXT I
  FOR I = 1 TO GP(row, col, 7)
    GG(row - I, col + I).player = player
    DrawGamePiece row - I, col + I, player
  NEXT I
  FOR I = 1 TO GP(row, col, 8)
    GG(row + I, col - I).player = player
    DrawGamePiece row + I, col - I, player
  NEXT I

  IF player = HUMAN THEN
    GS.rscore = GS.rscore + GG(row, col).nTake + 1
    GS.bscore = GS.bscore - GG(row, col).nTake
  ELSE
    GS.bscore = GS.bscore + GG(row, col).nTake + 1
    GS.rscore = GS.rscore - GG(row, col).nTake
  END IF
  viewscore

END SUB

SUB UserMove

  DrawCursor GS.curRow, GS.curCol
  DO
    DO
      a$ = INKEY$
    LOOP UNTIL a$ <> ""
    move = ASC(RIGHT$(a$, 1))
    IF GS.mdisplay THEN displaymsg ("")
    SELECT CASE move
      CASE 71 TO 81:
        DrawGamePiece GS.curRow, GS.curCol, GG(GS.curRow, GS.curCol).player
        IF move < 74 THEN
          IF GS.curRow = BBLOCK THEN
            GS.curRow = EBLOCK
          ELSE
            GS.curRow = GS.curRow - 1
          END IF
        ELSEIF move > 78 THEN
          IF GS.curRow = EBLOCK THEN
            GS.curRow = BBLOCK
          ELSE
            GS.curRow = GS.curRow + 1
          END IF
        END IF
        IF move = 71 OR move = 75 OR move = 79 THEN
          IF GS.curCol = BBLOCK THEN
            GS.curCol = EBLOCK
          ELSE
            GS.curCol = GS.curCol - 1
          END IF
        ELSEIF move = 73 OR move = 77 OR move = 81 THEN
          IF GS.curCol = EBLOCK THEN
            GS.curCol = BBLOCK
          ELSE
            GS.curCol = GS.curCol + 1
          END IF
        END IF
        DrawCursor GS.curRow, GS.curCol
      CASE START:
        GS.stat = START
      CASE PASS:
        IF GS.stat = FMOVE THEN
          displaymsg "You passed.  Computer will make first move."
          GS.stat = COMP
        ELSE
          displaymsg "You can only pass on your first turn."
        END IF
      CASE HELP:
        DisplayHelp
      CASE DIFF:
        IF GS.dLevel = "Novice" THEN
          GS.dLevel = "Expert"
        ELSE
          GS.dLevel = "Novice"
        END IF
      CALL viewscore
      CASE ENTER, SPACE, 76:
        IF GG(GS.curRow, GS.curCol).nTake > 0 THEN
          TakeBlocks GS.curRow, GS.curCol, HUMAN
          GS.stat = COMP
        ELSE
          displaymsg "Invalid move. Move to a space where the cursor is a circle."
        END IF
      CASE ESC, QUIT:
        GS.stat = QUIT
    END SELECT
  LOOP UNTIL GS.stat <> HUMAN AND GS.stat <> FMOVE

END SUB

FUNCTION ValidMove (Opponent)

  ValidMove = FALSE
  ERASE GP
  FOR row = 1 TO 8
    FOR col = 1 TO 8
      GG(row, col).nTake = 0

      IF GG(row, col).player = GBoard THEN
        IF col > 2 THEN
          GP(row, col, 1) = CheckPath(row, row, 0, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 1)
        END IF
        IF col < 7 THEN
          GP(row, col, 2) = CheckPath(row, row, 0, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 2)
        END IF
        IF row > 2 THEN
          GP(row, col, 3) = CheckPath(row - 1, 0, -1, col, col, 0, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 3)
        END IF
        IF row < 7 THEN
          GP(row, col, 4) = CheckPath(row + 1, 9, 1, col, col, 0, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 4)
        END IF
        IF col > 2 AND row > 2 THEN
          GP(row, col, 5) = CheckPath(row - 1, 0, -1, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 5)
        END IF
        IF col < 7 AND row < 7 THEN
          GP(row, col, 6) = CheckPath(row + 1, 9, 1, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 6)
        END IF
        IF col < 7 AND row > 2 THEN
          GP(row, col, 7) = CheckPath(row - 1, 0, -1, col + 1, 9, 1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 7)
        END IF
        IF col > 2 AND row < 7 THEN
          GP(row, col, 8) = CheckPath(row + 1, 9, 1, col - 1, 0, -1, Opponent)
          GG(row, col).nTake = GG(row, col).nTake + GP(row, col, 8)
        END IF
        IF GG(row, col).nTake > 0 THEN ValidMove = TRUE
      END IF
    NEXT col
  NEXT row

END FUNCTION

SUB viewscore
  LOCATE 20, 7: PRINT "Your Score:      "; GS.rscore
  LOCATE 21, 7: PRINT "Computer Score:  "; GS.bscore
  LOCATE 23, 7: PRINT "Difficulty:   "; GS.dLevel
END SUB

