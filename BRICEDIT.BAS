DECLARE SUB Animation ()
DECLARE SUB Grid ()
DECLARE SUB BrickBrowser ()
DECLARE SUB ClearBoard ()
DECLARE SUB DrawBoard ()
DECLARE SUB DrawBlock (StartPixX, StartPixY)
DECLARE SUB Editor ()
DECLARE SUB ExpressionDecoder ()
DECLARE SUB LoadBricks ()
DECLARE SUB MainMenu ()
DECLARE SUB SaveBricks ()
DECLARE SUB Statistics ()
DECLARE FUNCTION BrickCounter ()
DECLARE FUNCTION BrickArea ()

SCREEN 12

DIM SHARED BrickExp AS STRING
DIM SHARED ChangedBrickExp AS STRING
DIM SHARED Board(10, 5, 24) AS INTEGER
DIM SHARED CurTriNo AS INTEGER
DIM SHARED CurBloX AS INTEGER
DIM SHARED CurBloY AS INTEGER
DIM SHARED Bricks(210) AS STRING
DIM SHARED BrickColor AS INTEGER
DIM SHARED FillColor AS INTEGER

FillColor = 2
BrickColor = 1

CALL LoadBricks

DO
	CALL MainMenu
LOOP

SUB Animation
	FOR t = 1 TO 24
		CLS
		CurTriNo = t
		LOCATE 25, 5
		PRINT "Header Triangle: "; CurTriNo
		CurBloX = 5
		CurBloY = 3
		CALL ExpressionDecoder
		CALL DrawBoard
		FOR Delay = 1 TO 10000
		NEXT Delay
	NEXT t
END SUB

FUNCTION BrickArea
	a = 0
	FOR y = 1 TO 5
		FOR x = 1 TO 10
			FOR t = 1 TO 24
				IF NOT Board(x, y, t) = 0 THEN a = a + 1
			NEXT t
		NEXT x
	NEXT y
	IF a = 1 THEN a = 0
	BrickArea = a
END FUNCTION

SUB BrickBrowser
	BrickNo = 1
 
	LOCATE 5, 45
	PRINT "*        BRICK BROWSER         *"
	LOCATE 6, 45
	PRINT "                               "
	LOCATE 7, 45
	PRINT "Brick viewpoint (1-24):"
	FOR y = 8 TO 20
		LOCATE y, 45
		PRINT "                                 "
	NEXT y
	LOCATE 8, 45
	INPUT Viewpoint
	LOCATE 10, 45
	PRINT "p=previous  n=next"
	LOCATE 11, 45
	PRINT "m=return to main menu"
 
DO
	CALL ClearBoard
	BrickExp = Bricks(BrickNo)
	CurTriNo = Viewpoint
	CALL ExpressionDecoder
	CALL DrawBoard
	LOCATE 13, 45
	PRINT "Displayed Brick:"; BrickNo
	LOCATE 15, 45
	INPUT Choise$
	SELECT CASE Choise$
			CASE "p"
				BrickNo = BrickNo - 1
				IF BrickNo = 0 THEN BrickNo = 209
			CASE "n"
				BrickNo = BrickNo + 1
				IF BrickNo = 210 THEN BrickNo = 1
			CASE "m"
				GOTO EndBrowser
	END SELECT
LOOP

EndBrowser:

END SUB

FUNCTION BrickCounter
	c = 0
	FOR n = 1 TO 210
		IF LEN(Bricks(n)) > 1 THEN c = c + 1
	NEXT n
	BrickCounter = c
END FUNCTION

SUB ClearBoard
	FillColor = 0
	BrickColor = 0
	CALL DrawBoard
	FillColor = 2
	BrickColor = 1
END SUB

SUB DrawBlock (x, y)
	IF Board(CurBloX, CurBloY, 1) = 1 THEN
		LINE (x, y)-(x, y + 20), BrickColor
		LINE (x, y)-(x + 7, y + 15), BrickColor
		LINE (x, y + 20)-(x + 7, y + 15), BrickColor
		PAINT (x + 2, y + 10), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 2) = 1 THEN
		LINE (x, y)-(x + 7, y + 15), BrickColor
		LINE (x, y)-(x + 15, y + 10), BrickColor
		LINE (x + 7, y + 15)-(x + 15, y + 10), BrickColor
		PAINT (x + 7, y + 10), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 3) = 1 THEN
		LINE (x, y)-(x + 15, y), BrickColor
		LINE (x, y)-(x + 15, y + 10), BrickColor
		LINE (x + 15, y)-(x + 15, y + 10), BrickColor
		PAINT (x + 7, y + 2), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 4) = 1 THEN
		LINE (x + 15, y)-(x + 30, y), BrickColor
		LINE (x + 15, y)-(x + 15, y + 10), BrickColor
		LINE (x + 15, y + 10)-(x + 30, y), BrickColor
		PAINT (x + 17, y + 2), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 5) = 1 THEN
		LINE (x + 15, y + 10)-(x + 30, y), BrickColor
		LINE (x + 15, y + 10)-(x + 23, y + 15), BrickColor
		LINE (x + 23, y + 15)-(x + 30, y), BrickColor
		PAINT (x + 23, y + 10), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 6) = 1 THEN
		LINE (x + 23, y + 15)-(x + 30, y), BrickColor
		LINE (x + 23, y + 15)-(x + 30, y + 20), BrickColor
		LINE (x + 30, y)-(x + 30, y + 20), BrickColor
		PAINT (x + 28, y + 10), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 7) = 1 THEN
		LINE (x, y + 20)-(x, y + 30), BrickColor
		LINE (x, y + 20)-(x + 15, y + 30), BrickColor
		LINE (x, y + 30)-(x + 15, y + 30), BrickColor
		PAINT (x + 4, y + 28), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 8) = 1 THEN
		LINE (x, y + 20)-(x + 7, y + 15), BrickColor
		LINE (x, y + 20)-(x + 15, y + 30), BrickColor
		LINE (x + 7, y + 15)-(x + 15, y + 30), BrickColor
		PAINT (x + 7, y + 20), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 9) = 1 THEN
		LINE (x + 7, y + 15)-(x + 15, y + 30), BrickColor
		LINE (x + 7, y + 15)-(x + 15, y + 10), BrickColor
		LINE (x + 15, y + 10)-(x + 15, y + 30), BrickColor
		PAINT (x + 13, y + 20), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 10) = 1 THEN
		LINE (x + 15, y + 10)-(x + 15, y + 30), BrickColor
		LINE (x + 15, y + 10)-(x + 23, y + 15), BrickColor
		LINE (x + 23, y + 15)-(x + 15, y + 30), BrickColor
		PAINT (x + 17, y + 20), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 11) = 1 THEN
		LINE (x + 23, y + 15)-(x + 15, y + 30), BrickColor
		LINE (x + 23, y + 15)-(x + 30, y + 20), BrickColor
		LINE (x + 30, y + 20)-(x + 15, y + 30), BrickColor
		PAINT (x + 23, y + 21), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 12) = 1 THEN
		LINE (x + 30, y + 20)-(x + 15, y + 30), BrickColor
		LINE (x + 30, y + 20)-(x + 30, y + 30), BrickColor
		LINE (x + 30, y + 30)-(x + 15, y + 30), BrickColor
		PAINT (x + 28, y + 28), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 13) = 1 THEN
		LINE (x, y + 30)-(x + 15, y + 30), BrickColor
		LINE (x, y + 30)-(x, y + 40), BrickColor
		LINE (x, y + 40)-(x + 15, y + 30), BrickColor
		PAINT (x + 4, y + 32), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 14) = 1 THEN
		LINE (x, y + 40)-(x + 15, y + 30), BrickColor
		LINE (x, y + 40)-(x + 7, y + 45), BrickColor
		LINE (x + 7, y + 45)-(x + 15, y + 30), BrickColor
		PAINT (x + 7, y + 40), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 15) = 1 THEN
		LINE (x + 7, y + 45)-(x + 15, y + 30), BrickColor
		LINE (x + 7, y + 45)-(x + 15, y + 50), BrickColor
		LINE (x + 15, y + 50)-(x + 15, y + 30), BrickColor
		PAINT (x + 13, y + 40), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 16) = 1 THEN
		LINE (x + 15, y + 50)-(x + 15, y + 30), BrickColor
		LINE (x + 15, y + 50)-(x + 23, y + 45), BrickColor
		LINE (x + 23, y + 45)-(x + 15, y + 30), BrickColor
		PAINT (x + 17, y + 40), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 17) = 1 THEN
		LINE (x + 23, y + 45)-(x + 15, y + 30), BrickColor
		LINE (x + 23, y + 45)-(x + 30, y + 40), BrickColor
		LINE (x + 30, y + 40)-(x + 15, y + 30), BrickColor
		PAINT (x + 23, y + 40), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 18) = 1 THEN
		LINE (x + 30, y + 30)-(x + 15, y + 30), BrickColor
		LINE (x + 15, y + 30)-(x + 30, y + 40), BrickColor
		LINE (x + 30, y + 30)-(x + 30, y + 40), BrickColor
		PAINT (x + 28, y + 32), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 19) = 1 THEN
		LINE (x, y + 40)-(x + 7, y + 45), BrickColor
		LINE (x, y + 40)-(x, y + 60), BrickColor
		LINE (x, y + 60)-(x + 7, y + 45), BrickColor
		PAINT (x + 2, y + 45), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 20) = 1 THEN
		LINE (x, y + 60)-(x + 7, y + 45), BrickColor
		LINE (x + 7, y + 45)-(x + 15, y + 50), BrickColor
		LINE (x, y + 60)-(x + 15, y + 50), BrickColor
		PAINT (x + 9, y + 53), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 21) = 1 THEN
		LINE (x, y + 60)-(x + 15, y + 50), BrickColor
		LINE (x, y + 60)-(x + 15, y + 60), BrickColor
		LINE (x + 15, y + 50)-(x + 15, y + 60), BrickColor
		PAINT (x + 10, y + 58), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 22) = 1 THEN
		LINE (x + 15, y + 60)-(x + 15, y + 50), BrickColor
		LINE (x + 15, y + 50)-(x + 30, y + 60), BrickColor
		LINE (x + 15, y + 60)-(x + 30, y + 60), BrickColor
		PAINT (x + 17, y + 58), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 23) = 1 THEN
		LINE (x + 15, y + 50)-(x + 30, y + 60), BrickColor
		LINE (x + 15, y + 50)-(x + 23, y + 45), BrickColor
		LINE (x + 23, y + 45)-(x + 30, y + 60), BrickColor
		PAINT (x + 23, y + 50), FillColor, BrickColor
	END IF
	IF Board(CurBloX, CurBloY, 24) = 1 THEN
		LINE (x + 23, y + 45)-(x + 30, y + 40), BrickColor
		LINE (x + 23, y + 45)-(x + 30, y + 60), BrickColor
		LINE (x + 30, y + 40)-(x + 30, y + 60), BrickColor
		PAINT (x + 28, y + 50), FillColor, BrickColor
	END IF
END SUB

SUB DrawBoard
	CALL Grid
	FOR CurBloY = 1 TO 5
		FOR CurBloX = 1 TO 10
			StartPixX = CurBloX * 30
			StartPixY = CurBloY * 60
			CALL DrawBlock(StartPixX, StartPixY)
		NEXT CurBloX
	NEXT CurBloY
	CurBloX = 5
	CurBloY = 3
END SUB

SUB Editor
	LOCATE 5, 45
	PRINT "*        BRICK EDITOR         *"
	LOCATE 6, 45
	PRINT "                               "
	LOCATE 7, 45
	PRINT "Enter start viewpoint (1-24):"
	FOR y = 8 TO 20
		LOCATE y, 45
		PRINT "                                 "
	NEXT y
 
	LOCATE 8, 45
	INPUT CurTriNo
	LOCATE 10, 45
	PRINT "Enter brick to edit (1-209):"
	LOCATE 11, 45
	INPUT BrickNo

	LOCATE 15, 45
	PRINT "Expression codes:"
	LINE (450, 300)-(550, 300), 2
	LINE (450, 300)-(550, 250), 2
	LINE (550, 300)-(550, 250), 2
	LOCATE 17, 62
	PRINT "c"
	LOCATE 18, 70
	PRINT "a"
	LOCATE 20, 65
	PRINT "b"

ExistingExp:
	BrickExp = Bricks(BrickNo)
	CALL ClearBoard
	FOR y = 24 TO 28
		LOCATE y, 2
		PRINT "                                                               "
	NEXT y
	CALL ExpressionDecoder
	CALL DrawBoard
	LOCATE 22, 45
	PRINT "Brick Area:"; BrickArea
	LOCATE 24, 2
	PRINT "Existing expression for brick"; BrickNo; ":"
	LOCATE 25, 2
	PRINT Bricks(BrickNo)
	LOCATE 26, 2
	PRINT "Enter new expression for brick"; BrickNo; " (enter=copy existing exp.):"
	LOCATE 27, 2
	INPUT BrickExp
	IF BrickExp = "" THEN BrickExp = Bricks(BrickNo)
	LOCATE 22, 45
	PRINT "Brick Area:"; BrickArea
	CALL ClearBoard
	CALL ExpressionDecoder
	CALL DrawBoard
	

EditMenu:
	LOCATE 28, 2
	INPUT "a=accept, e=edit again, r=rotate/flip, m=main menu ", Choise$
	SELECT CASE Choise$
		CASE "a"
			GOTO ExpAccepted
		CASE "e"
			GOTO ReEdit
		CASE "r"
			GOTO RotaFlip
		CASE "m"
			GOTO EndEditor
	END SELECT
	GOTO EditMenu

ExpAccepted:
	Bricks(BrickNo) = BrickExp
	CALL SaveBricks
	FOR y = 24 TO 28
		LOCATE y, 2
		PRINT "                                                               "
	NEXT y
	LOCATE 24, 2
	PRINT "Design for brick"; BrickNo; "confirmed...Enter viewpoint for brick"; BrickNo + 1; ":"
	LOCATE 25, 2
	INPUT CurTriNo
	BrickNo = BrickNo + 1
	GOTO ExistingExp

ReEdit:
	FOR y = 24 TO 28
		LOCATE y, 2
		PRINT "                                                               "
	NEXT y
	LOCATE 24, 2
	PRINT "Current brick design expression for brick"; BrickNo; ":"
	LOCATE 25, 2
	PRINT BrickExp
	LOCATE 26, 2
	PRINT "Change design exp. to (enter=copy above exp.):"
	LOCATE 27, 2
	INPUT ChangedBrickExp
	IF ChangedBrickExp = "" THEN ChangedBrickExp = BrickExp
	BrickExp = ChangedBrickExp
	LOCATE 22, 45
	PRINT "Brick Area:"; BrickArea
	CALL ClearBoard
	CALL ExpressionDecoder
	CALL DrawBoard
	GOTO EditMenu

RotaFlip:
	CALL ClearBoard
	CurTriNo = CurTriNo + 1
	IF CurTriNo = 25 THEN CurTriNo = 1
	LOCATE 23, 45
	PRINT "Viewpoint:"; CurTriNo
	CALL ExpressionDecoder
	CALL DrawBoard
	GOTO EditMenu

EndEditor:

END SUB

SUB ExpressionDecoder

FOR x = 1 TO 10
	FOR y = 1 TO 5
		FOR t = 1 TO 24
			Board(x, y, t) = 0
		NEXT t
	NEXT y
NEXT x

CurBloX = 5
CurBloY = 3

Board(CurBloX, CurBloY, CurTriNo) = 1
n = 1
WHILE NOT MID$(BrickExp, n, 1) = "s"
	SELECT CASE MID$(BrickExp, n, 1)
		CASE "a"
			SELECT CASE CurTriNo
				CASE 1
					CurTriNo = 8
				CASE 2
					CurTriNo = 9
				CASE 3
					CurTriNo = 4
				CASE 4
					CurTriNo = 3
				CASE 5
					CurTriNo = 10
				CASE 6
					CurTriNo = 11
				CASE 7
					CurTriNo = 12
					CurBloX = CurBloX - 1
				CASE 8
					CurTriNo = 1
				CASE 9
					CurTriNo = 2
				CASE 10
					CurTriNo = 5
				CASE 11
					CurTriNo = 6
				CASE 12
					CurTriNo = 7
					CurBloX = CurBloX + 1
				CASE 13
					CurTriNo = 18
					CurBloX = CurBloX - 1
				CASE 14
					CurTriNo = 19
				CASE 15
					CurTriNo = 20
				CASE 16
					CurTriNo = 23
				CASE 17
					CurTriNo = 24
				CASE 18
					CurTriNo = 13
					CurBloX = CurBloX + 1
				CASE 19
					CurTriNo = 14
				CASE 20
					CurTriNo = 15
				CASE 21
					CurTriNo = 22
				CASE 22
					CurTriNo = 21
				CASE 23
					CurTriNo = 16
				CASE 24
					CurTriNo = 17
			END SELECT
		CASE "b"
			SELECT CASE CurTriNo
			 CASE 1
				 CurTriNo = 2
			 CASE 2
				 CurTriNo = 1
			 CASE 3
				 CurTriNo = 21
				 CurBloY = CurBloY - 1
			 CASE 4
				 CurTriNo = 22
				 CurBloY = CurBloY - 1
			 CASE 5
				 CurTriNo = 6
			 CASE 6
				 CurTriNo = 5
			 CASE 7
				 CurTriNo = 13
			 CASE 8
				 CurTriNo = 9
			 CASE 9
				 CurTriNo = 8
			 CASE 10
				 CurTriNo = 11
			 CASE 11
				 CurTriNo = 10
			 CASE 12
				 CurTriNo = 18
			 CASE 13
				 CurTriNo = 7
			 CASE 14
				 CurTriNo = 15
			 CASE 15
				 CurTriNo = 14
			 CASE 16
				 CurTriNo = 17
			 CASE 17
				 CurTriNo = 16
			 CASE 18
				 CurTriNo = 12
			 CASE 19
				 CurTriNo = 20
			 CASE 20
				 CurTriNo = 19
			 CASE 21
				 CurTriNo = 3
				 CurBloY = CurBloY + 1
			 CASE 22
				 CurTriNo = 4
				 CurBloY = CurBloY + 1
			 CASE 23
				 CurTriNo = 24
			 CASE 24
				 CurTriNo = 23
		END SELECT
		CASE "c":
			SELECT CASE CurTriNo
				CASE 1
					CurTriNo = 6
					CurBloX = CurBloX - 1
				CASE 2
					CurTriNo = 3
				CASE 3
					CurTriNo = 2
				CASE 4
					CurTriNo = 5
				CASE 5
					CurTriNo = 4
				CASE 6
					CurTriNo = 1
					CurBloX = CurBloX + 1
				CASE 7
					CurTriNo = 8
				CASE 8
					CurTriNo = 7
				CASE 9
					CurTriNo = 10
				CASE 10
					CurTriNo = 9
				CASE 11
					CurTriNo = 12
				CASE 12
					CurTriNo = 11
				CASE 13
					CurTriNo = 14
				CASE 14
					CurTriNo = 13
				CASE 15
					CurTriNo = 16
				CASE 16
					CurTriNo = 15
				CASE 17
					CurTriNo = 18
				CASE 18
					CurTriNo = 17
				CASE 19
					CurTriNo = 24
					CurBloX = CurBloX - 1
				CASE 20
					CurTriNo = 21
				CASE 21
					CurTriNo = 20
				CASE 22
					CurTriNo = 23
				CASE 23
					CurTriNo = 22
				CASE 24
					CurTriNo = 19
					CurBloX = CurBloX + 1
			END SELECT
	END SELECT
	Board(CurBloX, CurBloY, CurTriNo) = 1
	n = n + 1
WEND
 
END SUB

SUB Grid
	GridColor = 7
	LINE (30, 60)-(30, 360), GridColor
	LINE (330, 60)-(330, 360), GridColor
	FOR h = 60 TO 360 STEP 30
		LINE (30, h)-(330, h), GridColor
	NEXT h
	FOR y = 60 TO 300 STEP 60
		FOR x = 30 TO 300 STEP 30
			LINE (x, y)-(x + 30, y + 60), GridColor
			LINE (x + 30, y)-(x, y + 60), GridColor
		NEXT x
	NEXT y
END SUB

SUB LoadBricks
	OPEN "bricks.ete" FOR INPUT AS #1
	FOR BrickNo = 1 TO 209
		INPUT #1, Bricks(BrickNo)
	NEXT BrickNo
	CLOSE #1
END SUB

SUB MainMenu
	CLS
	LOCATE 2, 1
	PRINT "XTERNITY Project - Brick Editor                  (C) Allan Krogh Jensen 2000"
	LOCATE 3, 1
	PRINT "- Algorithms to solve the Eternity Puzzle"
 
	CALL Grid
 
	LOCATE 15, 50
	PRINT "Current Brick Collection"
	LOCATE 17, 50
	PRINT "Bricks File  : bricks.ete"
	LOCATE 18, 50
	PRINT "Total Bricks :"; BrickCounter
 
	LOCATE 5, 53
	PRINT "* MAIN MENU *"
	LOCATE 6, 50
	PRINT "1. Edit Bricks"
	LOCATE 7, 50
	PRINT "2. Save Bricks As File"
	LOCATE 8, 50
	PRINT "3. Brick Statistics"
	LOCATE 9, 50
	PRINT "4. Browse Bricks"
	LOCATE 10, 50
	PRINT "5. Quit"
	LOCATE 12, 50
	INPUT Choise

	SELECT CASE Choise
		CASE 1
			CALL Editor
		CASE 2
			CALL SaveBricks
		CASE 3
			CALL Statistics
		CASE 4
			CALL BrickBrowser
		CASE 5
			CLS
			END
	END SELECT
END SUB

SUB SaveBricks
	LOCATE 24, 2
	PRINT "Saving bricks..."
	OPEN "bricks.ete" FOR OUTPUT AS #1
	FOR BrickNo = 1 TO 209
		WRITE #1, Bricks(BrickNo)
	NEXT BrickNo
	CLOSE #1
	LOCATE 24, 2
	FOR d = 1 TO 20000
	NEXT d
	PRINT "                "
END SUB

SUB Statistics
	LOCATE 24, 2
	PRINT "Calculating total brick area..."
 
	TotalBrickArea = 0
	FOR BrickNo = 1 TO 209
		LOCATE 24, 45
		PRINT INT((BrickNo / 209) * 100); "%"
		BrickExp = Bricks(BrickNo)
		CurTriNo = 1
		CALL ExpressionDecoder
		TotalBrickArea = TotalBrickArea + BrickArea
	NEXT BrickNo
 
	LOCATE 24, 2
	PRINT "Total brick area for the Eternity Puzzle, for"; BrickCounter; "of 209 bricks..."
	LOCATE 26, 2
	PRINT "Total Brick Area:   "; TotalBrickArea; " st = "; TotalBrickArea / 6; " bt"
	LOCATE 28, 2
	INPUT "Press enter to return to main menu.", w

	FOR x = 1 TO 80
		FOR y = 24 TO 28
			LOCATE y, x
			PRINT " "
		NEXT y
	NEXT x
END SUB

