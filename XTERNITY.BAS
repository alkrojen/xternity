DECLARE SUB Grid ()
DECLARE SUB BrickBrowser ()
DECLARE SUB ChaoticEvolutionAlgorithm ()
DECLARE SUB DisplayBackground ()
DECLARE SUB DisplayBrickNums ()
DECLARE SUB DisplayPuzzle ()
DECLARE SUB DrawBoard ()
DECLARE SUB DrawBlock (StartPixX, StartPixY)
DECLARE SUB DrawDigit (PixX, PixY, Digit)
DECLARE SUB DrawNumber (PixX, PixY, BrickNo)
DECLARE SUB BrickBorders ()
DECLARE SUB BrickAlgorithm (BrickNo)
DECLARE SUB ExpressionDecoder (BrickNo)
DECLARE SUB FillBlock (x, y)
DECLARE SUB Frame ()
DECLARE SUB PlaceBrick (x, y, t, BrickNo)
DECLARE SUB RemoveBrick (BrickNo)
DECLARE SUB LoadBricks ()
DECLARE SUB LoadPuzzle ()
DECLARE SUB SavePuzzle ()
DECLARE SUB ShutDown ()
DECLARE SUB StartUp ()
DECLARE SUB Statistics ()
DECLARE FUNCTION BrickCounter ()
DECLARE FUNCTION BrickArea ()
DECLARE FUNCTION BrickOnBoard (BrickNo)
DECLARE FUNCTION Contentness (BrickNo)
DECLARE FUNCTION FrameArea ()
DECLARE FUNCTION NearPlacedBrick (BrickNo)
DECLARE FUNCTION PuzzleAccomplished ()
DECLARE FUNCTION PlacedBricks ()
DECLARE FUNCTION SpaceForBrick (x, y, t, BrickNo)

SCREEN 12

DIM SHARED BrickExp AS STRING
DIM SHARED ChangedBrickExp AS STRING
DIM SHARED Board(29, 17, 24) AS INTEGER
DIM SHARED CurTriNo AS INTEGER
DIM SHARED CurBloX AS INTEGER
DIM SHARED CurBloY AS INTEGER
DIM SHARED Bricks(210) AS STRING
DIM SHARED BrickColor AS INTEGER
DIM SHARED FillColor AS INTEGER
DIM SHARED PuzzleX(210) AS INTEGER
DIM SHARED PuzzleY(210) AS INTEGER
DIM SHARED PuzzleT(210) AS INTEGER
DIM SHARED TimeCounter AS INTEGER
DIM SHARED TurnCounter AS LONG
DIM SHARED ContentnessDemand AS INTEGER
DIM SHARED BricksPlacedMax AS INTEGER
DIM SHARED MaxMoves AS INTEGER
DIM SHARED SigmaContentness AS INTEGER
DIM SHARED BorderTriangles AS INTEGER
DIM SHARED TurnMoves AS LONG

RANDOMIZE 2810

CALL StartUp

CALL ChaoticEvolutionAlgorithm

CALL ShutDown

SUB BrickAlgorithm (ActiveBrick)
	Moves = 0
 
	IF NOT ActiveBrick = 34 THEN
	 
		REM Brick placed on board?
		IF BrickOnBoard(ActiveBrick) = 0 THEN
JumpOnBoard:
			x = INT(RND * 28)
			y = INT(RND * 16)
			t = INT(RND * 24)
			IF x = 0 THEN x = 1
			IF y = 0 THEN y = 1
			IF t = 0 THEN t = 1
			IF NOT Board(x, y, t) = 0 THEN GOTO JumpOnBoard
			Moves = 1
			GOTO CheckSpace
		ELSE
			x = PuzzleX(ActiveBrick)
			y = PuzzleY(ActveBrick)
			t = PuzzleT(ActiveBrick)
			GOTO CheckContentness
		END IF
 
CheckContentness:
		C = Contentness(ActiveBrick)
		LOCATE 20, 67
		PRINT C
		IF C >= ContentnessDemand THEN
			GOTO EndBA
		ELSE
			x = PuzzleX(ActiveBrick)
			y = PuzzleY(ActiveBrick)
			t = PuzzleT(ActiveBrick)
			CALL RemoveBrick(ActiveBrick)
			GOTO MoveBrick
		END IF

MoveBrick:
		t = t + 1
		Moves = Moves + 1
		LOCATE 15, 67
		PRINT Moves
		IF t = 25 THEN t = 1: x = x + 1
		IF x = 28 THEN x = 2: y = y + 1
		IF y = 17 THEN y = 1
		GOTO CheckSpace

CheckSpace:
		IF SpaceForBrick(x, y, t, ActiveBrick) = 1 THEN
			CALL PlaceBrick(x, y, t, ActiveBrick)
			GOTO CheckContentness
		ELSE
			IF Moves >= MaxMoves THEN
				CALL RemoveBrick(ActiveBrick)
				GOTO EndBA
			ELSE
				GOTO MoveBrick
			END IF
		END IF
 
	END IF

EndBA:
	LOCATE 16, 67
	PRINT PuzzleX(ActiveBrick)
	LOCATE 17, 67
	PRINT PuzzleY(ActiveBrick)
	LOCATE 18, 67
	PRINT PuzzleT(ActiveBrick)
	TurnMoves = TurnMoves + Moves
END SUB

SUB BrickBorders
	BorderColor = 1
	x = y = 0
	FOR by = 1 TO 16
		FOR bx = 1 TO 28
		 x = bx * 12
		 y = by * 24
		 IF NOT Board(bx, by, 1) = Board(bx - 1, by, 6) THEN LINE (x, y)-(x, y + 8), BorderColor
		 IF NOT Board(bx, by, 1) = Board(bx, by, 8) THEN LINE (x, y + 8)-(x + 3, y + 6), BorderColor
		 IF NOT Board(bx, by, 1) = Board(bx, by, 2) THEN LINE (x, y)-(x + 3, y + 6), BorderColor
		 IF NOT Board(bx, by, 2) = Board(bx, by, 9) THEN LINE (x + 3, y + 6)-(x + 6, y + 4), BorderColor
		 IF NOT Board(bx, by, 2) = Board(bx, by, 3) THEN LINE (x, y)-(x + 6, y + 4), BorderColor
		 IF NOT Board(bx, by, 3) = Board(bx, by - 1, 21) THEN LINE (x, y)-(x + 6, y), BorderColor
		 IF NOT Board(bx, by, 3) = Board(bx, by, 4) THEN LINE (x + 6, y)-(x + 6, y + 4), BorderColor
		 IF NOT Board(bx, by, 4) = Board(bx, by - 1, 22) THEN LINE (x + 6, y)-(x + 12, y), BorderColor
		 IF NOT Board(bx, by, 4) = Board(bx, by, 5) THEN LINE (x + 6, y + 4)-(x + 12, y), BorderColor
		 IF NOT Board(bx, by, 5) = Board(bx, by, 10) THEN LINE (x + 6, y + 4)-(x + 9, y + 6), BorderColor
		 IF NOT Board(bx, by, 5) = Board(bx, by, 6) THEN LINE (x + 9, y + 6)-(x + 12, y), BorderColor
		 IF NOT Board(bx, by, 6) = Board(bx + 1, by, 1) THEN LINE (x + 12, y)-(x + 12, y + 8), BorderColor
		 IF NOT Board(bx, by, 6) = Board(bx, by, 11) THEN LINE (x + 9, y + 6)-(x + 12, y + 8), BorderColor
		 IF NOT Board(bx, by, 7) = Board(bx - 1, by, 12) THEN LINE (x, y + 8)-(x, y + 12), BorderColor
		 IF NOT Board(bx, by, 7) = Board(bx, by, 13) THEN LINE (x, y + 12)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 7) = Board(bx, by, 8) THEN LINE (x, y + 8)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 8) = Board(bx, by, 9) THEN LINE (x + 3, y + 6)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 9) = Board(bx, by, 10) THEN LINE (x + 6, y + 4)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 10) = Board(bx, by, 11) THEN LINE (x + 9, y + 6)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 11) = Board(bx, by, 12) THEN LINE (x + 6, y + 12)-(x + 12, y + 8), BorderColor
		 IF NOT Board(bx, by, 12) = Board(bx + 1, by, 7) THEN LINE (x + 12, y + 8)-(x + 12, y + 12), BorderColor
		 IF NOT Board(bx, by, 12) = Board(bx, by, 18) THEN LINE (x + 6, y + 12)-(x + 12, y + 12), BorderColor
		 IF NOT Board(bx, by, 13) = Board(bx - 1, by, 18) THEN LINE (x, y + 12)-(x, y + 16), BorderColor
		 IF NOT Board(bx, by, 13) = Board(bx, by, 14) THEN LINE (x, y + 16)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 14) = Board(bx, by, 19) THEN LINE (x, y + 16)-(x + 3, y + 18), BorderColor
		 IF NOT Board(bx, by, 14) = Board(bx, by, 15) THEN LINE (x + 3, y + 18)-(x + 6, y + 12), BorderColor
		 IF NOT Board(bx, by, 15) = Board(bx, by, 20) THEN LINE (x + 3, y + 18)-(x + 6, y + 20), BorderColor
		 IF NOT Board(bx, by, 15) = Board(bx, by, 16) THEN LINE (x + 6, y + 12)-(x + 6, y + 20), BorderColor
		 IF NOT Board(bx, by, 16) = Board(bx, by, 23) THEN LINE (x + 6, y + 20)-(x + 9, y + 18), BorderColor
		 IF NOT Board(bx, by, 16) = Board(bx, by, 17) THEN LINE (x + 6, y + 12)-(x + 9, y + 18), BorderColor
		 IF NOT Board(bx, by, 17) = Board(bx, by, 24) THEN LINE (x + 9, y + 18)-(x + 12, y + 16), BorderColor
		 IF NOT Board(bx, by, 17) = Board(bx, by, 18) THEN LINE (x + 6, y + 12)-(x + 12, y + 16), BorderColor
		 IF NOT Board(bx, by, 18) = Board(bx + 1, by, 13) THEN LINE (x + 12, y + 12)-(x + 12, y + 16), BorderColor
		 IF NOT Board(bx, by, 19) = Board(bx - 1, by, 24) THEN LINE (x, y + 16)-(x, y + 24), BorderColor
		 IF NOT Board(bx, by, 19) = Board(bx, by, 20) THEN LINE (x + 3, y + 18)-(x, y + 24), BorderColor
		 IF NOT Board(bx, by, 20) = Board(bx, by, 21) THEN LINE (x, y + 24)-(x + 6, y + 20), BorderColor
		 IF NOT Board(bx, by, 21) = Board(bx, by + 1, 3) THEN LINE (x, y + 24)-(x + 6, y + 24), BorderColor
		 IF NOT Board(bx, by, 21) = Board(bx, by, 22) THEN LINE (x + 6, y + 20)-(x + 6, y + 24), BorderColor
		 IF NOT Board(bx, by, 22) = Board(bx, by + 1, 4) THEN LINE (x + 6, y + 24)-(x + 12, y + 24), BorderColor
		 IF NOT Board(bx, by, 22) = Board(bx, by, 23) THEN LINE (x + 6, y + 20)-(x + 12, y + 24), BorderColor
		 IF NOT Board(bx, by, 23) = Board(bx, by, 24) THEN LINE (x + 9, y + 18)-(x + 12, y + 24), BorderColor
		 IF NOT Board(bx, by, 24) = Board(bx + 1, by, 19) THEN LINE (x + 12, y + 16)-(x + 12, y + 24), BorderColor
		NEXT bx
	NEXT by

END SUB

FUNCTION BrickCounter
	C = 0
	FOR n = 1 TO 210
		IF LEN(Bricks(n)) > 1 THEN C = C + 1
	NEXT n
	BrickCounter = C
END FUNCTION

FUNCTION BrickOnBoard (BrickNo)
	t = 0
	IF NOT PuzzleX(BrickNo) = 0 THEN t = 1
	BrickOnBoard = t
END FUNCTION

SUB ChaoticEvolutionAlgorithm
	NewHours = OldHours = VAL(TIME$)
	MaxMoves = 5000
	OldSeconds = VAL(MID$(TIME$, 7, 2)) + 60 * VAL(MID$(TIME$, 4, 2))
	CDStep = 1

	WHILE PuzzleAccomplished = 0 AND INKEY$ = ""
	 
		REM Placed Bricks Status
		BricksPlacedNow = PlacedBricks
		LOCATE 4, 67
		PRINT BricksPlacedNow
		LOCATE 4, 74
		PRINT USING "###.#"; (BricksPlacedNow / 209) * 100
		IF BricksPlacedNow > BricksPlacedMax THEN
			BricksPlacedMax = BricksPlacedNow
			LOCATE 5, 67
			PRINT BricksPlacedMax
			LOCATE 5, 74
			PRINT USING "###.#"; (BricksPlacedMax / 209) * 100
		END IF
	 
		REM Time Counter & Auto Save
		NewHours = VAL(TIME$)
		IF NOT NewHours = OldHours THEN
			OldHours = NewHours
			TimeCounter = TimeCounter + 1
			CALL SavePuzzle
			LOCATE 6, 67
			PRINT TimeCounter
		END IF
	 
		REM Turn Counter
		TurnCounter = TurnCounter + 1
		IF TurnCounter = 4000000000# THEN
			TurnCounter = 0
			LOCATE 7, 67
			PRINT "           "
		END IF
		LOCATE 7, 67
		PRINT TurnCounter

		REM Display Update
		CALL DisplayPuzzle
	 
		REM ContentnessDemand Setting
		IF BricksPlacedNow > BricksPlacedOld THEN
			ContentnessDemand = ContentnessDemand + CDStep
			CDStep = 1
		END IF
		IF BricksPlacedNow < BricksPlacedOld THEN
			ContentnessDemand = ContentnessDemand - CDStep
			CDStep = 1
		END IF
		IF BricksPlacedNow = BricksPlacedOld THEN
			ContentnessDemand = ContentnessDemand + CDStep
			CDStep = CDStep + 1
		END IF

		IF ContentnessDemand < 1 THEN ContentnessDemand = 1
		IF ContentnessDemand > 100 THEN ContentnessDemand = 100
		LOCATE 9, 67
		PRINT ContentnessDemand
		BricksPlacedOld = BricksPlacedNow

		REM Individual Brick Multiplexing
		FOR ActiveBrick = 1 TO 209
			LOCATE 14, 67
			PRINT ActiveBrick
			CALL BrickAlgorithm(ActiveBrick)
		NEXT ActiveBrick

		REM Processing Speed
		NewSeconds = VAL(MID$(TIME$, 7, 2)) + 60 * VAL(MID$(TIME$, 4, 2))
		TurnTime = ABS(NewSeconds - OldSeconds)
		LOCATE 10, 67
		PRINT TurnTime
		LOCATE 11, 67
		PRINT TurnMoves
		IF TurnTime = 0 THEN TurnTime = 1
		Speed = INT(TurnMoves / TurnTime)
		LOCATE 8, 67
		PRINT Speed
		OldSecond = NewSecond
		TurnMoves = 0

	WEND

END SUB

FUNCTION Contentness (BrickNo)
SigmaContentness = 0
BorderTriangles = 0

CurBloX = PuzzleX(BrickNo)
CurBloY = PuzzleY(BrickNo)
CurTriNo = PuzzleT(BrickNo)
BrickExp = Bricks(BrickNo)

REM Measuring Contentness In Header Triangle
REM NaborA
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 8
				CASE 2
					t = 9
				CASE 3
					t = 4
				CASE 4
					t = 3
				CASE 5
					t = 10
				CASE 6
					t = 11
				CASE 7
					t = 12
					x = x - 1
				CASE 8
					t = 1
				CASE 9
					t = 2
				CASE 10
					t = 5
				CASE 11
					t = 6
				CASE 12
					t = 7
					x = x + 1
				CASE 13
					t = 18
					x = x - 1
				CASE 14
					t = 19
				CASE 15
					t = 20
				CASE 16
					t = 23
				CASE 17
					t = 24
				CASE 18
					t = 13
					x = x + 1
				CASE 19
					t = 14
				CASE 20
					t = 15
				CASE 21
					t = 22
				CASE 22
					t = 21
				CASE 23
					t = 16
				CASE 24
					t = 17
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborB
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
			 CASE 1
				 t = 2
			 CASE 2
				 t = 1
			 CASE 3
				 t = 21
				 y = y - 1
			 CASE 4
				 t = 22
				 y = y - 1
			 CASE 5
				 t = 6
			 CASE 6
				 t = 5
			 CASE 7
				 t = 13
			 CASE 8
				 t = 9
			 CASE 9
				 t = 8
			 CASE 10
				 t = 11
			 CASE 11
				 t = 10
			 CASE 12
				 t = 18
			 CASE 13
				 t = 7
			 CASE 14
				 t = 15
			 CASE 15
				 t = 14
			 CASE 16
				 t = 17
			 CASE 17
				 t = 16
			 CASE 18
				 t = 12
			 CASE 19
				 t = 20
			 CASE 20
				 t = 19
			 CASE 21
				 t = 3
				 y = y + 1
			 CASE 22
				 t = 4
				 y = y + 1
			 CASE 23
				 t = 24
			 CASE 24
				 t = 23
		END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborC
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 6
					x = x - 1
				CASE 2
					t = 3
				CASE 3
					t = 2
				CASE 4
					t = 5
				CASE 5
					t = 4
				CASE 6
					t = 1
					x = x + 1
				CASE 7
					t = 8
				CASE 8
					t = 7
				CASE 9
					t = 10
				CASE 10
					t = 9
				CASE 11
					t = 12
				CASE 12
					t = 11
				CASE 13
					t = 14
				CASE 14
					t = 13
				CASE 15
					t = 16
				CASE 16
					t = 15
				CASE 17
					t = 18
				CASE 18
					t = 17
				CASE 19
					t = 24
					x = x - 1
				CASE 20
					t = 21
				CASE 21
					t = 20
				CASE 22
					t = 23
				CASE 23
					t = 22
				CASE 24
					t = 19
					x = x + 1
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM Measuring Contentness In The Other Triangles
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
 
REM NaborA
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 8
				CASE 2
					t = 9
				CASE 3
					t = 4
				CASE 4
					t = 3
				CASE 5
					t = 10
				CASE 6
					t = 11
				CASE 7
					t = 12
					x = x - 1
				CASE 8
					t = 1
				CASE 9
					t = 2
				CASE 10
					t = 5
				CASE 11
					t = 6
				CASE 12
					t = 7
					x = x + 1
				CASE 13
					t = 18
					x = x - 1
				CASE 14
					t = 19
				CASE 15
					t = 20
				CASE 16
					t = 23
				CASE 17
					t = 24
				CASE 18
					t = 13
					x = x + 1
				CASE 19
					t = 14
				CASE 20
					t = 15
				CASE 21
					t = 22
				CASE 22
					t = 21
				CASE 23
					t = 16
				CASE 24
					t = 17
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborB
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
			 CASE 1
				 t = 2
			 CASE 2
				 t = 1
			 CASE 3
				 t = 21
				 y = y - 1
			 CASE 4
				 t = 22
				 y = y - 1
			 CASE 5
				 t = 6
			 CASE 6
				 t = 5
			 CASE 7
				 t = 13
			 CASE 8
				 t = 9
			 CASE 9
				 t = 8
			 CASE 10
				 t = 11
			 CASE 11
				 t = 10
			 CASE 12
				 t = 18
			 CASE 13
				 t = 7
			 CASE 14
				 t = 15
			 CASE 15
				 t = 14
			 CASE 16
				 t = 17
			 CASE 17
				 t = 16
			 CASE 18
				 t = 12
			 CASE 19
				 t = 20
			 CASE 20
				 t = 19
			 CASE 21
				 t = 3
				 y = y + 1
			 CASE 22
				 t = 4
				 y = y + 1
			 CASE 23
				 t = 24
			 CASE 24
				 t = 23
		END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborC
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 6
					x = x - 1
				CASE 2
					t = 3
				CASE 3
					t = 2
				CASE 4
					t = 5
				CASE 5
					t = 4
				CASE 6
					t = 1
					x = x + 1
				CASE 7
					t = 8
				CASE 8
					t = 7
				CASE 9
					t = 10
				CASE 10
					t = 9
				CASE 11
					t = 12
				CASE 12
					t = 11
				CASE 13
					t = 14
				CASE 14
					t = 13
				CASE 15
					t = 16
				CASE 16
					t = 15
				CASE 17
					t = 18
				CASE 18
					t = 17
				CASE 19
					t = 24
					x = x - 1
				CASE 20
					t = 21
				CASE 21
					t = 20
				CASE 22
					t = 23
				CASE 23
					t = 22
				CASE 24
					t = 19
					x = x + 1
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT
 

	n = n + 1
WEND
LOCATE 19, 67
PRINT BorderTriangles
Contentness = INT((SigmaContentness / BorderTriangles) * 100)
END FUNCTION

SUB DisplayBackground
	LOCATE 1, 2
	PRINT "XTERNITY Project - Eternity Puzzle Solver           (C) Allan Krogh Jensen 2000"
	LOCATE 28, 2
	PRINT "Chaotic Evolution Algorithm (CEA)               Press any key to break and save"
	LOCATE 3, 47
	PRINT "Puzzle Status:"
	LOCATE 4, 48
	PRINT "Bricks Placed Now :      =      %"
	LOCATE 5, 48
	PRINT "Bricks Placed Max :      =      %"
	LOCATE 6, 48
	PRINT "Time Elapsed      :         hours"
	LOCATE 7, 48
	PRINT "Turns Elapsed     :              "
	LOCATE 8, 48
	PRINT "Processing Speed  :       moves/s"
	LOCATE 9, 48
	PRINT "Contentness Demand:              "
	LOCATE 10, 48
	PRINT "Turn Time         :             s"
	LOCATE 11, 48
	PRINT "Turn Moves        :              "
	LOCATE 13, 47
	PRINT "Brick Status:"
	LOCATE 14, 48
	PRINT "Active Brick      :"
	LOCATE 15, 48
	PRINT "Moves             :"
	LOCATE 16, 48
	PRINT "Possition X       :"
	LOCATE 17, 48
	PRINT "Possition Y       :"
	LOCATE 18, 48
	PRINT "Header Triangle   :"
	LOCATE 19, 48
	PRINT "Triangle Borders  :"
	LOCATE 20, 48
	PRINT "Contentness       :"
END SUB

SUB DisplayBrickNums
	FOR BrickNo = 1 TO 209
		IF NOT PuzzleX(BrickNo) = 0 THEN
			x = PuzzleX(BrickNo) * 12
			y = PuzzleY(BrickNo) * 24
			SELECT CASE PuzzleT(BrickNo)
				CASE 1
					x = x + 1
					y = y + 5
				CASE 2
					x = x + 3
					y = y + 4
				CASE 3
					x = x + 5
					y = y + 2
				CASE 4
					x = x + 8
					y = y + 2
				CASE 5
					x = x + 9
					y = y + 4
				CASE 6
					x = x + 11
					y = y + 6
				CASE 7
					x = x + 1
					y = y + 10
				CASE 8
					x = x + 3
					y = y + 8
				CASE 9
					x = x + 5
					y = y + 7
				CASE 10
					x = x + 8
					y = y + 7
				CASE 11
					x = x + 10
					y = y + 8
				CASE 12
					x = x + 10
					y = y + 11
				CASE 13
					x = x + 3
					y = y + 13
				CASE 14
					x = x + 3
					y = y + 16
				CASE 15
					x = x + 5
					y = y + 17
				CASE 16
					x = x + 7
					y = y + 17
				CASE 17
					x = x + 10
					y = y + 16
				CASE 18
					x = x + 10
					y = y + 13
				CASE 19
					x = x + 2
					y = y + 19
				CASE 20
					x = x + 4
					y = y + 20
				CASE 21
					x = x + 4
					y = y + 22
				CASE 22
					x = x + 7
					y = y + 22
				CASE 23
					x = x + 9
					y = y + 20
				CASE 24
					x = x + 10
					y = y + 19
			END SELECT
			CALL DrawNumber(x, y, BrickNo)
		END IF
	NEXT BrickNo
END SUB

SUB DisplayPuzzle
	LINE (12, 24)-(348, 408), 7, BF
	CALL DrawBoard
	CALL BrickBorders
	LINE (12, 24)-(348, 408), 1, B
	CALL DisplayBrickNums
END SUB

SUB DrawBlock (x, y)
	IF NOT Board(CurBloX, CurBloY, 1) = 0 THEN
		LINE (x, y)-(x, y + 8), BrickColor
		LINE (x, y)-(x + 3, y + 6), BrickColor
		LINE (x, y + 8)-(x + 3, y + 6), BrickColor
		PAINT (x + 1, y + 4), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 2) = 0 THEN
		LINE (x, y)-(x + 3, y + 6), BrickColor
		LINE (x, y)-(x + 6, y + 4), BrickColor
		LINE (x + 3, y + 6)-(x + 6, y + 4), BrickColor
		PAINT (x + 3, y + 4), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 3) = 0 THEN
		LINE (x, y)-(x + 6, y), BrickColor
		LINE (x, y)-(x + 6, y + 4), BrickColor
		LINE (x + 6, y)-(x + 6, y + 4), BrickColor
		PAINT (x + 3, y + 1), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 4) = 0 THEN
		LINE (x + 6, y)-(x + 12, y), BrickColor
		LINE (x + 6, y)-(x + 6, y + 4), BrickColor
		LINE (x + 6, y + 4)-(x + 12, y), BrickColor
		PAINT (x + 7, y + 1), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 5) = 0 THEN
		LINE (x + 6, y + 4)-(x + 12, y), BrickColor
		LINE (x + 6, y + 4)-(x + 9, y + 6), BrickColor
		LINE (x + 9, y + 6)-(x + 12, y), BrickColor
		PAINT (x + 9, y + 4), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 6) = 0 THEN
		LINE (x + 9, y + 6)-(x + 12, y), BrickColor
		LINE (x + 9, y + 6)-(x + 12, y + 8), BrickColor
		LINE (x + 12, y)-(x + 12, y + 8), BrickColor
		PAINT (x + 11, y + 6), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 7) = 0 THEN
		LINE (x, y + 8)-(x, y + 12), BrickColor
		LINE (x, y + 8)-(x + 6, y + 12), BrickColor
		LINE (x, y + 12)-(x + 6, y + 12), BrickColor
		PAINT (x + 2, y + 11), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 8) = 0 THEN
		LINE (x, y + 8)-(x + 3, y + 6), BrickColor
		LINE (x, y + 8)-(x + 6, y + 12), BrickColor
		LINE (x + 3, y + 6)-(x + 6, y + 12), BrickColor
		PAINT (x + 3, y + 9), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 9) = 0 THEN
		LINE (x + 3, y + 6)-(x + 6, y + 12), BrickColor
		LINE (x + 3, y + 6)-(x + 6, y + 4), BrickColor
		LINE (x + 6, y + 4)-(x + 6, y + 12), BrickColor
		PAINT (x + 5, y + 8), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 10) = 0 THEN
		LINE (x + 6, y + 4)-(x + 6, y + 12), BrickColor
		LINE (x + 6, y + 4)-(x + 9, y + 6), BrickColor
		LINE (x + 9, y + 6)-(x + 6, y + 12), BrickColor
		PAINT (x + 7, y + 8), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 11) = 0 THEN
		LINE (x + 9, y + 6)-(x + 6, y + 12), BrickColor
		LINE (x + 9, y + 6)-(x + 12, y + 8), BrickColor
		LINE (x + 12, y + 8)-(x + 6, y + 12), BrickColor
		PAINT (x + 9, y + 8), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 12) = 0 THEN
		LINE (x + 12, y + 8)-(x + 6, y + 12), BrickColor
		LINE (x + 12, y + 8)-(x + 12, y + 12), BrickColor
		LINE (x + 6, y + 12)-(x + 12, y + 12), BrickColor
		PAINT (x + 10, y + 10), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 13) = 0 THEN
		LINE (x, y + 12)-(x + 6, y + 12), BrickColor
		LINE (x, y + 12)-(x, y + 16), BrickColor
		LINE (x, y + 16)-(x + 6, y + 12), BrickColor
		PAINT (x + 1, y + 14), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 14) = 0 THEN
		LINE (x, y + 16)-(x + 6, y + 12), BrickColor
		LINE (x, y + 16)-(x + 3, y + 18), BrickColor
		LINE (x + 3, y + 18)-(x + 6, y + 12), BrickColor
		PAINT (x + 3, y + 16), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 15) = 0 THEN
		LINE (x + 3, y + 18)-(x + 6, y + 12), BrickColor
		LINE (x + 3, y + 18)-(x + 6, y + 20), BrickColor
		LINE (x + 6, y + 20)-(x + 6, y + 12), BrickColor
		PAINT (x + 5, y + 16), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 16) = 0 THEN
		LINE (x + 6, y + 20)-(x + 6, y + 12), BrickColor
		LINE (x + 6, y + 20)-(x + 9, y + 18), BrickColor
		LINE (x + 9, y + 18)-(x + 6, y + 12), BrickColor
		PAINT (x + 7, y + 16), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 17) = 0 THEN
		LINE (x + 9, y + 18)-(x + 6, y + 12), BrickColor
		LINE (x + 9, y + 18)-(x + 12, y + 16), BrickColor
		LINE (x + 12, y + 16)-(x + 6, y + 12), BrickColor
		PAINT (x + 9, y + 16), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 18) = 0 THEN
		LINE (x + 12, y + 12)-(x + 6, y + 12), BrickColor
		LINE (x + 6, y + 12)-(x + 12, y + 16), BrickColor
		LINE (x + 12, y + 12)-(x + 12, y + 16), BrickColor
		PAINT (x + 10, y + 13), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 19) = 0 THEN
		LINE (x, y + 16)-(x + 3, y + 18), BrickColor
		LINE (x, y + 16)-(x, y + 24), BrickColor
		LINE (x, y + 24)-(x + 3, y + 18), BrickColor
		PAINT (x + 1, y + 18), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 20) = 0 THEN
		LINE (x, y + 24)-(x + 3, y + 18), BrickColor
		LINE (x + 3, y + 18)-(x + 6, y + 20), BrickColor
		LINE (x, y + 24)-(x + 6, y + 20), BrickColor
		PAINT (x + 4, y + 20), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 21) = 0 THEN
		LINE (x, y + 24)-(x + 6, y + 20), BrickColor
		LINE (x, y + 24)-(x + 6, y + 24), BrickColor
		LINE (x + 6, y + 20)-(x + 6, y + 24), BrickColor
		PAINT (x + 4, y + 23), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 22) = 0 THEN
		LINE (x + 6, y + 24)-(x + 6, y + 20), BrickColor
		LINE (x + 6, y + 20)-(x + 12, y + 24), BrickColor
		LINE (x + 6, y + 24)-(x + 12, y + 24), BrickColor
		PAINT (x + 7, y + 23), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 23) = 0 THEN
		LINE (x + 6, y + 20)-(x + 12, y + 24), BrickColor
		LINE (x + 6, y + 20)-(x + 9, y + 18), BrickColor
		LINE (x + 9, y + 18)-(x + 12, y + 24), BrickColor
		PAINT (x + 8, y + 20), FillColor, BrickColor
	END IF
	IF NOT Board(CurBloX, CurBloY, 24) = 0 THEN
		LINE (x + 9, y + 18)-(x + 12, y + 16), BrickColor
		LINE (x + 9, y + 18)-(x + 12, y + 24), BrickColor
		LINE (x + 12, y + 16)-(x + 12, y + 24), BrickColor
		PAINT (x + 11, y + 19), FillColor, BrickColor
	END IF
END SUB

SUB DrawBoard
	x = 0
	y = 0
	CALL Grid
	FOR CurBloY = 1 TO 16
		FOR CurBloX = 1 TO 28
			StartPixX = CurBloX * 12
			StartPixY = CurBloY * 24
			CALL DrawBlock(StartPixX, StartPixY)
		NEXT CurBloX
	NEXT CurBloY
END SUB

SUB DrawDigit (x, y, Digit)
	NumberColor = 4
	SELECT CASE Digit
		CASE 1
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
		CASE 2
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x + 2, y)-(x + 2, y + 2), NumberColor
			LINE (x + 2, y + 2)-(x, y + 2), NumberColor
			LINE (x, y + 2)-(x, y + 4), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
		CASE 3
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
		CASE 4
			LINE (x, y)-(x, y + 2), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
		CASE 5
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x, y)-(x, y + 2), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x + 2, y + 2)-(x + 2, y + 4), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
		CASE 6
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x, y)-(x, y + 4), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
			LINE (x + 2, y + 2)-(x + 2, y + 4), NumberColor
		CASE 7
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
		CASE 8
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
			LINE (x, y)-(x, y + 4), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
		CASE 9
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x, y + 2)-(x + 2, y + 2), NumberColor
			LINE (x, y)-(x, y + 2), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
		CASE 0
			LINE (x, y)-(x + 2, y), NumberColor
			LINE (x, y)-(x, y + 4), NumberColor
			LINE (x, y + 4)-(x + 2, y + 4), NumberColor
			LINE (x + 2, y)-(x + 2, y + 4), NumberColor
	END SELECT
END SUB

SUB DrawNumber (x, y, BrickNo)
	CrossColor = 4
	LINE (x - 1, y - 1)-(x + 1, y + 1), CrossColor
	LINE (x - 1, y + 1)-(x + 1, y - 1), CrossColor
	SELECT CASE BrickNo
		CASE IS < 10
			CALL DrawDigit(x + 3, y - 2, BrickNo)
		CASE IS < 100
			Tens = INT(BrickNo / 10)
			CALL DrawDigit(x + 3, y - 2, Tens)
			Ones = BrickNo - INT(BrickNo / 10) * 10
			CALL DrawDigit(x + 7, y - 2, Ones)
		CASE IS < 1000
			Hunds = INT(BrickNo / 100)
			CALL DrawDigit(x + 3, y - 2, Hunds)
			Tens = INT((BrickNo - INT(BrickNo / 100) * 100) / 10)
			CALL DrawDigit(x + 7, y - 2, Tens)
			Ones = BrickNo - 100 * Hunds - 10 * Tens
			CALL DrawDigit(x + 11, y - 2, Ones)
	END SELECT
END SUB

SUB ExpressionDecoder (BrickNo)

Board(CurBloX, CurBloY, CurTriNo) = BrickNo
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
	Board(CurBloX, CurBloY, CurTriNo) = BrickNo
	n = n + 1
WEND
 
END SUB

SUB FillBlock (x, y)
	FOR t = 1 TO 24
		Board(x, y, t) = 210
	NEXT t
END SUB

SUB Frame
	FOR y = 1 TO 16
		CALL FillBlock(1, y)
		CALL FillBlock(28, y)
	NEXT y
	FOR y = 1 TO 5
		CALL FillBlock(2, y)
		CALL FillBlock(27, y)
	NEXT y
	FOR y = 12 TO 16
		CALL FillBlock(2, y)
		CALL FillBlock(27, y)
	NEXT y
	FOR y = 1 TO 4
		CALL FillBlock(3, y)
		CALL FillBlock(26, y)
	NEXT y
	FOR y = 13 TO 16
		CALL FillBlock(3, y)
		CALL FillBlock(26, y)
	NEXT y
	FOR y = 1 TO 3
		CALL FillBlock(4, y)
		CALL FillBlock(25, y)
	NEXT y
	FOR y = 14 TO 16
		CALL FillBlock(4, y)
		CALL FillBlock(25, y)
	NEXT y
	FOR x = 5 TO 9
		CALL FillBlock(x, 1)
		CALL FillBlock(x, 16)
	NEXT x
	FOR x = 20 TO 24
		CALL FillBlock(x, 1)
		CALL FillBlock(x, 16)
	NEXT x
	FOR x = 5 TO 6
		CALL FillBlock(x, 2)
		CALL FillBlock(x, 15)
	NEXT x
	FOR x = 23 TO 24
		CALL FillBlock(x, 2)
		CALL FillBlock(x, 15)
	NEXT x

	y = 1
	FOR x = 10 TO 19
		FOR t = 1 TO 12
			Board(x, y, t) = 210
		NEXT t
	NEXT x
	y = 16
	FOR x = 10 TO 19
		FOR t = 13 TO 24
			Board(x, y, t) = 210
		NEXT t
	NEXT x

	x = 5
	FOR y = 3 TO 6
		 FOR t = 1 TO 5
			 Board(x, y, t) = 210
		 NEXT t
		 FOR t = 7 TO 10
			 Board(x, y, t) = 210
		 NEXT t
		 Board(x, y, 13) = 210
		 Board(x, y, 14) = 210
		 Board(x, y, 19) = 210
		 x = x - 1
	NEXT y
	x = 2
	FOR y = 11 TO 14
		 FOR t = 19 TO 23
			 Board(x, y, t) = 210
		 NEXT t
		 FOR t = 13 TO 16
			 Board(x, y, t) = 210
		 NEXT t
		 Board(x, y, 1) = 210
		 Board(x, y, 7) = 210
		 Board(x, y, 8) = 210
		 x = x + 1
	NEXT y
	x = 24
	FOR y = 3 TO 6
		 FOR t = 2 TO 6
			 Board(x, y, t) = 210
		 NEXT t
		 FOR t = 9 TO 12
			 Board(x, y, t) = 210
		 NEXT t
		 Board(x, y, 17) = 210
		 Board(x, y, 18) = 210
		 Board(x, y, 24) = 210
		 x = x + 1
	NEXT y
	x = 27
	FOR y = 11 TO 14
		 FOR t = 20 TO 24
			 Board(x, y, t) = 210
		 NEXT t
		 FOR t = 15 TO 18
			 Board(x, y, t) = 210
		 NEXT t
		 Board(x, y, 11) = 210
		 Board(x, y, 12) = 210
		 Board(x, y, 6) = 210
		 x = x - 1
	NEXT y

	Board(5, 3, 11) = 210
	Board(5, 3, 6) = 210
	FOR t = 1 TO 4
		Board(6, 3, t) = 210
	NEXT t
	FOR t = 1 TO 20
		Board(7, 2, t) = 210
	NEXT t
	FOR t = 1 TO 11
		Board(8, 2, t) = 210
	NEXT t
	Board(8, 2, 13) = 210
	FOR t = 1 TO 4
		Board(9, 2, t) = 210
	NEXT t
	FOR t = 13 TO 20
		Board(10, 1, t) = 210
	NEXT t
	Board(11, 1, 13) = 210

	Board(18, 1, 18) = 210
	FOR t = 13 TO 18
		Board(19, 1, t) = 210
	NEXT t
	Board(19, 1, 23) = 210
	Board(19, 1, 24) = 210
	FOR t = 3 TO 6
		Board(20, 2, t) = 210
	NEXT t
	FOR t = 1 TO 6
		Board(21, 2, t) = 210
	NEXT t
	FOR t = 8 TO 12
		Board(21, 2, t) = 210
	NEXT t
	Board(21, 2, 18) = 210
	FOR t = 1 TO 18
		Board(22, 2, t) = 210
	NEXT t
	Board(22, 2, 23) = 210
	Board(22, 2, 24) = 210
	FOR t = 3 TO 6
		Board(23, 3, t) = 210
	NEXT t
	Board(24, 3, 1) = 210
	Board(24, 3, 8) = 210

	Board(5, 14, 17) = 210
	Board(5, 14, 24) = 210
	FOR t = 19 TO 22
		Board(6, 14, t) = 210
	NEXT t
	Board(7, 15, 1) = 210
	Board(7, 15, 2) = 210
	FOR t = 7 TO 24
		Board(7, 15, t) = 210
	NEXT t
	FOR t = 19 TO 24
		Board(8, 15, t) = 210
	NEXT t
	FOR t = 13 TO 17
		Board(8, 15, t) = 210
	NEXT t
	Board(8, 15, 7) = 210
	FOR t = 19 TO 22
		Board(9, 15, t) = 210
	NEXT t
	Board(10, 16, 1) = 210
	Board(10, 16, 2) = 210
	FOR t = 7 TO 12
		Board(10, 16, t) = 210
	NEXT t
	Board(11, 16, 7) = 210

	Board(18, 16, 12) = 210
	FOR t = 5 TO 12
		Board(19, 16, t) = 210
	NEXT t
	FOR t = 21 TO 24
		Board(20, 15, t) = 210
	NEXT t
	Board(21, 15, 12) = 210
	FOR t = 14 TO 24
		Board(21, 15, t) = 210
	NEXT t
	FOR t = 5 TO 24
		Board(22, 15, t) = 210
	NEXT t
	FOR t = 21 TO 24
		Board(23, 14, t) = 210
	NEXT t
	Board(24, 14, 14) = 210
	Board(24, 14, 19) = 210

	REM Invisible Frame
	FOR x = 0 TO 29
		CALL FillBlock(x, 0)
		CALL FillBlock(x, 17)
	NEXT x
	FOR y = 1 TO 16
		CALL FillBlock(0, y)
		CALL FillBlock(29, y)
	NEXT y

END SUB

FUNCTION FrameArea
	a = 0
	FOR y = 1 TO 16
		FOR x = 1 TO 28
			FOR t = 1 TO 24
				IF Board(x, y, t) = 210 THEN a = a + 1
			NEXT t
		NEXT x
	NEXT y
	FrameArea = a / 6
END FUNCTION

SUB Grid
	GridColor = 8
	LINE (12, 24)-(12, 408), GridColor
	LINE (348, 24)-(348, 408), GridColor
	FOR h = 24 TO 408 STEP 12
		LINE (12, h)-(348, h), GridColor
	NEXT h
	FOR y = 24 TO 384 STEP 24
		FOR x = 12 TO 336 STEP 12
			LINE (x, y)-(x + 12, y + 24), GridColor
			LINE (x + 12, y)-(x, y + 24), GridColor
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

SUB LoadPuzzle
	OPEN "puzzle.ete" FOR INPUT AS #1
	FOR BrickNo = 1 TO 209
		INPUT #1, PuzzleX(BrickNo), PuzzleY(BrickNo), PuzzleT(BrickNo)
	NEXT BrickNo
	CLOSE #1

	OPEN "status.ete" FOR INPUT AS #2
		INPUT #2, TimeCounter, TurnCounter, ContentnessDemand, BricksPlacedMax
	CLOSE #2
END SUB

FUNCTION NearPlacedBrick (BrickNo)
	t = BrickNo
	WHILE PuzzleX(t) = 0
		t = t + 1
		IF t = 210 THEN t = 1
	WEND
	NearPlacedBrick = t
END FUNCTION

SUB PlaceBrick (x, y, t, BrickNo)
	CurBloX = x
	CurBloY = y
	CurTriNo = t
	BrickExp = Bricks(BrickNo)
	CALL ExpressionDecoder(BrickNo)
	PuzzleX(BrickNo) = x
	PuzzleY(BrickNo) = y
	PuzzleT(BrickNo) = t
END SUB

FUNCTION PlacedBricks
	Counter = 0
	FOR BrickNo = 1 TO 209
		IF NOT PuzzleX(BrickNo) = 0 THEN Counter = Counter + 1
	NEXT BrickNo
	PlacedBricks = Counter
END FUNCTION

FUNCTION PuzzleAccomplished
	a = 1
	BrickNo = 0
	WHILE (a = 1 AND BrickNo < 210)
		BrickNo = BrickNo + 1
		IF PuzzleX(BrickNo) = 0 THEN a = 0
	WEND
	PuzzleAccomplished = a
END FUNCTION

SUB RemoveBrick (BrickNo)
	CurBloX = PuzzleX(BrickNo)
	CurBloY = PuzzleY(BrickNo)
	CurTriNo = PuzzleT(BrickNo)
	BrickExp = Bricks(BrickNo)
	CALL ExpressionDecoder(0)
	PuzzleX(BrickNo) = 0
	PuzzleY(BrickNo) = 0
	PuzzleT(BrickNo) = 0
END SUB

SUB SavePuzzle
	OPEN "puzzle.ete" FOR OUTPUT AS #1
		FOR BrickNo = 1 TO 209
			WRITE #1, PuzzleX(BrickNo), PuzzleY(BrickNo), PuzzleT(BrickNo)
		NEXT BrickNo
	CLOSE #1

	OPEN "status.ete" FOR OUTPUT AS #2
		WRITE #2, TimeCounter, TurnCounter, ContentnessDemand, BricksPlacedMax
	CLOSE #2
END SUB

SUB ShutDown
	CALL SavePuzzle
	CALL DisplayPuzzle
	IF PuzzleAccomplished = 1 THEN
		COLOR 14
		LOCATE 28, 2
		PRINT "The Eternity Puzzle Is Solved! Congratulations!                 Press any key"; ""
		WHILE INKEY$ = ""
		WEND
	END IF
END SUB

FUNCTION SpaceForBrick (x, y, t, BrickNo)
CurBloX = x
CurBloY = y
CurTriNo = t
BrickExp = Bricks(BrickNo)

Space = 1
n = 1

IF NOT Board(CurBloX, CurBloY, CurTriNo) = 0 THEN Space = 0


WHILE NOT MID$(BrickExp, n, 1) = "s" AND Space = 1
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
	IF NOT Board(CurBloX, CurBloY, CurTriNo) = 0 THEN Space = 0
	n = n + 1
WEND
SpaceForBrick = Space
END FUNCTION

SUB StartUp

FillColor = 2
BrickColor = 2

CALL LoadBricks
CALL LoadPuzzle

FOR BrickNo = 1 TO 209
	x = PuzzleX(BrickNo)
	y = PuzzleY(BrickNo)
	t = PuzzleT(BrickNo)
	IF NOT PuzzleX(BrickNo) = 0 THEN CALL PlaceBrick(x, y, t, BrickNo)
NEXT BrickNo

CALL DisplayBackground
CALL Frame

CALL DisplayPuzzle

LOCATE 5, 66
PRINT BricksPlacedMax
LOCATE 5, 74
PRINT USING "###.#"; (BricksPlacedMax / 209) * 100

END SUB

SUB temp

REM NaborA
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 8
				CASE 2
					t = 9
				CASE 3
					t = 4
				CASE 4
					t = 3
				CASE 5
					t = 10
				CASE 6
					t = 11
				CASE 7
					t = 12
					x = x - 1
				CASE 8
					t = 1
				CASE 9
					t = 2
				CASE 10
					t = 5
				CASE 11
					t = 6
				CASE 12
					t = 7
					x = x + 1
				CASE 13
					t = 18
					x = x - 1
				CASE 14
					t = 19
				CASE 15
					t = 20
				CASE 16
					t = 23
				CASE 17
					t = 24
				CASE 18
					t = 13
					x = x + 1
				CASE 19
					t = 14
				CASE 20
					t = 15
				CASE 21
					t = 22
				CASE 22
					t = 21
				CASE 23
					t = 16
				CASE 24
					t = 17
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborB
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
			 CASE 1
				 t = 2
			 CASE 2
				 t = 1
			 CASE 3
				 t = 21
				 y = y - 1
			 CASE 4
				 t = 22
				 y = y - 1
			 CASE 5
				 t = 6
			 CASE 6
				 t = 5
			 CASE 7
				 t = 13
			 CASE 8
				 t = 9
			 CASE 9
				 t = 8
			 CASE 10
				 t = 11
			 CASE 11
				 t = 10
			 CASE 12
				 t = 18
			 CASE 13
				 t = 7
			 CASE 14
				 t = 15
			 CASE 15
				 t = 14
			 CASE 16
				 t = 17
			 CASE 17
				 t = 16
			 CASE 18
				 t = 12
			 CASE 19
				 t = 20
			 CASE 20
				 t = 19
			 CASE 21
				 t = 3
				 y = y + 1
			 CASE 22
				 t = 4
				 y = y + 1
			 CASE 23
				 t = 24
			 CASE 24
				 t = 23
		END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

REM NaborC
x = CurBloX
y = CurBloY
t = CurTriNo

			SELECT CASE t
				CASE 1
					t = 6
					x = x - 1
				CASE 2
					t = 3
				CASE 3
					t = 2
				CASE 4
					t = 5
				CASE 5
					t = 4
				CASE 6
					t = 1
					x = x + 1
				CASE 7
					t = 8
				CASE 8
					t = 7
				CASE 9
					t = 10
				CASE 10
					t = 9
				CASE 11
					t = 12
				CASE 12
					t = 11
				CASE 13
					t = 14
				CASE 14
					t = 13
				CASE 15
					t = 16
				CASE 16
					t = 15
				CASE 17
					t = 18
				CASE 18
					t = 17
				CASE 19
					t = 24
					x = x - 1
				CASE 20
					t = 21
				CASE 21
					t = 20
				CASE 22
					t = 23
				CASE 23
					t = 22
				CASE 24
					t = 19
					x = x + 1
			END SELECT

SELECT CASE Board(x, y, t)
	CASE BrickNo
		REM Not Border
	CASE 0
		REM Not Content Border
		BorderTriangles = BorderTriangles + 1
	CASE ELSE
		REM Content Border
		BorderTriangles = BorderTriangles + 1
		SigmaContentness = SigmaContentness + 1
END SELECT

END SUB

