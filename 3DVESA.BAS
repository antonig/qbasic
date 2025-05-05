DECLARE FUNCTION filsel$ (ext$)
DECLARE SUB helpme ()
DECLARE SUB SVGASwapScreen ()
DECLARE SUB MEMTRN (SOURCE AS LONG, DEST AS LONG)
DECLARE SUB triGOU (r1%, g1%, b1%, X1%, YY1%, r2%, g2%, b2%, X2%, YY2%, r3%, g3%, b3%, X3%, YY3%)
DECLARE FUNCTION SetVESAMode% (mode%)
DECLARE FUNCTION load3ds% (filename$)
DECLARE SUB centerit ()
DECLARE SUB rot3d (xrot!, yrot!, zrot!)
DECLARE SUB CalcArcCosTbl (fact%, fact2%, fact3%)
DECLARE SUB calcnormals ()
DECLARE SUB fastsort (SortArray() AS ANY, Lower%, Upper%)
DECLARE SUB rotate (p AS ANY, r AS ANY)
DECLARE SUB loadplg (filen$, isO3A%)
DECLARE SUB triflat (r%, g%, b%, X1%, YY1%, X2%, YY2%, X3%, YY3%)
DECLARE FUNCTION XTimerW& ()
DECLARE SUB ffix

DEFINT A-Z
'------------------------------------------------------------------------
'3DS,PLG and O3A mesh viewer in 640x480x16-15bit. Flat and gouraud
'Pure QB.
'Screen clear routine modified from Glenn Stumpff
'QuickSort iterative (rather than recursive) by Cornel Huth
'by Antoni Gual 9/2002   agual@eic.ictnet.es
'----------------------------------------------------------------------------
'KEYS: ESC:Exit     NumPad:Turn    */Numpad:3rd axis turn   5Numpad:Stop turn  '
'ArrowKeys+Ctrl: Cente             +-Numpad:Zoom            F1:Toggle gouraud
'----------------------------------------------------------------------------
'You may need to modify next two commands:
'  To run/compile it in PDS
'x$INCLUDE: 'QBx.BI'
'  To run/compile it in QB4.0 or 4.5 s
' $INCLUDE: 'QB.BI'
'If you want to compile it in QB4.5 you may want to use ffix floating point
' patch. (You'll need to link ffix and qb libs in a single one)
'ffix
'----------------------------------------------------------------------
'Some places with free 3ds and plg meshes to download
'http://www.dcs.ed.ac.uk/home/objects/rend386.html
'http://www.3dcafe.com/asp/meshes.asp
'http://www.amazing3d.com/free/free1.html
'---------------------------------------------------------------------
'ok can read plg's with quadrilateral and tringular faces mixed
'ok added backface culling by using normal dot products
'ok ended scanline 1 pixel before end
'ok added fastsort of faces
'ok added flat shading using precalculated face normals.ok!
'ok added subpixel accuracy. Must improve speed!
'ok flat shading shimmies must improve accuracy
'ok going to add kbd control. Bad zoom on chopper. Can't load a second file  ok!
'ok made into a sub
'ok added auto centering
'ok added o3a file reading (not textures)
'ok added automatic zooming to fullscreeen
'ok added 640x480 SVGA 5:5:5 color mode with 2 screen pages
'ok SVGA buffer cleaning is too slow. Added asm routine
'ok simple 3dstudio file reading.Needs
'ok checks SVGAmodes 110 and 111
'ok cls using an array of zeros and int 15H for memcopy
'ok added F1 switch for toggling gouraud
'ok avoid calculating normals for unused vertexs (normal would be undefined)
'ok can read plg's with faces with more than 4 vertexs

'TO DO
'    reduce flickering
'    implement arrays in EMS, for bigger meshes
'    reduce color span to 6 bits
'    allow center of gravity as center of rotation
'    should implement a serious overlapping face removal
'     entropy's Z map would be accurate if z was calculated for every pixel..
'     so z would have to be interpolated at face rendering...
'    gouraud!
'    light source orientation with alt-arrows
'    rotation using quaternions
'    mouse driven
'    screen menu
'    multiple objects
'   holes in some 3ds meshes!
'   read 3ds and plg multiple meshes
'   read the color field in meshes
'   colored background
'   should detect /ah switch
'   online help
'   phong shading
'   texture mapping
'ok better file selector (single input routine)


DIM regs  AS RegType, regsx AS RegTypeX

'vesa handling
TYPE vluttype
	page AS INTEGER
	ofs  AS INTEGER
END TYPE
 
DIM vlut(959) AS vluttype
DIM curbank, lasty, slina, slinb
DIM blook(255) AS INTEGER, glook(255) AS INTEGER, g2look(255) AS INTEGER
DIM rlook(255) AS INTEGER

'a big array that will remain empty for all program
REDIM CLBUF(0 TO 16383) AS LONG
FOR i = 0 TO 16383: CLBUF(i) = 0: NEXT
DIM gdt(1 TO 12) AS LONG

'You can use these constants to convert XTimer readings to seconds
CONST clockfreq# = 1193181.666#           'PIT clock frequency
CONST secstick# = 1 / clockfreq#

TYPE delaytype
 start AS LONG
 ends AS LONG
END TYPE




CONST subpix = 2
CONST pi# = 3.1415926#



TYPE point2d
x    AS INTEGER
y    AS INTEGER
END TYPE

TYPE stacktype
 low AS INTEGER
 hi AS INTEGER
END TYPE

TYPE sorttype
 n AS INTEGER
 z AS INTEGER
END TYPE


TYPE point3d
 x    AS SINGLE
 y    AS SINGLE
 z    AS SINGLE
END TYPE


TYPE planetype
 p1   AS INTEGER
 p2   AS INTEGER
 p3   AS INTEGER
 CLR  AS INTEGER
END TYPE

TYPE tdschunk
 typ AS INTEGER
 length AS LONG
END TYPE



DIM nv AS INTEGER, nf AS INTEGER



CONST kesc = &H1, kBSPC = &HE, kTAB = &HF, kENTER = &H1C, kSPC = &H39
CONST kctrl = &H1D, kLFSH = &H2A, kRGSH = &H36, kALT = &H38
CONST kHOME = &H47, kup = &H48, kPGUP = &H49
CONST kEND = &H4F, kdown = &H50, kPGDW = &H51
CONST kminus = &H4A, kplus = &H4E, kmult = &H37, kdiv = &H35
CONST kleft = &H4B, kmid = &H4C, kright = &H4D
CONST kINS = &H52, kDEL = &H53

CONST kf1 = &H3B, kf2 = &H3C, kf3 = &H3D, kf4 = &H3E, kf5 = &H3F
CONST kf6 = &H40, kf7 = &H41, kf8 = &H42, kf9 = &H43, kf10 = &H44

'DIM j AS LONG
REDIM vertx(0)   AS point3d
REDIM rvertx(0)  AS point3d
REDIM svertx(0)  AS point2d

REDIM nvertx(0)  AS point3d
REDIM rnvertx(0)  AS point3d

REDIM face(0)  AS planetype
REDIM nface(0)  AS point3d
REDIM rnface(0)  AS point3d
REDIM oface(0)  AS sorttype
REDIM arccos(0)
DIM spot   AS point3d, camera AS point3d
DIM ddx AS INTEGER, ddy AS INTEGER, zoom AS DOUBLE
DIM kbd(127)
helpme
vm = Setsvgamode(3)
IF vm THEN PRINT "No VESA SVGA driver available, can't run.": END
goura = -1

CalcArcCosTbl 100, 128, 123
'new file loop
DO
	cmd = INSTR(COMMAND$, "/")
	IF cmd THEN
	 flnam$ = MID$(COMMAND$, cmd + 1)
	ELSE
	 
	 flnam$ = filsel$("PLG 3DS O3A")
	END IF
	IF LEN(flnam$) = 0 THEN END
	flnam$ = UCASE$(flnam$)
	dot = INSTR(flnam$, ".")
	ext$ = MID$(flnam$, dot + 1)
	loadresult = 0
	SELECT CASE ext$
	CASE "PLG": loadplg flnam$, 0
	CASE "O3A": loadplg flnam$, -1
	CASE "3DS": loadresult = load3ds(flnam$)
	CASE ELSE: PRINT "format not supported": GOTO continue
	END SELECT
	SELECT CASE loadresult
	CASE 1: PRINT "Can't handle so many vertexs": GOTO continue
	CASE 2: PRINT "Can't handle so many faces": GOTO continue
	CASE 3: PRINT "Mesh not found in 3D Studio file": GOTO continue
	CASE 4: PRINT "Array bigger than 128K": GOTO continue
	END SELECT
	PS& = (VARSEG(CLBUF(0)) AND &HFFFF&) * 16& + (VARPTR(CLBUF(0)) AND &HFFFF&)
	PD& = &HA0000
	SZ& = 65536
	MEMTRN PS&, PD&
 
	centerit
	calcnormals
	WHILE LEN(INKEY$): WEND
 
	'set VESA MODE 110 OR 111
	vm = SetVESAMode(&H110)
	IF vm THEN vm = SetVESAMode(&H111)
	IF vm THEN PRINT "No VESA 1.1 640x480x64K(32k) mode available. Can't run": END
 
	start# = TIMER
 
	spot.x = 3.4
	spot.y = 6.2
	spot.z = -SQR(100 - spot.x * spot.x - spot.y * spot.y)
	camera.z = 9500
	FpsC& = 0
	'rotator loop


	dx! = 0
	dy! = 0
	dz! = 0

	DEF SEG = &H40: POKE &H1C, PEEK(&H1A): POKE &H17, PEEK(&H17) AND NOT 32
	T! = TIMER: f = 0: tt& = XTimerW&
	ddx = 320: ddy = 240
	xrot! = 0: yrot! = 0: xrot! = 0
	dx! = 0: dy! = 0: dz! = 0
	kincr! = 0
	DO
		tt1& = XTimerW&
		kincr! = ((tt1& - tt&) / 10000000)
		SWAP tt&, tt1&
		dzz! = 0
		DEF SEG = &H40: POKE &H1C, PEEK(&H1A)
		k = INP(&H60): kbd(k AND 127) = -((k AND 128) = 0)
		IF kbd(kmid) THEN dx! = 0: dy! = 0: dz! = 0: dzm! = 0: GOSUB ERASEKBD
		IF kbd(kesc) THEN kbd(1) = 0: GOSUB ERASEKBD: EXIT DO
		IF kbd(kright) THEN
				IF kbd(kctrl) THEN ddx = ddx + 1 ELSE dx! = dx! + kincr!
		ELSEIF kbd(kleft) THEN
				IF kbd(kctrl) THEN ddx = ddx - 1 ELSE dx! = dx! - kincr!
		ELSE
			dx! = 0
		END IF
		IF kbd(kdown) THEN
				IF kbd(kctrl) THEN ddy = ddy + 1 ELSE dy! = dy! + kincr!
		ELSEIF kbd(kup) THEN
				IF kbd(kctrl) THEN ddy = ddy - 1 ELSE dy! = dy! - kincr!
		ELSE
			dy! = 0
		END IF
		IF kbd(kmult) THEN
				dz! = dz! + kincr!
		ELSEIF kbd(kdiv) THEN
				dz! = dz! - kincr!
		ELSE
				dz! = 0
		END IF
		IF kbd(kplus) THEN
				dzm! = dzm! + kincr! / 20
		ELSEIF kbd(kminus) THEN
				dzm! = dzm! - kincr! / 20
		ELSE
				dzm! = 0
		END IF
		IF kbd(kf1) THEN GOSUB ERASEKBD: goura = NOT goura
	
		'update rotation matrix
		xrot! = xrot! + dx!
		yrot! = yrot! + dy!
		zrot! = zrot! + dz!
		zoom# = zoom# + dzm!
	 rot3d xrot!, yrot!, zrot!
	
	 SVGASwapScreen
	 FpsC& = FpsC& + 1
	LOOP
	WHILE LEN(INKEY$): WEND
	vm = SetVESAMode(3): SCREEN 0: COLOR 15, 0: CLS
	TotalTime# = (TIMER - start#)
	PRINT USING "\      \: ###.# Fps, #### vrtx, #### fcs"; flnam$; FpsC& / TotalTime#; nv; nf
continue:
	PRINT "New File Y/N": a$ = UCASE$(INPUT$(1)): IF a$ = "N" THEN EXIT DO
LOOP

ERASE vertx, rvertx, svertx, nvertx, rnvertx, face, nface, oface

SCREEN 0: WIDTH 80, 25: VIEW PRINT: COLOR 7, 0: CLS
PRINT "3d rotator, by Antoni Gual agual@eic.ictnet.es"
END

nofile: errata% = ERR: RESUME NEXT


Trapit:
RESUME NEXT


ERASEKBD:
FOR i = 1 TO 127: kbd(i) = 0: NEXT
RETURN

DEFSNG A-Z
SUB CalcArcCosTbl (fact%, fact2%, fact3%)
SHARED arccos%()
REDIM arccos%(-fact% TO fact%)
ff = fact2% * 2 / pi#

arccos%(-fact) = fact3% - fact2%
arccos%(fact) = fact3% + fact2%
FOR i% = -fact% + 1 TO fact% - 1
	tmp! = i% / fact%
	arccos%(i%) = ff * ATN(tmp! / SQR(1 - tmp! * tmp!)) + fact3%
NEXT
END SUB

DEFDBL A-Z
SUB calcnormals
'calculate normals to faces
SHARED vertx() AS point3d, nvertx() AS point3d
SHARED face() AS planetype, nface() AS point3d
SHARED nv AS INTEGER, nf AS INTEGER

DIM u AS point3d, v AS point3d, n AS point3d

'

FOR i% = 1 TO nf
	u.x = vertx(face(i%).p2).x - vertx(face(i%).p1).x
	u.y = vertx(face(i%).p2).y - vertx(face(i%).p1).y
	u.z = vertx(face(i%).p2).z - vertx(face(i%).p1).z

	v.x = vertx(face(i%).p3).x - vertx(face(i%).p1).x
	v.y = vertx(face(i%).p3).y - vertx(face(i%).p1).y
	v.z = vertx(face(i%).p3).z - vertx(face(i%).p1).z

	n.x = u.y * v.z - u.z * v.y
	n.y = -u.x * v.z + u.z * v.x
	n.z = u.x * v.y - u.y * v.x

	imod = 10 / SQR(n.x * n.x + n.y * n.y + n.z * n.z)

	nface(i%).x = n.x * imod
	nface(i%).y = n.y * imod
	nface(i%).z = n.z * imod
	IF ABS(nface(i%).x) + ABS(nface(i%).y) + ABS(nface(i%).z) < .00001 THEN STOP
NEXT

'EXIT SUB
'
'calculate normals to vertexs (interpolate normals to faces)
'
FOR i% = 1 TO nf
	tmp% = face(i%).p1
	nvertx(tmp%).x = nvertx(tmp%).x + nface(i%).x
	nvertx(tmp%).y = nvertx(tmp%).y + nface(i%).y
	nvertx(tmp%).z = nvertx(tmp%).z + nface(i%).z
	tmp% = face(i%).p2
	nvertx(tmp%).x = nvertx(tmp%).x + nface(i%).x
	nvertx(tmp%).y = nvertx(tmp%).y + nface(i%).y
	nvertx(tmp%).z = nvertx(tmp%).z + nface(i%).z
	tmp% = face(i%).p3
	nvertx(tmp%).x = nvertx(tmp%).x + nface(i%).x
	nvertx(tmp%).y = nvertx(tmp%).y + nface(i%).y
	nvertx(tmp%).z = nvertx(tmp%).z + nface(i%).z
NEXT

'normalize
FOR i% = 1 TO nv
	IF ABS(nvertx(i%).x) + ABS(nvertx(i%).y) + ABS(nvertx(i%).z) > .00001 THEN
	imod = 8 / SQR(nvertx(i%).x * nvertx(i%).x + nvertx(i%).y * nvertx(i%).y + nvertx(i%).z * nvertx(i%).z)
	nvertx(i%).x = nvertx(i%).x * imod
	nvertx(i%).y = nvertx(i%).y * imod
	nvertx(i%).z = nvertx(i%).z * imod
	END IF
NEXT

END SUB

SUB centerit
	'centers and calculates point of view so image is more or less fullscreen
 
	SHARED vertx() AS point3d, nv AS INTEGER, zoom AS DOUBLE
	 
	maxx = -1E+35
	maxy = -1E+35
	maxz = -1E+35
	minx = 1E+35
	miny = 1E+35
	minz = 1E+35
	FOR i% = 1 TO nv
		 IF vertx(i%).x > maxx THEN maxx = vertx(i%).x
		 IF vertx(i%).x < minx THEN minx = vertx(i%).x
		 IF vertx(i%).y > maxy THEN maxy = vertx(i%).y
		 IF vertx(i%).y < miny THEN miny = vertx(i%).y
		 IF vertx(i%).z > maxz THEN maxz = vertx(i%).z
		 IF vertx(i%).z < minz THEN minz = vertx(i%).z
	NEXT
	centx = (maxx + minx) / 2
	centy = (maxy + miny) / 2
	centz = (maxz + minz) / 2
	sizex = maxx - minx
	sizey = maxy - miny
	sizez = maxz - minz
	maxsize = SQR(sizex * sizex + sizey * sizey + sizez * sizez)
	ms = 500 / maxsize
	FOR i% = 1 TO nv
			vertx(i%).x = (vertx(i%).x - centx) * ms
			vertx(i%).y = (vertx(i%).y - centy) * ms
			vertx(i%).z = (vertx(i%).z - centz) * ms
	NEXT
 
	'zoom it
	zoom = 1' 50 / maxsize
	'PRINT maxsize; zoom
	'STOP
END SUB

DEFINT A-Z
SUB fastsort (SortArray() AS sorttype, Lower, Upper)

	'QuickSort iterative (rather than recursive) by Cornel Huth
	DIM Lstack(1 TO 128) AS stacktype   'our stack
	DIM Sp AS INTEGER                   'out stack pointer
	Sp = 1
	'maxsp = sp
	Lstack(Sp).low = Lower
	Lstack(Sp).hi = Upper
	Sp = Sp + 1
	DO
		Sp = Sp - 1
		low = Lstack(Sp).low
		hi = Lstack(Sp).hi
		DO
			i = low
			j = hi
			mid = (low + hi) \ 2
			Compare = SortArray(mid).z
			DO
				WHILE SortArray(i).z > Compare: i = i + 1: WEND
				WHILE SortArray(j).z < Compare: j = j - 1: WEND
				IF i <= j THEN
					SWAP SortArray(i), SortArray(j)
					i = i + 1
					j = j - 1
				END IF
			LOOP WHILE i <= j
			IF j - low < hi - i THEN
				IF i < hi THEN
					Lstack(Sp).low = i
					Lstack(Sp).hi = hi
					Sp = Sp + 1
				END IF
				hi = j
			ELSE
				IF low < j THEN
					Lstack(Sp).low = low
					Lstack(Sp).hi = j
					Sp = Sp + 1
				END IF
				low = i
			END IF
		LOOP WHILE low < hi
	LOOP WHILE Sp <> 1
END SUB

DEFSNG A-Z
FUNCTION filsel$ (ext$)
'allows to select file in a menu. returns filename with path or "" if ESC'ed
'can't change drive
'more than aprox 90 files/dirs in a dir will not work
CONST wid = 18
CONST row1 = 2
CONST col1 = 1
CONST col4 = 4
blank$ = SPACE$(80)
DIM file$, row, col, lastrow, lastcol

'parse extensions
DIM xte$(10)
ext$ = LTRIM$(RTRIM$(ext$))

IF LEN(ext$) = 0 THEN
 xte$(0) = ""    'no extension enforced
	cnt = 0
 ELSE            'extensions strings not empty
xte$(0) = "*"'   ' get dirs
i = 1: cnt = 1
DO               ' for each extension requested
	i1 = INSTR(i, ext$, " ")
	IF i1 = 0 THEN xte$(cnt) = "*." + MID$(ext$, i, 100): EXIT DO 'single ext
	xte$(cnt) = "*." + MID$(ext$, i, i1 - i)
	cnt = cnt + 1: i = i1 + 1
LOOP
END IF
COLOR 15, 0
'use FILES to get and save cuurent folder
CLS : FILES: GOSUB getfolder: curfolder$ = RTRIM$(ff1$) + "\"
'
GOSUB rescan

DO
DO: k$ = INKEY$: LOOP UNTIL LEN(k$)

SELECT CASE RIGHT$(k$, 1)
CASE CHR$(27)     'exit
		filsel$ = "": GOTO quit
 CASE CHR$(13)
		ff$ = LTRIM$(RTRIM$(file$))      'chdir or exit with filename
		IF LEN(ff$) THEN
			IF RIGHT$(ff$, 5) = "<DIR>" THEN
				 ff$ = LEFT$(ff$, INSTR(ff$, " ") - 1)
				 CHDIR ff$
				 GOSUB rescan
			ELSE
			 
				GOSUB getfolder: fold$ = RTRIM$(ff1$) + "\"
				'remove spaces inserted by FILES before .ext  
				pt = INSTR(ff$, ".")
				ff$ = RTRIM$(LEFT$(ff$, pt - 1)) + MID$(ff$, 9, 4)
				filsel$ = fold$ + ff$: CHDIR curfolder$: GOTO quit
			END IF
		END IF
CASE CHR$(&H48)   'up
		IF row > row1 THEN row = row - 1: GOSUB cursor
CASE CHR$(&H50) 'down
		IF row < lastrow THEN row = row + 1: GOSUB cursor
CASE CHR$(&H4B) 'left
		IF col > col1 THEN col = col - 1: GOSUB cursor
CASE CHR$(&H4D) 'right
		 IF col < col4 THEN col = col + 1: GOSUB cursor
END SELECT
LOOP
EXIT FUNCTION

cursor:     'move cursor and get text from screen
DIM lastr, lastc
IF lastr <> 0 THEN 'on function entry
	r = lastr
	c = (lastc - 1) * wid + 1
	FOR i = 0 TO wid - 1
		COLOR , 0: LOCATE r, c + i: PRINT CHR$(SCREEN(r, c + i));
	NEXT
	file$ = ""
END IF
	'OSUB getfolder: fold$ = rtrim(ff1$)+ "\"
	r = row
	c = (col - 1) * wid + 1
	GOSUB getcurrent
	file$ = fold$ + ff1$
	lastr = row
	lastc = col
RETURN

rescan:     'scans dir after a dir change
COLOR 15, 0: CLS
LOCATE 25, 1: COLOR 14, 6: PRINT " Cursor to select file or dir | Enter changes dir - selects file | ESC quits    ";
LOCATE 1, 1: COLOR 15, 0
ON ERROR GOTO nofile
FOR i = 0 TO cnt
FILES xte$(i)   'get file listings for different extensions

	LOCATE CSRLIN - 1, 1: PRINT SPACE$(80); : LOCATE CSRLIN - 1, 1
NEXT
lastrow = CSRLIN - 1
row = 2
col = 1
ON ERROR GOTO 0
GOSUB cursor
RETURN

getfolder:
r = 1
c = 1
getcurrent:
	ff1$ = ""
	FOR i = 0 TO wid - 1
		a$ = CHR$(SCREEN(r, c + i))
		ff1$ = ff1$ + a$
		COLOR , 6: LOCATE r, c + i: PRINT a$;
	NEXT
RETURN
quit: COLOR 15, 0: CLS : EXIT FUNCTION
END FUNCTION

DEFINT A-Z
SUB helpme
CLS
PRINT : PRINT
PRINT "3DS,PLG and O3A mesh viewer in 640x480x16-15bit. Flat and gouraud"
PRINT "by Antoni Gual 9/2002"
PRINT
PRINT "KEYS:"
PRINT "----------------------------------------------------------------------------"
PRINT "      ESC:Exit     NumPad:Turn    */Numpad:3rd axis turn   5Numpad:Stop turn"
PRINT "ArrowKeys+Ctrl:    Center         +-Numpad:Zoom            F1:Toggle shading"
PRINT "----------------------------------------------------------------------------"
PRINT
PRINT "Any key to start"
SLEEP
END SUB

FUNCTION load3ds (filename$)
SHARED vertx() AS point3d, svertx() AS point2d, rvertx() AS point3d
SHARED nvertx() AS point3d, rnvertx() AS point3d
SHARED face() AS planetype, oface() AS sorttype
SHARED rnface() AS point3d, nface() AS point3d
SHARED nv AS INTEGER, nf AS INTEGER
CONST debug = 1
DIM tds AS tdschunk
a$ = " "
OPEN filename$ FOR BINARY AS #1

GET #1, , tds
IF tds.typ <> &H4D4D THEN CLOSE : PRINT "This file is not 3ds format": END
DO
GET #1, , tds
IF debug THEN PRINT "main "; HEX$(tds.typ), tds.length
SELECT CASE tds.typ
CASE &H3D3D: GOSUB ReadEditChunk
CASE ELSE: GOSUB skipit
END SELECT
LOOP UNTIL EOF(1)
STOP
load3ds = 0
CLOSE
EXIT FUNCTION

ReadEditChunk:
pptr1& = SEEK(1) + tds.length - 6
DO
	GET #1, , tds
	IF debug THEN PRINT "Edit Chunk "; HEX$(tds.typ), tds.length
	SELECT CASE tds.typ
	CASE &H4000:
		GOSUB readname
		'IF cnt = 1 THEN SEEK #1, SEEK(1) - 1: GOSUB skipit ELSE
		GOSUB ReadObject
	CASE ELSE: GOSUB skipit
	END SELECT
LOOP UNTIL SEEK(1) = pptr1&
RETURN

ReadObject:
pptr2& = SEEK(1) + tds.length - 6 - cnt
DO
GET #1, , tds
IF debug THEN PRINT "readobject "; HEX$(tds.typ), tds.length
SELECT CASE tds.typ
CASE &H4100: GOSUB ReadTrimesh
CASE ELSE: GOSUB skipit
END SELECT
LOOP UNTIL SEEK(1) = pptr2&
RETURN

ReadTrimesh:
pptr3& = SEEK(1) + tds.length - 6
DO
GET #1, , tds
IF debug THEN PRINT "readtrimesh "; HEX$(tds.typ), tds.length
SELECT CASE tds.typ
CASE &H4110: GOSUB readtrivertexl
CASE &H4120: GOSUB readfacelist: CLOSE : EXIT FUNCTION
CASE ELSE: GOSUB skipit
END SELECT
LOOP UNTIL SEEK(1) = pptr3&
RETURN

readname:
cnt = 0
b$ = ""
DO
 GET #1, , a$
 cnt = cnt + 1
 b$ = b$ + a$
LOOP UNTIL ASC(a$) = 0
b$ = LEFT$(b$, LEN(b$) - 1)
IF debug THEN PRINT "readname "; b$
RETURN


readtrivertexl:
GET #1, , nv
IF debug THEN PRINT "NV "; nv
ERASE vertx, rvertx, svertx, nvertx, rnvertx
'file size =nv*(52+20*1.9) 1vertex +1.9*1face
IF nv > (FRE(-1) \ 81) THEN load3ds = 1: CLOSE : EXIT FUNCTION
IF nv * 12& > 131070 * 2 THEN load3ds = 4: CLOSE : EXIT FUNCTION
REDIM vertx(nv)   AS point3d
REDIM rvertx(nv) AS point3d
REDIM svertx(nv) AS point2d
REDIM nvertx(nv) AS point3d
REDIM rnvertx(nv) AS point3d
FOR i = 1 TO nv
 GET #1, , vertx(i).x
 GET #1, , vertx(i).y
 GET #1, , vertx(i).z
NEXT
RETURN

readfacelist:
 GET #1, , nf
 IF debug THEN PRINT "NV "; nf
 ERASE face, nface, rnface, oface
 'file size =nv*(52/1.9+20) 1vertex/1.9 + 1face
 IF nf > (FRE(-1) \ 48) THEN load3ds = 2: CLOSE : EXIT FUNCTION
 IF nf * 12& > 131070 THEN load3ds = 4: CLOSE : EXIT FUNCTION

	REDIM face(nf)  AS planetype
	REDIM nface(nf) AS point3d
	REDIM rnface(nf) AS point3d
	REDIM oface(nf) AS sorttype
FOR i = 1 TO nf
 GET #1, , x
 face(i).p1 = x + 1
 GET #1, , x
 face(i).p2 = x + 1
 GET #1, , x
 face(i).p3 = x + 1
 GET #1, , dummy%
 'IF (dummy% AND 7) <> 7 THEN SWAP face(i).p3, face(i).p2
NEXT
RETURN

skipit:
	IF debug THEN PRINT "skipping "
	SEEK #1, SEEK(1) + tds.length - 6
	IF SEEK(1) > LOF(1) THEN load3ds = 3: CLOSE : EXIT FUNCTION
RETURN

END FUNCTION

SUB loadplg (filen$, isO3A)

SHARED vertx() AS point3d, svertx() AS point2d, rvertx() AS point3d
SHARED nvertx() AS point3d, rnvertx() AS point3d
SHARED face() AS planetype, oface() AS sorttype
SHARED rnface() AS point3d, nface() AS point3d

SHARED nv AS INTEGER, nf AS INTEGER
	Sp$ = CHR$(32)

	OPEN filen$ FOR INPUT AS #1
	LINE INPUT #1, a$

	IF isO3A THEN LINE INPUT #1, a$ ELSE GOSUB nextword

	GOSUB nextword
	nv = VAL(nw$)
	GOSUB nextword
	nf1 = VAL(nw$)

	REDIM vertx(nv)   AS point3d
	REDIM rvertx(nv) AS point3d
	REDIM svertx(nv) AS point2d
	REDIM nvertx(nv) AS point3d
	REDIM rnvertx(nv) AS point3d

	nf = nf1
	IF NOT isO3A THEN
		FOR i = 1 TO nv
			DO: LINE INPUT #1, a$: LOOP UNTIL LEN(a$)
		NEXT

		
		FOR i = 1 TO nf1
			DO: LINE INPUT #1, a$: LOOP UNTIL LEN(a$)
				GOSUB nextword
				GOSUB nextword
				faces = VAL(nw$)
				nf = nf + faces - 3
		NEXT
		SEEK #1, 1
		LINE INPUT #1, a$
	END IF


	REDIM face(nf)  AS planetype
	REDIM nface(nf) AS point3d
	REDIM rnface(nf) AS point3d
	REDIM oface(nf) AS sorttype


	FOR i = 1 TO nv
		DO: LINE INPUT #1, a$: LOOP UNTIL LEN(LTRIM$(a$))
		GOSUB nextword
		vertx(i).x = VAL(nw$)
		GOSUB nextword
		vertx(i).y = VAL(nw$)
		GOSUB nextword
		vertx(i).z = VAL(nw$)
		'PRINT vertx(i).x; vertx(i).y; vertx(i).z
	NEXT

	IF isO3A THEN
		FOR i = 1 TO nv
			DO: LINE INPUT #1, a$: LOOP UNTIL LEN(a$)
		NEXT
	END IF

	i = 1
	FOR j = 1 TO nf1
		DO: LINE INPUT #1, a$: LOOP UNTIL LEN(LTRIM$(a$))
		IF NOT isO3A THEN
			GOSUB nextword
			GOSUB nextword
		faces = VAL(nw$)
		END IF
		GOSUB nextword
		face(i).p1 = VAL(nw$) + 1
		GOSUB nextword
		face(i).p2 = VAL(nw$) + 1
		GOSUB nextword
		face(i).p3 = VAL(nw$) + 1
		'PRINT face(i).p1; face(i).p2; face(i).p3
		i = i + 1
		IF NOT isO3A THEN
			IF faces > 3 THEN
			 FOR ff = 4 TO faces
				 face(i).p1 = face(i - 1).p1
				 face(i).p2 = face(i - 1).p3
				 GOSUB nextword
				 face(i).p3 = VAL(nw$) + 1
				 'PRINT face(i).p1; face(i).p2; face(i).p2
				 i = i + 1
				NEXT
			END IF
		END IF
	NEXT
	CLOSE #1
EXIT SUB

nextword:
	a$ = LTRIM$(a$)
	n = INSTR(a$, " ")
	IF n THEN
		nw$ = LEFT$(a$, n - 1)
		a$ = LTRIM$(MID$(a$, n + 1))
	ELSE
		nw$ = a$
		a$ = ""
	END IF
RETURN

END SUB

SUB MEMTRN (SOURCE AS LONG, DEST AS LONG)
'Setup of the descriptor for memory transfer with INT 15H service 87H
'adapted from Glenn Stumpff

SHARED gdt()  AS LONG
'
'  Get address of Global Descriptor Table.
'
SMGDT = VARSEG(gdt(1)): OSGDT = VARPTR(gdt(1))
'
'  Get four bytes comprising SOURCE and DEST.
'
B1S = SOURCE AND &HFF
B2S = (SOURCE AND &HFF00&) / 256
B3S = (SOURCE AND &HFF0000) / 65536
B4S = (SOURCE AND &HFF000000) / 16777216 AND &HFF
B1D = DEST AND &HFF
B2D = (DEST AND &HFF00&) / 256
B3D = (DEST AND &HFF0000) / 65536
B4D = (DEST AND &HFF000000) / 16777216 AND &HFF
'
'  Note that B4S and B4D will be zero on 80286 machines due to the 16 MB
' address limit.
'
'  Set up GDT for XMS transfer.
'
DEF SEG = SMGDT
FOR i = 0 TO 15
POKE OSGDT + i, 0
NEXT i
POKE OSGDT + 16, 255
POKE OSGDT + 17, 255
POKE OSGDT + 18, B1S
POKE OSGDT + 19, B2S
POKE OSGDT + 20, B3S
POKE OSGDT + 21, &H93
POKE OSGDT + 22, 0
POKE OSGDT + 23, B4S
POKE OSGDT + 24, 255
POKE OSGDT + 25, 255
POKE OSGDT + 26, B1D
POKE OSGDT + 27, B2D
POKE OSGDT + 28, B3D
POKE OSGDT + 29, &H93
POKE OSGDT + 30, 0
POKE OSGDT + 31, B4D
FOR i = 0 TO 15
POKE OSGDT + 32& + i, 0
NEXT i

END SUB

SUB rot3d (xrot!, yrot!, zrot!)
SHARED vertx() AS point3d, svertx() AS point2d, rvertx() AS point3d
SHARED nvertx() AS point3d, rnvertx() AS point3d
SHARED face() AS planetype, oface() AS sorttype
SHARED rnface() AS point3d, nface() AS point3d
SHARED arccos()
SHARED spot  AS point3d, camera AS point3d
SHARED cubex1!, cubey1!, cubez1!, cubex2!, cubey2!, cubez2!, cubex3!, cubey3!, cubez3!
SHARED nv AS INTEGER, nf AS INTEGER
SHARED ddx AS INTEGER, ddy AS INTEGER, zoom AS DOUBLE
SHARED goura
		 a! = COS(xrot!)
		 b! = SIN(xrot!)
		 c! = COS(yrot!)
		 d! = SIN(yrot!)
		 E! = COS(zrot!)
		 f! = SIN(zrot!)
		 AD! = a! * d!
		 BD! = b! * d!
		
		 cubex1! = c! * E!
		 cubey1! = -c! * f!
		 cubez1! = -d!
		
		 cubex2! = -BD! * E! + a! * f!
		 cubey2! = BD! * f! + a! * E!
		 cubez2! = -b! * c!
	 
		 cubex3! = AD! * E! + b! * f!
		 cubey3! = -AD! * f! + b! * E!
		 cubez3! = a! * c!
	 
		'rotate vertexs and normals
	 
		FOR i = 1 TO nv
			rotate vertx(i), rvertx(i)
		NEXT
		FOR i = 1 TO nf
			rotate nface(i), rnface(i)
		NEXT
		FOR i = 1 TO nv
			rotate nvertx(i), rnvertx(i)
		NEXT


		'convert to 2d
		temp1! = zoom * camera.z
		FOR i = 1 TO nv
			temp! = temp1! / (camera.z - rvertx(i).z)
			tx = (ddx + temp! * rvertx(i).x) * subpix
			ty = (ddy + temp! * rvertx(i).y) * subpix
			svertx(i).x = tx
			svertx(i).y = ty
		NEXT
	 
		'Z-order
		FOR i = 1 TO nf
		 oface(i).n = i
		 oface(i).z = rvertx(face(i).p1).z + rvertx(face(i).p2).z + rvertx(face(i).p3).z
		NEXT
		fastsort oface(), 1, nf
		'display
IF NOT goura THEN
		'flat shading
		FOR i = 1 TO nf
			n = oface(i).n
			p1 = face(n).p1
			X1 = svertx(p1).x
			Y1 = svertx(p1).y
			p2 = face(n).p2
			X2 = svertx(p2).x
			Y2 = svertx(p2).y
			p3 = face(n).p3
			X3 = svertx(p3).x
			Y3 = svertx(p3).y
			'backface culling
			IF (CLNG(X1) - X2) * (CLNG(Y3) - Y2) <= (CLNG(Y1) - Y2) * (CLNG(X3) - X2) THEN
			 CLR = arccos(rnface(n).x * spot.x + rnface(n).y * spot.y + rnface(n).z * spot.z)
			 triflat CLR, CLR, CLR, X1, Y1, X2, Y2, X3, Y3
			END IF
	 NEXT
ELSE
		'gouraud shading
		FOR i = 1 TO nf
			n = oface(i).n
			p1 = face(n).p1
			X1 = svertx(p1).x
			Y1 = svertx(p1).y
			p2 = face(n).p2
			X2 = svertx(p2).x
			Y2 = svertx(p2).y
			p3 = face(n).p3
			X3 = svertx(p3).x
			Y3 = svertx(p3).y
			'backface culling
			IF (CLNG(X1) - X2) * (CLNG(Y3) - Y2) <= (CLNG(Y1) - Y2) * (CLNG(X3) - X2) THEN
			 clr1 = arccos(rnvertx(p1).x * spot.x + rnvertx(p1).y * spot.y + rnvertx(p1).z * spot.z)
			 clr2 = arccos(rnvertx(p2).x * spot.x + rnvertx(p2).y * spot.y + rnvertx(p2).z * spot.z)
			 CLR3 = arccos(rnvertx(p3).x * spot.x + rnvertx(p3).y * spot.y + rnvertx(p3).z * spot.z)
			 triGOU clr1, clr1, clr1, X1, Y1, clr2, clr2, clr2, X2, Y2, CLR3, CLR3, CLR3, X3, Y3
			END IF
	 NEXT
END IF
END SUB

DEFSNG A-Z
SUB rotate (p AS point3d, r AS point3d)
SHARED cubex1!, cubey1!, cubez1!, cubex2!, cubey2!, cubez2!, cubex3!, cubey3!, cubez3!
	 r.x = p.x * cubex1! + p.y * cubex2! + p.z * cubex3!
	 r.y = p.x * cubey1! + p.y * cubey2! + p.z * cubey3!
	 r.z = p.x * cubez1! + p.y * cubez2! + p.z * cubez3!
	 
END SUB

DEFINT A-Z
FUNCTION SetVESAMode (mode)
'---------------------------------------------------------
'sets vesa mode, sets line length and makes lookup tables
'does not calculate line len
'returns 0 if all ok
'---------------------------------------------------------
SHARED vlut() AS vluttype, curbank, lasty, slina, slinb, regs AS RegType
SHARED blook() AS INTEGER, glook() AS INTEGER, g2look() AS INTEGER
SHARED rlook() AS INTEGER

CONST pixlen = 1024
CONST linlen = 2048&    ' <<<<<<<<Constant!!!
CONST oflin = linlen / 16
'Set mode
regs.ax = &H4F02
regs.bx = mode
CALL INTERRUPT(&H10, regs, regs)
IF regs.ax <> &H4F THEN SetVESAMode = 2: EXIT FUNCTION
IF mode = 3 THEN EXIT FUNCTION
'Set line Length
regs.ax = &H4F06
regs.bx = 0
regs.cx = pixlen
CALL INTERRUPT(&H10, regs, regs)
IF regs.ax <> &H4F OR regs.cx <> pixlen THEN
	dummy = SetVESAMode(3)
	SetVESAMode = 3: EXIT FUNCTION
END IF
'make line lut
T& = 0
FOR i = 0 TO 959
	vlut(i).page = T& \ &H10000
	vlut(i).ofs = &HA000 + oflin * (i AND 31)
	T& = T& + linlen
NEXT
'make color luts
IF mode = &H111 THEN
	'5:6:5
	FOR i% = 0 TO 255
			blook(i%) = i% \ 8
			glook(i%) = (i% AND &H38) * 8
			g2look(i%) = i% \ 32
			rlook(i%) = i% AND &HF8
	NEXT
ELSE
	 '5:5:5
	 FOR i% = 0 TO 255
		blook(i%) = i% \ 8
		glook(i%) = (i% AND &H38) * 4
		g2look(i%) = i% \ 64
		rlook(i%) = (i% \ 2) AND &H7C
	 NEXT
END IF
slina = 0: slinb = 480
curbank = -1: lasty = -1
END FUNCTION

SUB SVGASwapScreen
SHARED slina, slinb, curbank, gdt() AS LONG, vlut() AS vluttype
SHARED regs AS RegType, regsx AS RegTypeX
	 'swap screens
	 WAIT &H3DA, 8
	 regs.ax = &H4F07
	 regs.bx = &H80 '     &H80         'wait for retrace  linea 5448 ralf brow
	 regs.cx = 0
	 regs.dx = slina
	 CALL INTERRUPT(&H10, regs, regs)
 
	 'clear background page
	 bb = vlut(slinb).page
	 bb1 = bb + 14
	 cc1 = VARSEG(gdt(1))
	 cc2 = VARPTR(gdt(1))
	 'scan svga banks
	 FOR i = bb TO bb1
		 'set a bank
		 regs.ax = &H4F05
		 regs.bx = 0
		 regs.dx = i
		 CALL INTERRUPT(&H10, regs, regs)
		 'copy an array of zeros to it
		 regsx.ax = &H8700
		 regsx.cx = &H8000
		 regsx.es = cc1
		 regsx.si = cc2
		 CALL INTERRUPTX(&H15, regsx, regsx)
	 NEXT
	 'swap fore/back page indexs
	 SWAP slina, slinb
	 curbank = -1
END SUB

SUB triflat (r, g, b, X1, YY1, X2, YY2, X3, YY3)
SHARED vlut() AS vluttype, curbank, lasty, regs AS RegType, slina
SHARED blook() AS INTEGER, glook() AS INTEGER, g2look() AS INTEGER
SHARED rlook() AS INTEGER
	
	'DRAWS TRIANGLE WITH FLAT SHADING
	CONST spmask = subpix - 1
	CONST mscreenx = 619 * subpix
	CONST mscreeny = 479 * subpix
	'
	'reorder coords so yy1<yy2<yy3
	'
	IF YY2 > YY3 THEN SWAP X2, X3: SWAP YY2, YY3
	IF YY1 > YY2 THEN SWAP X1, X2: SWAP YY1, YY2
	IF YY2 > YY3 THEN SWAP X2, X3: SWAP YY2, YY3
	IF YY1 > mscreeny THEN EXIT SUB
	IF YY3 < 0 THEN EXIT SUB
	'
	'find which side is x2,y2 from(x3,y3)-(x1,y1) s=1 right, -1 left
	'
	IF YY1 <> YY3 THEN
			IF X2 > X1 AND X2 > X3 THEN
				s = 1
			ELSEIF X2 < X1 AND X2 < X3 THEN
				s = -1
			ELSE
				s = SGN((CLNG(YY3) - YY1) * (X2 - X1) - (CLNG(YY2) - YY1) * (X3 - X1))
			END IF
	END IF
	'IF s < 0 THEN EXIT SUB
	'
	'calculate Bresenham coefs of sides to calc start and end point of scanlines
	'

	IF s < 0 THEN
		tl = X2 - X1: tr = X3 - X1
		dyl = YY2 - YY1: dyr = YY3 - YY1
	ELSE
	 tl = X3 - X1: tr = X2 - X1
	 dyl = YY3 - YY1: dyr = YY2 - YY1
	END IF
	dxl = ABS(tl): sxl = SGN(tl): el = 0
	dxr = ABS(tr): sxr = SGN(tr): er = 0
	'
	'at the top of triangle scanline both starts and ends at x1

	xx = X1
	xxx = X1
	'
	'for each scanline
	'
	FOR scanline = YY1 TO YY3
	
		'
		'if arrived to y2 we must change Bresenham coeficients of one side
		'
		IF scanline = YY2 THEN
			IF s < 0 THEN
				'
				'if dyl=0, flat bottom, end
				'
				dyl = YY3 - YY2: IF dyl = 0 THEN EXIT SUB
				tl = X3 - X2
				dxl = ABS(tl): sxl = SGN(tl): xx = X2
			ELSE
			 '
			 'if dyr=0, flat bottom, end
			 '
			 dyr = YY3 - YY2: IF dyr = 0 THEN EXIT SUB
			 tr = X3 - X2
			 dxr = ABS(tr): sxr = SGN(tr): xxx = X2
			END IF
		END IF
	
		'
		'if visible then draw scanline
		'
		IF (scanline AND spmask) = 0 THEN
		IF scanline >= 0 THEN
			IF scanline <= mscreeny THEN
				IF xx <= mscreenx THEN
					IF xxx >= 0 THEN
						'
						'clip scanline at screen borders
						'
						xx1 = xx AND &HFFFE
						xxx1 = (xxx - 1) AND &HFFFE
						IF xxx1 > mscreenx THEN xxx1 = mscreenx
						IF xx1 < 0 THEN xx1 = 0
						'
						'draw scanline!
						'
						ll = scanline \ subpix + slina
						IF vlut(ll).page <> curbank THEN
							curbank = vlut(ll).page
							regs.ax = &H4F05
							regs.bx = 0
							regs.dx = curbank
							CALL INTERRUPT(&H10, regs, regs)
						END IF
						DEF SEG = vlut(ll).ofs
						FOR x = xx1 TO xxx1 STEP 2
							POKE x, glook(g) OR blook(b)
							POKE x + 1, rlook(r) OR g2look(g)
							 
						NEXT
					END IF
				END IF
			END IF
		END IF
		END IF
		'
		'Use Bresenham to calculate start of next scanline
		'
		WHILE el < 0: el = el + dyl: xx = xx + sxl: WEND
		el = el - dxl
		'
		'Use Bresenham to calculate end of next scanline
		'
		WHILE er < 0: er = er + dyr: xxx = xxx + sxr: WEND
		er = er - dxr
	NEXT scanline
END SUB

SUB triGOU (r1, g1, b1, X1, YY1, r2, g2, b2, X2, YY2, r3, g3, b3, X3, YY3)
SHARED vlut() AS vluttype, curbank, lasty, regs AS RegType, slina
SHARED blook() AS INTEGER, glook() AS INTEGER, g2look() AS INTEGER
SHARED rlook() AS INTEGER
 
	'DRAWS TRIANGLE WITH GOURAUD SHADING
	CONST spmask = 2 - 1
	CONST mscreenx = 619 * 2
	CONST mscreeny = 479 * 2
	'
	'reorder coords so yy1<yy2<yy3
	'
	IF YY2 > YY3 THEN SWAP X2, X3: SWAP YY2, YY3: SWAP r2, r3: SWAP g2, g3: SWAP b2, b3
	IF YY1 > YY2 THEN SWAP X1, X2: SWAP YY1, YY2: SWAP r1, r2: SWAP g1, g2: SWAP b1, b2
	IF YY2 > YY3 THEN SWAP X2, X3: SWAP YY2, YY3: SWAP r2, r3: SWAP g2, g3: SWAP b2, b3
	IF YY1 > mscreeny THEN EXIT SUB
	IF YY3 < 0 THEN EXIT SUB
	'
	'find which side is x2,y2 from(x3,y3)-(x1,y1) s=1 right, -1 left
	'
	IF YY1 <> YY3 THEN
			IF X2 > X1 AND X2 > X3 THEN
				s = 1
			ELSEIF X2 < X1 AND X2 < X3 THEN
				s = -1
			ELSE
				s = SGN((CLNG(YY3) - YY1) * (X2 - X1) - (CLNG(YY2) - YY1) * (X3 - X1))
			END IF
	END IF
	'IF s < 0 THEN EXIT SUB
	'
	'calculate Bresenham coefs of sides to calc start and end point of scanlines
	'

	IF s < 0 THEN
		dyl = YY2 - YY1 + 1: dyr = YY3 - YY1
		txl = X2 - X1: txr = X3 - X1
		trl = r2 - r1: trr = r3 - r1
		tgl = g2 - g1:  tgr = g3 - g1
		tbl = b2 - b1:  tbr = b3 - b1
	ELSE
	 dyl = YY3 - YY1 + 1: dyr = YY2 - YY1
	 txl = X3 - X1: txr = X2 - X1
	 trl = r3 - r1: trr = r2 - r1
	 tgl = g3 - g1: tgr = g2 - g1
	 tbl = b3 - b1: tbr = b2 - b1
	END IF
	dxl = ABS(txl): sxl = SGN(txl): el = 0: ERrL = 0: EGl = 0: Ebl = 0
	dxr = ABS(txr): sxr = SGN(txr): er = 0: ERRR = 0: EGr = 0: EBr = 0
	drl = ABS(trl): srl = SGN(trl): drr = ABS(trr): srr = SGN(trr)
	dgl = ABS(tgl): sgl = SGN(tgl): dgr = ABS(tgr): sgr = SGN(tgr)
	dbl = ABS(tbl): sbl = SGN(tbl): dbr = ABS(tbr): sbr = SGN(tbr)
	'
	'at the top of triangle scanline both starts and ends at x1
	xx = X1
	xxx = X1
	rr = r1
	gr = g1
	br = b1
	rl = r1
	gl = g1
	bl = b1

	'
	'for each scanline
	'
	FOR scanline = YY1 TO YY3
	 
		'
		'if arrived to y2 we must change Bresenham coeficients of one side
		'
		IF scanline = YY2 THEN
			IF s < 0 THEN
				'
				'if dyl=0, flat bottom, end
				'
				dyl = YY3 - YY2 + 1: IF dyl = 0 THEN EXIT SUB
				trl = r3 - r2: drl = ABS(trl): srl = SGN(trl): rl = r2: ERrL = 0
				tgl = g3 - g2: dgl = ABS(tgl): sgl = SGN(tgl): gl = g2: EGl = 0
				tbl = b3 - b2: dbl = ABS(tbl): sbl = SGN(tbl): bl = b2: Ebl = 0
				tl = X3 - X2: dxl = ABS(tl): sxl = SGN(tl): xx = X2
			ELSE
			 '
			 'if dyr=0, flat bottom, end
			 '
			 dyr = YY3 - YY2 + 1: IF dyr = 0 THEN EXIT SUB
			 trr = r3 - r2: drr = ABS(trr): srr = SGN(trr): rr = r2: ERRR = 0
			 tgr = g3 - g2: dgr = ABS(tgr): sgr = SGN(tgr): gr = g2: EGr = 0
			 tbr = b3 - b2: dbr = ABS(tbr): sbr = SGN(tbr): br = b2: EBr = 0
			 tr = X3 - X2: dxr = ABS(tr): sxr = SGN(tr): xxx = X2
			END IF
		END IF
	 
		'
		'if visible then draw scanline
		'
		IF (scanline AND spmask) = 0 THEN
		IF scanline >= 0 THEN
			IF scanline <= mscreeny THEN
				IF xx <= mscreenx THEN
					IF xxx >= 0 THEN
						'
						'clip scanline at screen borders
						'
						xx1 = xx AND &HFFFE
						xxx1 = xxx AND &HFFFE - 1
						ddx = (xxx1 - xx1) \ 2
						IF xxx1 > mscreenx THEN xxx1 = mscreenx
						'
						'draw scanline!
						'
						ll = scanline \ 2 + slina
						IF vlut(ll).page <> curbank THEN
							curbank = vlut(ll).page
							regs.ax = &H4F05
							regs.bx = 0
							regs.dx = curbank
							CALL INTERRUPT(&H10, regs, regs)
						END IF
						trx = rr - rl: drx = ABS(trx): srx = SGN(trx): r = rl
						tgx = gr - gl: dgx = ABS(tgx): sgx = SGN(tgx): g = gl
						tbx = br - bl: dbx = ABS(tbx): sbx = SGN(tbx): b = bl
						erx = 0: egx = 0: ebx = 0
						
						DEF SEG = vlut(ll).ofs
						FOR x = xx1 TO xxx1 STEP 2
						 
							WHILE erx < 0: erx = erx + ddx: r = r + srx: WEND
							erx = erx - drx
							WHILE egx < 0: egx = egx + ddx: g = g + sgx: WEND
							egx = egx - dgx
							WHILE ebx < 0: ebx = ebx + ddx: b = b + sbx: WEND
							ebx = ebx - dbx
							IF x > 0 THEN
							POKE x, glook(g) OR blook(b)
							POKE x + 1, rlook(r) OR g2look(g)
							END IF
						NEXT
					END IF
				END IF
			END IF
		END IF
		END IF
		'
		'Use Bresenham to calculate start of next scanline
		'
		WHILE el < 0: el = el + dyl: xx = xx + sxl: WEND
		el = el - dxl
		WHILE ERrL < 0: ERrL = ERrL + dyl: rl = rl + srl: WEND
		ERrL = ERrL - drl
		WHILE EGl < 0: EGl = EGl + dyl: gl = gl + sgl: WEND
		EGl = EGl - dgl
		WHILE Ebl < 0: Ebl = Ebl + dyl: bl = bl + sbl: WEND
		Ebl = Ebl - dbl
		'
		'Use Bresenham to calculate end of next scanline
		'
		WHILE er < 0: er = er + dyr: xxx = xxx + sxr: WEND
		er = er - dxr
		WHILE ERRR < 0: ERRR = ERRR + dyr: rr = rr + srr: WEND
		ERRR = ERRR - drr
		WHILE EGr < 0: EGr = EGr + dyr: gr = gr + sgr: WEND
		EGr = EGr - dgr
		WHILE EBr < 0: EBr = EBr + dyr: br = br + sbr: WEND
		EBr = EBr - dbr

	NEXT scanline

END SUB

DEFSNG A-Z
 FUNCTION XTimerW& STATIC
'returns time in PIT ticks modulo 30 min
'Slow but safe to use in a DOS box in Windows as in plain DOS
CONST forty = &H40
CONST byte = 256&
		DEF SEG = forty
		IF NOT ini% THEN OUT &H43, &H0: ini% = -1
		
				T& = INP(forty)
				tpic& = 65536 - (INP(forty) * byte + T&)
xtimerloop:
				T& = PEEK(&H6D)
				ttimer& = PEEK(&H6C) + byte * T&
				IF tpic& < ltpic& THEN IF ttimer& = lttimer& THEN GOTO xtimerloop
				SWAP tpic&, ltpic&: SWAP lttimer&, ttimer&
				XTimerW& = (lttimer& AND &H7FFF) * 65536 + ltpic&
END FUNCTION

