DECLARE FUNCTION filsel$ (ext$)
DECLARE FUNCTION EmsAllocp% (numPages%)
DECLARE FUNCTION EMSCopy% ()
DECLARE FUNCTION emsfreep% (handle%)
DECLARE FUNCTION EMSMapP% (frame%, EmsPage%, handle%)
DECLARE FUNCTION EMSSwap% ()
DECLARE SUB setclip (x0%, y0%, x1%, y1%)
DECLARE SUB clipit (x0%, y0%, xs%, ys%, x00%, y00%, x11%, y11%)
DECLARE FUNCTION savegif% (name$, x1%, y1%, xs%, ys%)
DECLARE FUNCTION MFilesel$ (txt$)
DECLARE FUNCTION MouReadFmScrn$ (x0%, y0%, x1%, y1%)
'---------------------------------------------------------------------------
'Program: ANIMATED GIF VIEWER
'By     : Antoni Gual  14-12-2002 agual@eic.ictnet.es
'---------------------------------------------------------------------------
'Rich Geldreich made his pure QB GIF viewer back in 1993. Rich's program
'stopped halfway into the 89a spec, it displayed  all frames of an animated
'gif in succesion without proper timing. It lacked also a transparent color,
'and looping. No one has gone further since then.
'So I decided to do my version: featuring SVGA and full animated GIFS, a
'step-by step mode, a frame saver in PUT format and a mouse file selector.
'----------------------------------------------------------------------------

'PARTS OF GIF89A STANDARD NOT IMPLEMENTED IN THIS VERSION:
'   Revert to previous image as disposal method. It would need a buffer
'   User triggered images. Bah!
'   Viewable Text extensions .Who uses it?

'TO DO:
'ok Added MODE 13
'ok Redo the gif decode routine (not Rich's)
'ok Merge the microsecond timer
'ok Implement Global palette saving
'ok Revert to straight GET in decodegif, so i can put getbyte inside lzwdecode,
'ok Try a variable length buffer for lzwdwecode--doubled speed!!
'ok Implement Netscape looping App extension
'ok Implement user looping and image stop
'ok Center image
'OK File selector Abort-Retry should be mouse selected
'   Better user interface
'   A memory buffer to reduce flickering and allow return to previous image
'   Add GIF parameters viewer
'   Export to screen13
'   don't like palette compressed to 64 values...
'   An User Help

'
'KNOWN BUGS
'   opr006ar.gif black stripe in one frame. Would disappear with a buffer..
'   Mousefilesel scrolls if dir too big. IT'S A FEATURE! :D
'----------------------------------------------------------------------------
DEFINT A-Z
'$INCLUDE: 'QB.BI'
DECLARE FUNCTION DelayOrStep% (delay%)
DECLARE SUB lzwdecode (xstart, ystart, xlength, ylength, INTERLACED, TRANSP)
DECLARE FUNCTION setvesamode% (mode%)
DECLARE FUNCTION getbyte% ()
DECLARE FUNCTION decodegif (a$)
DECLARE FUNCTION Driveready% (d$)
DECLARE FUNCTION mouseint% (func%, c%, r%)
DECLARE SUB setvesay (y)

DECLARE FUNCTION XTimerW& ()
'You can use these constants to convert XTimer readings to seconds
CONST clockfreq# = 1193181.666#           'PIT clock frequency
CONST hundsofsec# = clockfreq# / 100

TYPE emstype                         'general ems data: frame segment, free pag
		init AS INTEGER                  'if init=0 EMS has not been initialized
		ver  AS INTEGER
		sg   AS INTEGER
		frep  AS INTEGER
		totp  AS INTEGER
END TYPE

TYPE memcopytype                     'this reg must be filled before a copy
		length AS LONG
		srctyp AS STRING * 1
		SrcHndl AS INTEGER
		SrcOfFs AS INTEGER
		SrcSgPg AS INTEGER
		dsttyp AS STRING * 1
		dsthndl AS INTEGER
		dstOfFs AS INTEGER
		dstSgPg AS INTEGER
END TYPE


DIM ems  AS emstype
DIM Emscopya AS memcopytype, emsbacka AS memcopytype
 DIM EmsPag(3)  AS LONG                  'saves the offsets of the 4 frames

TYPE skreentype
	minxs AS INTEGER
	minys AS INTEGER
	maxxs AS INTEGER
	maxys AS INTEGER
	minx AS INTEGER
	maxx AS INTEGER
	miny AS INTEGER
	maxy AS INTEGER
	linlen AS INTEGER
	mode AS INTEGER
END TYPE


TYPE gifheadertype
	sig AS STRING * 3
	ver AS STRING * 3
	wid AS INTEGER
	hei AS INTEGER
	pak AS STRING * 1
	BGI AS STRING * 1
	par AS STRING * 1
END TYPE

TYPE grctrlexttype
	siz AS STRING * 1
	pak AS STRING * 1
	DEL AS INTEGER
	TCI AS STRING * 1
	NUL AS STRING * 1
END TYPE

TYPE gifimdesctype
	lef AS INTEGER
	top AS INTEGER
	wid AS INTEGER
	hei AS INTEGER
	pak AS STRING * 1
END TYPE

TYPE textdesctype
	siz AS STRING * 1
	wid AS INTEGER
	hei AS INTEGER
	cwi AS STRING * 1
	the AS STRING * 1
	frm AS STRING * 1
	bgr AS STRING * 1
END TYPE

TYPE appexttype
 siz AS STRING * 1
 net AS STRING * 11
END TYPE

TYPE GIFLOOPTYPE
 XXX  AS INTEGER
 lop  AS INTEGER
 NUL  AS STRING * 1
END TYPE

'vesa handling
TYPE vluttype
	page AS INTEGER
	ofs  AS INTEGER
END TYPE
REDIM vlut(0) AS vluttype


'Precalculate power of two tables for fast shifts.
DIM powersof2(30)  AS LONG, shiftout(8)
DIM SHL8(511) AS LONG
FOR a = 0 TO 30: powersof2(a) = 2 ^ a: NEXT
FOR a = 0 TO 255: SHL8(a) = a * 256&: SHL8(a + 256) = SHL8(a) * 256: NEXT



'Screen mode
retry:
SCREEN 0: COLOR 15, 0: CLS
INPUT "screen mode 1 screen 13 /2 640X480 /3 800X600 4/1024x768 "; a
SELECT CASE a
CASE 1: mmod = &H13
CASE 2: mmod = &H101
CASE 3: mmod = &H103
CASE 4: mmod = &H105
CASE ELSE: GOTO retry
END SELECT

'Get GIF filename.
F$ = COMMAND$
DO
	IF F$ = "" THEN F$ = filsel$("gif")
	COLOR 15, 0
	IF F$ = "" THEN EXIT DO
	'Open file for input so QB stops with an error if it doesn't exist.
	OPEN F$ FOR INPUT ACCESS READ AS #1: CLOSE #1

	IF setvesamode(mmod) THEN STOP
	'wait for monitor to adapt to new mode
	T! = TIMER + .2: WHILE TIMER < T!: WEND
	T! = TIMER
	errc = decodegif(F$)
	T! = TIMER - T!
	IF errc = 0 THEN
		DO: LOOP UNTIL LEN(INKEY$)
		dummy = setvesamode(3)
		SCREEN 0: WIDTH 80, 25: COLOR 15, 0: CLS
		PRINT T!
	ELSE
		dummy = setvesamode(3)
		SCREEN 0: WIDTH 80, 25: COLOR 15, 0: CLS
		SELECT CASE errc
		CASE 1: PRINT "File "; F$; " is not a GIF image."
		END SELECT
	END IF
	DO: LOOP UNTIL LEN(INKEY$)
	F$ = ""
LOOP
COLOR 7, 0: CLS
PRINT "Pure QuickBasic Animated GIF Viewer"
PRINT "Antoni Gual 12/2002   agual@eic.ictnet.es"
END

nofile: errata% = ERR: RESUME NEXT

SUB clipit (x0, y0, xs, ys, x00, y00, x11, y11)
SHARED sk AS skreentype
	IF x0 < sk.minx THEN x00 = sk.minx ELSE x00 = x0
	IF y0 < sk.miny THEN y00 = sk.miny ELSE y00 = y0
	IF x0 + xs - 1 > sk.maxx THEN x11 = sk.maxx ELSE x11 = x0 + xs - 1
	IF y0 + ys - 1 > sk.maxy THEN y11 = sk.maxy ELSE y11 = y0 + ys - 1
END SUB

FUNCTION decodegif (a$)
SHARED powersof2() AS LONG
SHARED jfile, sk AS skreentype
	DIM w AS LONG

	jfile = FREEFILE
	OPEN a$ FOR BINARY ACCESS READ AS #jfile
	DIM hdr AS gifheadertype, id AS gifimdesctype, ge AS grctrlexttype
	DIM app AS appexttype, loopit AS GIFLOOPTYPE
	'Get header and work with it...
	'Check to see if GIF file. Ignore GIF version number.
	GET jfile, , hdr
	IF hdr.sig <> "GIF" THEN CLOSE : decodegif = 1: EXIT FUNCTION

	xl = (sk.maxx - sk.minx - hdr.wid + 1) \ 2
	yt = (sk.maxy - sk.minx - hdr.hei + 1) \ 2

	globpal = (ASC(hdr.pak) AND 128) <> 0
	'Retrieve global palette if it exists.
	IF globpal THEN
		'the screen is preset to color 0 so no need to draw background if it's 0
		IF ASC(hdr.BGI) <> 0 THEN
			clipit xl, yt, hdr.wid, hdr.wid, nx, ny, mx, my
			GOSUB SETBACKG
		END IF
		globcolors = powersof2((ASC(hdr.pak) AND 7) + 1)
		globpal$ = SPACE$(globcolors * 3)
		GET jfile, , globpal$
		globpalset = 0
	ELSE
		globpalset = -1
	END IF

	'Variable part...
	g$ = " ": g1$ = " ": g2$ = " "
	TRANSP = -1: loops = 0: loopptr& = SEEK(jfile)
	DO
		'To cope with files without End Of Gif
		IF EOF(jfile) THEN g$ = CHR$(59) ELSE GET jfile, , g$
		SELECT CASE ASC(g$)
		CASE 0        'end of block
		CASE 33       'extension
			GET jfile, , g1$
			SELECT CASE ASC(g1$)
			CASE 1        'plain text extension
				GOSUB skipit
			CASE 249      'graphic ctrl ext
				'get delay , transparent color and erase command for next image
				GET jfile, , ge
				IF (ASC(ge.pak) AND 1) <> 0 THEN TRANSP = ASC(ge.TCI) ELSE TRANSP = -1
				IF (ASC(ge.pak) AND 8) THEN nexteraser = 1
				delay = ge.DEL: IF delay = 0 THEN delay = 1
			CASE 254      'comment
				GOSUB skipit
			CASE 255      'app extension. Have to check for netscape loop
				GET jfile, , app
				IF app.net = "NETSCAPE2.0" THEN
					 GET jfile, , loopit
					 IF loopit.lop = 0 THEN loopit.lop = -1
					 IF loops = 0 THEN loops = loopit.lop
				ELSE
					GOSUB skipit
				END IF
			CASE ELSE
				STOP
			END SELECT
		CASE 44       'image descriptor+local palette+image
			GET jfile, , id
			INTERLACED = (ASC(id.pak) AND 64) <> 0

			'if local color table exists, then get it
			locpalflag = (ASC(id.pak) AND 128) <> 0
			IF locpalflag THEN
				loccolors = powersof2((ASC(id.pak) AND 7) + 1)
				locpal$ = SPACE$(loccolors * 3)
				GET jfile, , locpal$
			END IF

			'deal with delays,stepping
			IF DelayOrStep(delay) THEN EXIT DO

			'if previous image must be erased, do it
			IF ERASER THEN GOSUB SETBACKG: ERASER = 0
			IF locpalflag THEN
				GOSUB setpalette
				IF globpalset = 1 THEN globpalset = 0
			ELSEIF globpalset = 0 THEN
				locpal$ = globpal$
				loccolors = globcolors
				GOSUB setpalette
				globpalset = 1
			END IF
			'decode image to screen
			lzwdecode xl + id.lef, yt + id.top, id.wid, id.hei, INTERLACED, TRANSP

			TRANSP = -1:
			ERASER = nexteraser: nexteraser = 0
			IF ERASER THEN clipit id.lef + xl, id.top + yt, id.wid, id.hei, nx, ny, mx, my
		CASE 59       'end of gif

			SELECT CASE loops
			CASE -1: SEEK jfile, loopptr&
			CASE 0: EXIT DO
			CASE ELSE: loops = loops - 1: SEEK jfile, loopptr&
			END SELECT
		CASE ELSE
			STOP  'Wrong GIF Sequence!
		END SELECT
	LOOP
 CLOSE jfile
EXIT FUNCTION

skipit:
 DO
	GET jfile, , g2$
	bl = ASC(g2$)
	IF bl THEN
	 a$ = SPACE$(bl)
	 GET jfile, , a$
	END IF
 LOOP UNTIL bl = 0
RETURN
EXIT FUNCTION

SETBACKG:
p = ASC(hdr.BGI)
WAIT &H3DA, 8
FOR i = ny TO my
setvesay i
FOR j = nx TO mx
	POKE j, p
NEXT
NEXT
RETURN

setpalette:
OUT &H3C8, 0
FOR i = 1 TO loccolors * 3:
T = ASC(MID$(locpal$, i)) / 4: IF T > 63 THEN T = 63
OUT &H3C9, T
NEXT
RETURN



END FUNCTION

FUNCTION DelayOrStep (delay)
 'Returns after delay expired or user pressed a key
 STATIC PrevDelay&, steps
	IF PrevDelay& THEN
		DO
		 
			IF LEN(INKEY$) OR click AND 1 THEN DelayOrStep = 1
			IF click AND 2 THEN : steps = NOT steps: BEEP
		LOOP UNTIL (XTimerW& > PrevDelay&) OR click
		IF click THEN T! = TIMER + .1: DO: LOOP UNTIL TIMER > T!
		PrevDelay& = 0
	ELSEIF steps THEN
		click = 0
		DO
		 
			IF click AND 2 THEN steps = NOT steps: BEEP
		LOOP UNTIL (INKEY$ = " ") OR click
		T! = TIMER + .1: DO: LOOP UNTIL TIMER > T!
	END IF
	'if this image has also a delay, start it
	IF NOT steps THEN
		IF delay THEN PrevDelay& = XTimerW& + delay * hundsofsec#: delay = 0
	END IF
END FUNCTION

FUNCTION EmsAllocp (numPages)
'Allocate pages. First time it's called it checks for EMS
'Returns negative values if error, positive if a handler has been assigned
SHARED ems  AS emstype, regs AS regtype, EmsPag() AS LONG
IF NOT ems.init THEN
	 '-1 handler EMS not avialable
	 regs.ax = &H4000
	 CALL INTERRUPT(&H67, regs, regs)
	 IF regs.ax \ 256 THEN EmsAllocp = -1: EXIT FUNCTION


	 regs.ax = &H4200
	 CALL INTERRUPT(&H67, regs, regs)
	 ems.frep = regs.bx
	 ems.totp = regs.dx
	 IF regs.ax \ 256 THEN EmsAllocp = -2: EXIT FUNCTION

	 GOSUB freep

	 regs.ax = &H4600
	 CALL INTERRUPT(&H67, regs, regs)
	 ems.ver = regs.ax AND 255
	 IF regs.ax \ 256 THEN EmsAllocp = -5: EXIT FUNCTION



	 regs.ax = &H4100
	 CALL INTERRUPT(&H67, regs, regs)
	 ems.sg = regs.bx
	 IF regs.ax \ 256 THEN EmsAllocp = -3: EXIT FUNCTION

	 T& = 0
	 FOR i% = 0 TO 3
			EmsPag(i%) = T&
			T& = T& + &H4000
	 NEXT
	 ems.init = -1
END IF

IF ems.frep = 0 THEN EXIT FUNCTION

regs.ax = &H4300
regs.bx = numPages
CALL INTERRUPT(&H67, regs, regs)
IF (regs.ax \ 256) THEN
	 res = -4
ELSE
	 res = regs.dx
END IF

GOSUB freep
EmsAllocp = res
EXIT FUNCTION

freep:
	 regs.ax = &H4200
	 CALL INTERRUPT(&H67, regs, regs)
	 ems.frep = regs.bx
	 ems.totp = regs.dx
	 IF regs.ax \ 256 THEN EmsAllocp = -2: EXIT FUNCTION
RETURN

END FUNCTION

'' BIOS call to run an extremely quick memory copy routine that is provided by
' EMS.
FUNCTION EMSCopy
SHARED Emscopya  AS memcopytype, regsx AS regtypex
regsx.ax = &H5700
regsx.ds = VARSEG(Emscopya)
regsx.si = VARPTR(Emscopya)
CALL INTERRUPTX(&H67, regsx, regsx)
EMSCopy = regsx.ax \ 256
END FUNCTION

' BIOS call to deallocate EMS pages.
FUNCTION emsfreep (handle)
SHARED regs AS regtype
regs.ax = &H4500
regs.dx = handle
CALL INTERRUPT(&H67, regs, regs)
emsfreep = regs.ax \ 256

END FUNCTION

' BIOS call to map EMS to conventional memory.
FUNCTION EMSMapP (frame, EmsPage, handle)
SHARED regs AS regtype
regs.ax = &H4400 OR (frame AND 3)
regs.bx = EmsPage
regs.dx = handle
CALL INTERRUPT(&H67, regs, regs)
EMSMapP = regs.ax \ 256
END FUNCTION

FUNCTION EMSSwap
SHARED Emscopya  AS memcopytype, regsx AS regtypex
IF Emscopya.SrcHndl = 0 THEN Emscopya.srctyp = CHR$(0) ELSE Emscopya.srctyp = CHR$(1)
IF Emscopya.dsthndl = 0 THEN Emscopya.dsttyp = CHR$(0) ELSE Emscopya.dsttyp = CHR$(1)
regsx.ax = &H5701
regsx.ds = VARSEG(Emscopya)
regsx.si = VARPTR(Emscopya)
CALL INTERRUPTX(&H67, regsx, regsx)
EMSSwap = regsx.ax \ 256
END FUNCTION

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
		filsel$ = "": EXIT FUNCTION
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
				filsel$ = fold$ + ff$: CHDIR curfolder$: EXIT FUNCTION
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

END FUNCTION

DEFINT A-Z
SUB lzwdecode (xstart, ystart, xsize, ysize, INTERLACED, TRANSP)
	'We save the strings in string table as a PrefixCode + a SuffixChar.
	' The Prefix code is an index to a new pair PrefixCode+SuffixChar, and so
	' on until the PrefixCode is -1. This avoids using variable length strings.

	SHARED powersof2() AS LONG, SHL8() AS LONG, shiftout()
	SHARED jfile
	SHARED sk AS skreentype
	DIM prefixcod(4095), suffixchar(4095), outstack(4095)
	xmax = xstart + xsize - 1
	ymax = ystart + ysize - 1
	'Calculate clear code, end of stream code, and first free LZW code.
	g$ = " ": GET jfile, , g$
	aaa = ASC(g$)
	clearcode = powersof2(aaa)
	eoscode = clearcode + 1
	FOR i = 0 TO clearcode + 1: prefixcod(i) = -1: suffixchar(i) = i: NEXT
	FirstCode = clearcode + 2
	StartCodeSize = aaa + 1
	startmaxcode = powersof2(StartCodeSize) - 1

	'init Screen output GOSUB
	PassNumber = 0: PassStep = 8
	x = xmax + 1
	IF INTERLACED THEN y = ystart - PassStep ELSE y = ystart - 1

	'pointers to bit fetchers
	bitsin = 0: blockpointer = 0

	'LZW decoding loop.
	codesize = StartCodeSize
	'WAIT &H3DA, 8
	DO
		'Retrieve one LZW code.
		GOSUB getcode
		SELECT CASE code
		CASE -1
			'End also if getcode found a zero-sized block
			EXIT DO
		CASE eoscode
			'not all images end by a clear EOS code..
			EXIT DO
		CASE clearcode
			WHILE code = clearcode
			 'clear dictionnary, reset code length
			 codesize = StartCodeSize
			 nextcode = FirstCode
			 maxcode = startmaxcode
			 'reinit the code stack
			 GOSUB getcode
			 IF code <> clearcode THEN
					IF code = eoscode THEN EXIT DO
					outstack(0) = suffixchar(code)
					sptr = 0
					GOSUB display
					old = code
			 END IF
			WEND
		'a normal code
		CASE ELSE
			IF code < nextcode THEN
				sptr = -1: i = code
				GOSUB getoutput
			ELSEIF code = nextcode THEN
				i = old: sptr = 0
				GOSUB getoutput
				outstack(0) = outstack(sptr)
			ELSE
				'code can't be bigger than next free code or we would have a hole in
				'the dictionnary
				 STOP
			END IF
			'Display it
			GOSUB display
		END SELECT
	LOOP
ImageDone:
ERASE prefixcod, suffixchar, outstack
EXIT SUB

getoutput:
	'build the output string
	WHILE i > -1
		sptr = sptr + 1
		outstack(sptr) = suffixchar(i)
		i = prefixcod(i)
	WEND
	IF nextcode > 4095 THEN RETURN
	'add a new entry to dictionnary
	prefixcod(nextcode) = old
	suffixchar(nextcode) = outstack(sptr)
	old = code
	'increase the dictionnary pointer
	nextcode = nextcode + 1
	'If we reached maximum value for a given codesize, increase codesize
	'if we're not at maximum size (a clearcode should arrive soon)
	IF nextcode > maxcode AND codesize < 12 THEN codesize = codesize + 1: maxcode = powersof2(codesize) - 1
RETURN


'Adds the string just decoded to display
display:
	'read backwards the output stack
	WHILE sptr > -1
		IF x > xmax THEN
			IF INTERLACED THEN
				y = y + PassStep
					IF y > ymax THEN
							PassNumber = PassNumber + 1
							SELECT CASE PassNumber
							CASE 1: y = ystart + 4: PassStep = 8
							CASE 2: y = ystart + 2: PassStep = 4
							CASE 3: y = ystart + 1: PassStep = 2
							END SELECT
					END IF
			ELSE
					y = y + 1
			END IF
			IF y >= sk.miny AND y <= sk.maxy THEN setvesay y
			x = xstart
		END IF
		IF x >= sk.minx AND x <= sk.maxx AND y >= sk.miny AND y <= sk.maxy THEN
			IF outstack(sptr) <> TRANSP THEN POKE x, outstack(sptr)
		END IF
		x = x + 1
		sptr = sptr - 1
	WEND
RETURN

'Reads a codesize sized code from the data stream,skipping block headers.
getcode:
	pad& = 0: blocksize = 0: j = 0
	IF bitsin THEN pad& = workcode: blocksize = bitsin
	WHILE blocksize < codesize

		'if end of block get following block size
		IF blockpointer = 0 THEN
			 IF EOF(jfile) THEN code = -1: RETURN
			 GET jfile, , g$: blockpointer = ASC(g$)
			 'found a zero size block, end (not in the spec!!)
			 IF blockpointer = 0 THEN code = -1: RETURN
			 IF blockpointer <> lbp THEN g1$ = SPACE$(blockpointer): lbp = blockpointer
			 GET jfile, , g1$: ii = 1
		END IF
		workcode = ASC(MID$(g1$, ii, 1)): blockpointer = blockpointer - 1: ii = ii + 1
		pad& = pad& OR SHL8(workcode + j)
		j = j + 256
		blocksize = blocksize + 8
	WEND

	'shift right and mask code
	code = (pad& \ powersof2(8 - bitsin)) AND (powersof2(codesize) - 1)
	IF blocksize >= codesize THEN bitsin = blocksize - codesize ELSE bitsin = 0
RETURN

END SUB

SUB setclip (x0, y0, x1, y1)
SHARED sk AS skreentype
IF x0 < 0 THEN sk.minx = sk.minxs ELSE sk.minx = x0
IF y0 < 0 THEN sk.miny = sk.minys ELSE sk.miny = x0
IF x1 < 0 THEN sk.maxx = sk.maxxs ELSE sk.maxx = x1
IF y1 < 0 THEN sk.maxy = sk.maxys ELSE sk.maxy = y1
END SUB

SUB setEMSy
SHARED vlut() AS vluttype, regs AS regtype, sk AS skreentype
STATIC curbank, CURY
	 IF y = CURY THEN EXIT SUB
	 CURY = y
		DEF SEG = vlut(y).ofs
		IF sk.mode < 256 THEN EXIT SUB
		IF vlut(y).page <> curbank THEN
			curbank = vlut(y).page
			regs.ax = &H4F05: regs.bx = 0: regs.dx = curbank
			CALL INTERRUPT(&H10, regs, regs)
		END IF
END SUB

FUNCTION setvesamode (mode)
'---------------------------------------------------------
'sets vesa mode, sets line length and makes lookup tables
'does not calculate line len
'returns 0 if all ok
'---------------------------------------------------------
SHARED vlut() AS vluttype, regs AS regtype, sk AS skreentype
SELECT CASE mode
CASE 3
	regs.ax = &H4F02
	regs.bx = 3
	CALL INTERRUPT(&H10, regs, regs)
	SCREEN 0
	REDIM vlut(0) AS vluttype
	EXIT FUNCTION
CASE &H101
	sk.maxxs = 639
	sk.minxs = 0
	sk.maxys = 479
	sk.minys = 0
	sk.linlen = 1024
	sk.mode = &H101
	'set mode
	regs.ax = &H4F02
	regs.bx = mode
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F THEN setvesamode = 2: EXIT FUNCTION

	'Set line Length
	regs.ax = &H4F06
	regs.bx = 0
	regs.cx = sk.linlen
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F OR regs.cx <> sk.linlen THEN
		dummy = setvesamode(3)
		setvesamode = 3: EXIT FUNCTION
	END IF
CASE &H103
	sk.maxxs = 799
	sk.minxs = 0
	sk.maxys = 599
	sk.minys = 0
	sk.linlen = 1024
	sk.mode = &H103
	'set mode
	regs.ax = &H4F02
	regs.bx = mode
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F THEN setvesamode = 2: EXIT FUNCTION

	'Set line Length
	regs.ax = &H4F06
	regs.bx = 0
	regs.cx = sk.linlen
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F OR regs.cx <> sk.linlen THEN
		dummy = setvesamode(3)
		setvesamode = 3: EXIT FUNCTION
	END IF
CASE &H105
	sk.maxxs = 1023
	sk.minxs = 0
	sk.maxys = 767
	sk.minys = 0
	sk.linlen = 1024
	sk.mode = &H105
	'set mode
	regs.ax = &H4F02
	regs.bx = mode
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F THEN setvesamode = 2: EXIT FUNCTION

	'Set line Length
	regs.ax = &H4F06
	regs.bx = 0
	regs.cx = sk.linlen
	CALL INTERRUPT(&H10, regs, regs)
	IF regs.ax <> &H4F OR regs.cx <> sk.linlen THEN
		dummy = setvesamode(3)
		setvesamode = 3: EXIT FUNCTION
	END IF

CASE &H13
	sk.maxxs = 319
	sk.minxs = 0
	sk.maxys = 199
	sk.minys = 0
	sk.linlen = 320
	sk.mode = &H13
	SCREEN 13
CASE ELSE
	setvesamode = 4: EXIT FUNCTION
END SELECT
setclip -1, -1, -1, -1
'make line lut
oflin& = sk.linlen \ 16
T& = 0
REDIM vlut(sk.miny TO sk.maxy) AS vluttype
FOR i = sk.miny TO sk.maxy
	vlut(i).page = T& \ &H10000
	vlut(i).ofs = &HA000 + ((oflin& * i) AND &HFFF)
	T& = T& + sk.linlen
NEXT

setvesay sk.maxy

END FUNCTION

SUB setvesay (y)
SHARED vlut() AS vluttype, regs AS regtype, sk AS skreentype
STATIC curbank, CURY
	 IF y = CURY THEN EXIT SUB
	 CURY = y
		DEF SEG = vlut(y).ofs
		IF sk.mode < 256 THEN EXIT SUB
		IF vlut(y).page <> curbank THEN
			curbank = vlut(y).page
			regs.ax = &H4F05: regs.bx = 0: regs.dx = curbank
			CALL INTERRUPT(&H10, regs, regs)
		END IF
END SUB

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

