DECLARE SUB print6x5 (n AS INTEGER, clr AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, dohex AS INTEGER)
DECLARE FUNCTION monthname$ (m%)
DECLARE SUB today (Y%, m%, D%)
'Calendrical library
'julian to gregorian by  David Williams 2000

DECLARE FUNCTION Jday& (Y%, m%, D%)
DECLARE FUNCTION daysinmonth% (Y%, m%)
DECLARE FUNCTION isdate$ (Y%, m%, D%)
DECLARE FUNCTION ISLEAPYEAR% (Y%)
DECLARE SUB j2g (j&, Y%, m%, D%)
DECLARE FUNCTION DayofWeek% (Y%, m%, D%)
DEFINT A-Z
CONST pi2 = 6.283185
CLS
PRINT "Biorythm Calculator": PRINT
PRINT "Today is "; DATE$
CALL today(yt%, mt%, dt%)
jt& = Jday&(yt%, mt%, 1)
dm% = daysinmonth%(yt%, mt%)

IF 0 THEN 'for test
	y0% = 1958: m0% = 5: d0% = 15
ELSE
DO
	INPUT "Birth date YYYY,MM,DD "; y0%, m0%, d0%
	IF y0% = 0 THEN END
	a$ = isdate$(y0%, m0%, d0%): PRINT a$
LOOP UNTIL a$ = "Ok"
END IF
j0& = Jday&(y0%, m0%, d0%)
dj& = jt& - j0&
i23! = (dj& MOD 23&) / 23 * pi2
i28! = (dj& MOD 28&) / 28 * pi2
i33! = (dj& MOD 33&) / 33 * pi2
SCREEN 12
PRINT "Yout Biorythm for " + monthname$(mt%) + " " + STR$(yt%)
PRINT "Birth date " + STR$(y0%) + " " + monthname$(m0%) + " " + STR$(d0%)
PRINT
COLOR 10: PRINT "Physical"
COLOR 11:  PRINT "Emotional"
COLOR 12: PRINT "Intellectual"


FOR i% = 1 TO dm%
CALL print6x5(i%, 15, 170, (i% - .6) * 640 / dm%, 0)
NEXT
WINDOW (.5, 2.5)-(dm% + .5, -1.5)
LINE (1, 0)-(dm%, 0), 15
LINE (1, -1)-(1, 1), 15, , &H3333

FOR i% = 2 TO dm%
LINE (i%, -1)-(i%, 1), 15, , &H3333

LINE (i% - 1, SIN((i% - 1) / 23 * pi2 + i23!))-(i%, SIN(i% / 23 * pi2 + i23!)), 10
LINE (i% - 1, SIN((i% - 1) / 28 * pi2 + i28!))-(i%, SIN(i% / 28 * pi2 + i28!)), 11
LINE (i% - 1, SIN((i% - 1) / 33 * pi2 + i33!))-(i%, SIN(i% / 33 * pi2 + i33!)), 12
NEXT
SLEEP
END

myfont:
DATA  6 , 9 , 9 , 9 , 6     : REM 0
DATA  2 , 6 , 2 , 2 , 2
DATA  6 , 9 , 2 , 4 , 15
DATA  14 , 1 , 6 , 1 , 14
DATA  1 , 3 , 5 , 15 , 1
DATA  15 , 8 , 14 , 1 , 14
DATA  6 , 8 , 14 , 9 , 6
DATA  15 , 1 , 2 , 4 , 8
DATA  6 , 9 , 6 , 9 , 15
DATA  6 , 9 , 7 , 1 , 6     : REM 9
DATA  4 , 10 , 10 , 15 , 9   : REM A
DATA  14 , 9 , 14 , 9 , 14
DATA  7 , 9 , 8 , 8 , 7
DATA  12 , 10 , 9 , 9 , 14
DATA  15 , 8 , 14 , 8 , 15
DATA  7 , 8 , 14 , 8 , 8    : REM F
DATA  0 , 0 , 15 , 0 , 0    : REM -
DATA  4 , 4 , 14 , 4 , 4    : REM +
DATA  0 , 0 , 0 , 0 , 6     : REM .



FUNCTION DayofWeek% (Y%, m%, D%) 'returns the day of week of a date 0=monday
	'Zeller's congruence (it's in Wikipedia)
	
	DIM p AS INTEGER, Q AS INTEGER

	p = m% + (12 AND (m% < 3))   ' march to dec , jan=13,feb=14
	Q = Y% - (1 AND (m% < 3))    'substract 1 to year if month is jan or feb
 'dow = (D% + CINT(30.6001 * p) + Q + (Q \ 4) - (Q \ 100) + (Q \ 400)) MOD 7
	'hacked formula!
	dow = (D% + INT(2.57 * p) + (1 AND (p = 14)) + Q + (Q \ 4) - (Q \ 100) + (Q \ 400)) MOD 7

	'this is  to be able to start the week on sunday or on monday
	DayofWeek = (dow + 1 + sun1dow) MOD 7'+ 1
END FUNCTION

FUNCTION daysinmonth% (Y%, m%)  'returns nr of days in a month of a year
	SELECT CASE m%
		CASE 2: daysinmonth% = 28 + (1 AND ISLEAPYEAR%(Y%))
		CASE 4, 6, 9, 11: daysinmonth% = 30
		CASE ELSE: daysinmonth% = 31
	END SELECT
END FUNCTION

FUNCTION isdate$ (Y%, m%, D%)  'y m d is a correct date?
	IF Y% < 0 THEN isdate$ = "Bad year": EXIT FUNCTION
	IF m% < 1 OR m% > 12 THEN isdate$ = "Bad month": EXIT FUNCTION
	IF D% < 1 OR D% > daysinmonth%(Y%, m%) THEN isdate$ = "Bad Monthday": EXIT FUNCTION
	isdate$ = "Ok"
END FUNCTION

FUNCTION ISLEAPYEAR% (Y%) 'Returns -1 if Gregorian year y is a leap year
	ISLEAPYEAR% = ((Y% AND 3) = 0) AND (NOT ((Y% MOD 100) = 0) OR ((Y% MOD 400) = 0))
END FUNCTION

DEFLNG A-Z
SUB j2g (j&, Y%, m%, D%)   'returns a y-m-d date from julian day
	n = j - 1721120
 
	a1 = n \ 146097
	n = n MOD 146097
	a2 = n \ 36524 - (1 AND ((n \ 36524) = 4))'
	n = n - (a2 * 36524)
	a3 = n \ 1461
	n = n MOD 1461
	a4 = n \ 365 - (1 AND ((n \ 365) = 4))'
	n = n - (a4 * 365)'
	a = 400 * a1 + 100 * a2 + 4 * a3 + a4
 
	b1 = n \ 153
	n = n MOD 153
	b2 = n \ 61
	n = n MOD 61
	b3 = n \ 31
	n = n MOD 31
	B = 5 * b1 + 2 * b2 + b3
 
	m% = B + (3 AND B < 10) - (9 AND B >= 10)
	Y% = a + (1 AND B >= 10)
	D% = n + 1
END SUB

FUNCTION Jday& (Y%, m%, D%)    'julian day from y m d
	B = m% - 3 + (12 AND m% < 3)
	a = Y% - (1 AND m% < 3)
	L = a \ 4 - a \ 100 + a \ 400
	C = CLNG(30.6001 * B)
	Jday& = 1721120 + 365 * a + L + C + D% - 1
END FUNCTION

DEFINT A-Z
FUNCTION monthname$ (m%)
monthname$ = MID$("JanFebMarAprMayJunJulAugSepOctNovDec", (m% - 1) * 3 + 1, 3)
END FUNCTION

SUB print6x5 (n AS INTEGER, clr AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, dohex AS INTEGER) STATIC
DIM f(18, 4), m(1 TO 3), p(4), loaded, s$, i, j, a, x
IF loaded = 0 THEN
	RESTORE myfont
	FOR i = 0 TO 18
		FOR j = 0 TO 4
			READ x
			f(i, j) = x * 2048 OR x * 64 OR x * 2
		NEXT
	NEXT
	m(1) = &H7C00: m(2) = &H3C0: m(3) = &H1E
	loaded = 1
END IF

ERASE p
IF dohex THEN
	s$ = HEX$(n)
ELSE
	s$ = LTRIM$(STR$(n))
END IF

FOR i = 1 TO LEN(s$)' max 3
	a = ASC(MID$(s$, i, 1)) - 48' asc("0")
	IF a > 9 THEN a = a - 7
	FOR j = 0 TO 4
	 p(j) = p(j) OR (f(a, j) AND m(i))
	NEXT
NEXT
FOR j = 0 TO 4
	LINE (y1, x1 + j)-STEP(15, 0), clr, , p(j)
NEXT
END SUB

DEFLNG A-Z
SUB today (Y%, m%, D%)  'y m d from date$ string
	a$ = LTRIM$(RTRIM$(DATE$))
	m% = VAL(LEFT$(a$, 2))
	D% = VAL(MID$(a$, 4, 2))
	Y% = VAL(RIGHT$(a$, 4))
END SUB

