DECLARE SUB getevents (yr%)
DECLARE SUB printmonth (m AS INTEGER, y AS INTEGER, r AS INTEGER, c AS INTEGER)
DECLARE SUB printyear (y AS INTEGER)
DECLARE FUNCTION DayofWeek% (y AS INTEGER, m AS INTEGER, d AS INTEGER)
DECLARE FUNCTION ISLEAPYEAR% (y AS INTEGER)
DECLARE FUNCTION daysinmonth% (y AS INTEGER, m AS INTEGER)
'To do:
'ok     why event type 2 flashes??
'       event editor?
'ok     month name vertical
'ok     se come dias a fin de mes
'       fases de la luna  omnicalculator.com/everydy-life/moon-phase
'---------------------------------------
CONST sun1dow = 0
CONST lang = "en"
'---------------------------------------
DEFINT A-Z
OPTION BASE 1
DIM SHARED monthname$(12)
DIM SHARED down$
DIM i AS INTEGER
DIM SHARED events&(0 TO 6, 1 TO 12)  'colors, months
DIM SHARED eventclr(1 TO 6, 0 TO 1) 'colors f/b
DIM SHARED pwrsof2&(31)

DEF FNCentrar$ (a$, b$) :
 MID$(a$, INT((LEN(a$) - LEN(b$) + 1) / 2), LEN(b$)) = b$
 FNCentrar$ = a$
END DEF

FOR i = 1 TO 31: pwrsof2&(i) = 2 ^ (i - 1): NEXT 'powers of 2 with an offset..

'read language strings from DATA
SELECT CASE lang
CASE "en":
	RESTORE langen
CASE "es":
	RESTORE langes
 CASE "fr"
	RESTORE langfr
CASE ELSE:
	PRINT "Constant Lang set to a non implemented language. Will use english"
	SLEEP
	RESTORE langen
END SELECT

FOR i = 1 TO 12: READ monthname$(i): NEXT
READ down$
IF sun1dow THEN down$ = RIGHT$(down$, 2) + " " + LEFT$(down$, LEN(down$) - 3)
'down$ = " " + down$
DIM y AS INTEGER

CLS
PRINT " --------------------------------------------------------------------------"
PRINT "|                       Perpetual Calendar for QB4.5 and QBasic             |"
PRINT "| Prints to screen the calendar of any year. Antoni Gual 2005-2025          |"
PRINT "| Uses Gregorian calendar(adopted by different countries at different times)|"
PRINT "| CONSTANT 'sun1dow' =1 starts week on sunday | 0=starts on monday          |"
PRINT "| CONSTANT 'lang'    changes language en,es,fr ATM. Add yours...            |"
PRINT "| Have event days relevant to you in the calendar  by editing the DATA      |"
PRINT " ---------------------------------------------------------------------------"
PRINT : PRINT
INPUT "year to print "; yr
IF yr = 0 THEN END

getevents yr
WIDTH , 43: CLS
printyear yr

SLEEP
WIDTH , 25

'---------------------------------------------------------------------
' language related DATA
langen:
DATA "January","February","March","April","May","June","July"
DATA "August","September","October","November","December"
DATA "Mo Tu We Th Fr Sa Su"
langes:
DATA "Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio"
DATA "Agosto","Septimbre","Octubre","Noviembre","Dicembre"
DATA "Lu Ma Mi Ju Vi Sa Do"
langfr:
DATA "Janvier","Fevrier","Mars","Avril","Mai","Juin","Julliet"
DATA "Aout","Septembre","Octobre","Novembre","Decembre"
DATA "Lu Ma Me Je Ve Sa Di"
'--------------------------------------------------------------------
'event defining DATA

eventcolors:
'here you set the fore and back color for a class f events
'ther must be 6 data lines, you can leave at DATA 0,0,""
'first declared color has priority
'0 = black       4 = red           8 = grey             12 = light red
'1 = blue        5 = magenta       9 = light blue       13 = light magenta
'2 = green       6 = brown        10 = light green      14 = light yellow
'3 = cyan        7 = white        11 = light cyan       15 = bright white
'For background, light/bright  means blinking!!
'format forecolor,backcolor,eventname$
		 
DATA  12,0,"official"
DATA  15,2,"meetings"
DATA  10,0,"rent"
DATA 0,0,""
DATA 0,0,""
DATA 0,0,""


eventdays:
'here you declare your events,the first item is the index the list of evclrs,1 based
'add/remove/comment out as many lines you want the last line must be DATA 0
'DATA color,year, month,day     :REM optional remark (recommended)

'fixed events
'use a 0 in the year field to have it every year
'use a 0 in the month field to have it repeated every month
DATA 1,0,12,25     : REM Xmas
DATA 1,0,12,26     : REM Boxing day (St.Esteve for me)
DATA 1,0,1,1       : REM New year
DATA 1,0,6,24      : REM St John, the summer solstice
DATA 3,0,0,22      : REM rent payment due

'events tied to a day of week
'day must be ---negative---:  dayofweek (-1:mon -7:sun)  -16*pos_in_month
'pos_in_month is(0=every week  1..4=nth dow of month  5= last dow of month)
' for example day-7 means last
DATA 1,0,0,-7      : REM sundays
DATA 2,0,0,-19     : REM doctor: first wednesday of month -(1*16+3) every month every year
DATA 1,0,5,-17     : REM may bank holiday  first monday of may -(1+16*1) may every year
DATA 1,0,5,-81     : REM spring bank holiday  last monday of may -(1+16*5) may every year
DATA 1,0,8,-81     : REM summer bank holiday  last monday of august -(1+16*5) august every year

'events for a year (moveable: depending on moon, leaders decisions or other )
'you need to enter them for each year they have to appear in....

DATA 1,2025,4,18    : REM good friday

DATA 0              : REM end of list marker. Don't erase or comment out !
'----------------------------------------------------------

'---------------------------------------------------------------------------------------------
FUNCTION DayofWeek (y AS INTEGER, m AS INTEGER, d AS INTEGER)
	'Zeller's congruence (it's in Wikipedia)
	'returns the day of week of a date 0=monday
	DIM p AS INTEGER, Q AS INTEGER

		'don't ask, Zeller does this manipulation before using the formula
		p = m + (12 AND (m < 3))   ' march to dec , jan=13,feb=14
		Q = y - (1 AND (m < 3))    'substract 1 to year if month is jan or feb

		'I had to tweak the ziegler's formula so it gives the correct values
		 'Zeller uses a 2.6 coeff for p, i had to decrease it to 2.57
		 'anr i had to add  a 'cheat' term so february gives a correct value.
		 DayofWeek = (2 + d + INT(2.57 * p) + (1 AND (p = 14)) + Q + (Q \ 4) - (Q \ 100) + (Q \ 400)) MOD 7
		'0=sunday, 1=monday....6=saturday
END FUNCTION

FUNCTION daysinmonth (y AS INTEGER, m AS INTEGER)
SELECT CASE m
CASE 2: daysinmonth = 28 + (1 AND ISLEAPYEAR(y))
CASE 4, 6, 9, 11: daysinmonth = 30
CASE ELSE: daysinmonth = 31
END SELECT
END FUNCTION

SUB getevents (yr)
'events are expanded and kept in an array of (colors, months)
'elements of array are long, so 31 bits are used to indicate an event day
'color 0 sums events for all colors so printmonth can skip non-event day  easily

'gets event days for presnt year
RESTORE eventcolors
FOR i = 1 TO 6
		READ eventclr(i, 0), eventclr(i, 1), dummy$
NEXT
cnt = 1

ERASE events&
RESTORE eventdays
DO
	READ c
	IF c = 0 THEN EXIT DO
	READ y, mt, d
	PRINT cnt, c, y, mt, d
	IF y = yr OR y = 0 THEN    'this year or alll years
		IF mt = 0 THEN            'all months
			 FOR m = 1 TO 12
					GOSUB doday
			 NEXT
		ELSE
			 m = mt: GOSUB doday
		END IF
	END IF
	cnt = cnt + 1
LOOP


FOR i = 1 TO 12
	 FOR j = 0 TO 6
	 events&(0, i) = events&(0, i) OR events&(j, i)
	 NEXT
NEXT
EXIT SUB

doday:
IF d > 0 THEN    'single day
	 events&(c, m) = events&(c, m) OR pwrsof2&(d):
	 RETURN
ELSE              'd<0  nth day of week
	 d1 = -d
	 dow = (d1 AND 7) - 1
	 rep = d1 / 16
	 IF rep = 5 THEN   'last day of week
			 ldim = daysinmonth(yr, m)
			 dowldim = DayofWeek(yr, m, ldim)
			 dd = ldim - ((dowldim - dow - 1 + 7) MOD 7)
			 events&(c, m) = events&(c, m) OR pwrsof2&(dd)
	 ELSEIF rep > 0 THEN
			dowfdim = DayofWeek(yr, m, 1)
			dd = 1 + ((dow - dowfdim + 1 + 7) MOD 7)
			events&(c, m) = events&(c, m) OR pwrsof2&(dd)

	 ELSEIF rep = 0 THEN      'same dow all week
			dowfdim = DayofWeek(yr, m, 1)
			ldim = daysinmonth(yr, m)
			dd = 1 + ((dow - dowfdim + 1 + 7) MOD 7)
			DO WHILE dd <= ldim
				events&(c, m) = events&(c, m) OR pwrsof2&(dd)
				dd = dd + 7
			LOOP
	END IF
END IF
RETURN
END SUB

FUNCTION ISLEAPYEAR (y AS INTEGER)
'Returns -1 if Gregorian year y is a leap year
ISLEAPYEAR = ((y AND 3) = 0) AND (NOT ((y MOD 100) = 0) OR ((y MOD 400) = 0))
END FUNCTION

SUB printmonth (m AS INTEGER, y AS INTEGER, r AS INTEGER, c AS INTEGER) STATIC
DIM i AS INTEGER, j AS INTEGER

'print header: month name and days of week

DIM namebuf AS STRING * 7: namebuf = monthname$(m)
LOCATE r, c + 1
rw = 1
GOSUB printmonth
COLOR 15, 3


PRINT " " + down$;
LOCATE r + 1, c
DIM sd AS INTEGER, ld AS INTEGER

'print the  month days
sd = DayofWeek(y, m, 1)                          'returns 0 for sunday
IF (sun1dow = 0) THEN sd = ((sd + 6) MOD 7)       'monday day0
ld = daysinmonth(y, m)                           'last day of month
COLOR 15, 0
j = 0:           'day of month  (0 before first day of month)
DO             'do rows
	GOSUB printmonth
	FOR i = 0 TO 6    'days of week
		 IF (i = sd) OR (j > 0) THEN j = j + 1
		 IF j > ld THEN EXIT DO
		 IF j < 1 THEN
				PRINT "   ";
		 ELSE
			 IF (events&(0, m) AND pwrsof2&(j)) THEN         'event for the day?
					FOR k = 1 TO 6                              'which class?
						 IF events&(k, m) AND pwrsof2&(j) THEN
								 COLOR eventclr(k, 0), eventclr(k, 1) 'color the day
								 EXIT FOR
						 END IF
					 NEXT
			 ELSE                                           'no event
					COLOR 15, 0                                 'neutral color
			 END IF
			 PRINT RIGHT$("   " + STR$(j), 3);              'print the day
		 END IF
	NEXT
	PRINT                                                     'next roe
LOOP
EXIT SUB

printmonth:  'called at every line, prints month name vertically
LOCATE , c - 2
COLOR 15, 2
PRINT MID$(namebuf, rw, 1) + SPACE$(1);
COLOR 15, 0
LOCATE , c
rw = rw + 1
RETURN


END SUB

'
'---------------------------------------------------------------------------
SUB printyear (y AS INTEGER)
DIM m AS INTEGER, i AS INTEGER, j AS INTEGER
LOCATE 2, 1, 0
DIM centryear AS STRING * 80
COLOR 15, 3
PRINT FNCentrar$(centryear, "Calendar of " + STR$(y))
m = 1
FOR i = 0 TO 3
FOR j = 0 TO 2
	printmonth m, y, i * 8 + 5, j * 25 + 5
	m = m + 1
NEXT
NEXT
END SUB

