DECLARE FUNCTION filsel$ (ext$)
DECLARE SUB TimeGet (t AS ANY)
DECLARE SUB timeout2 (hdl%, delay&)
DECLARE SUB readfile (f$, dly&)
DECLARE SUB ansiout (aval%)
DECLARE SUB CONOUT (func%, par%)
DECLARE FUNCTION timerFree% (handle%)
'add,substract and compare two times
DECLARE FUNCTION TimeCalc% (res AS ANY, t1 AS ANY, t2 AS ANY, sb%)
CONST TimeAdd = 0
CONST TimeSub = 1
CONST TimeComp = 2

DEFINT A-Z

CONST NrofTimers = 10 'change depending on your program needs

'this variable type keeps time readings (In PowerBasic we could use a QuadWord)
TYPE timetype
   tick AS LONG       'clock ticks (54.9254 msec) elapsed from midnight
   pit  AS LONG       'PIT counts  (0,8381 æsec)  from last tick
END TYPE

TYPE TimerType
    tstart AS timetype
    tend   AS timetype
    flag AS INTEGER
END TYPE

'TimerType flag constants
CONST flInUse = 1
CONST flPastMidnight = 2
CONST flElapsed = 4



DIM TimArray(1 TO NrofTimers) AS TimerType

'some constants you can't touch!
CONST clockfreq# = 1193181.666#           'PIT clock frequency
CONST pitdivisor# = 65536#                'PIT divisor
CONST tick4sec# = clockfreq# / pitdivisor 'Hz

CONST scrcls = 1, scrhom = 2, scrchr = 3, scrrow = 4, scrcol = 5
CONST scrup = 6, scrdn = 7, scrlf = 8, scrrg = 9
CONST scrfgr = 10, scrbgr = 11, scrsvc = 12, scrrtc = 13, scrcll = 14



    DO
        WIDTH 80, 25: CLS : f$ = filsel$("asc ans")
        
        WIDTH 80, 25: COLOR 7, 0: CLS
        IF f$ = "" THEN PRINT "No file selected. Exiting": SLEEP: END
        t! = TIMER
        dly& = 3
        readfile f$, dly&
        IF timed THEN LOCATE 1, 1: PRINT TIMER - t!;
        a$ = INPUT$(1)
    LOOP UNTIL a$ = CHR$(27)

CLS : PRINT "Thanks for using QBANSI. Please e-mail bugs & suggestions to agual@eic.ictnet.es"
END
nofile: errata% = ERR: RESUME NEXT
DEFSNG A-Z

DEFINT A-Z
SUB ansiout (aval) STATIC
'ANSI decoder. Receives char numbers from a file reader (could be a COM port)
'Sends screen commands to CONOUT
'---------------------------------------------------------------------------
'  parm = -2 => not in ansi sequence
'  parm =  0 => esc received, waitng for ]
'  parm >  0 => waiting for parameter parm
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  5/5/2001
'---------------------------------------------------------------------------
  IF aval = -1 THEN
    DIM parms(16)
    fc = 7: bc = 0: cfx = 0: blink = 0
    lx = 1: ly = 1
    CONOUT scrfgr, 7
    CONOUT scrbgr, 0
    CONOUT scrcls, 0
    parm = -2
    EXIT SUB
  END IF
 
   
    SELECT CASE parm
    'si no sec en proceso, ESC inicia una, otros chr van a pantalla!
    CASE -2
       IF aval = 27 THEN
            parm = 0
       ELSE
           IF NOT invis THEN CONOUT scrchr, aval
       END IF
    'si sec es ESC debe seguir un [, si no error
    CASE 0
      parms(1) = -1
      IF aval = 91 THEN parm = 1 ELSE anserr = 1: parm = -2
      
   
    'secuencia inicializada
    CASE ELSE
      SELECT CASE aval
      CASE 48 TO 57                                'numeric parameter
        IF parms(parm) = -1 THEN parms(parm) = 0
        parms(parm) = parms(parm) * 10 + aval - 48
      CASE 44, 59                                  'parameter separator
        parm = parm + 1: parms(parm) = -1
      CASE 61, 63                                  'parameter prefix
         'parm = -2
     
      ' --------------------cursors-----------------------------------
      CASE 65 'A                                      'up
        parm = -2: IF parms(1) = -1 THEN p = 1 ELSE p = parms(1)
        CONOUT scrup, p
      CASE 66 'B                                      'down
        parm = -2: IF parms(1) = -1 THEN p = 1 ELSE p = parms(1)
        CONOUT scrdn, p
      CASE 67 'C                                      'right
        parm = -2: IF parms(1) = -1 THEN p = 1 ELSE p = parms(1)
        CONOUT scrrg, p
      CASE 68 'D                                      'left
        parm = -2: IF parms(1) = -1 THEN p = 1 ELSE p = parms(1)
        CONOUT scrlf, p
      CASE 102, 72  'f,H                              'cursorto
        SELECT CASE parm
        CASE 1
          IF parms(1) = -1 THEN
            CONOUT scrrow, 1
            CONOUT scrcol, 1
          ELSE
            CONOUT scrrow, parms(1)
          END IF
        CASE ELSE
            CONOUT scrrow, parms(1)
            CONOUT scrcol, parms(2)
        END SELECT
        parm = -2
      CASE 115 's                                    'save cursor
        CONOUT scrsvc, 0: parm = -2
      CASE 117 'u                                      'retrieve cursor
        CONOUT scrrtc, 0: parm = -2
                             
      '--------------------------clear-------------------------------

      CASE 74 'J                                      'cls
        IF parms(1) = 2 THEN CONOUT scrcls, 0
        parm = -2
      CASE 75 'K                                      'clear to eol
        CONOUT scrcll, 0: parm = -2
     
      '--------------------------color-------------------------------
      CASE 109' "m"
        FOR j = 1 TO parm
          SELECT CASE parms(j)
          CASE -1
            EXIT FOR
          CASE 0    'RESET
            fc = 7: bc = 0: cfx = 0: blink = 0: invis = 0
            aclr = fc: GOSUB tocolor: CONOUT scrfgr, clr + cfx OR blink
            aclr = bc: GOSUB tocolor: CONOUT scrbgr, clr AND 7
          CASE 1    'BOLD
            cfx = 8
            aclr = fc: GOSUB tocolor: CONOUT scrfgr, clr + cfx OR blink
          CASE 4    'UNDERLINE
          CASE 5    'BLINK
            blink = 16
            aclr = fc: GOSUB tocolor: CONOUT scrfgr, clr + cfx OR blink
          CASE 7    'REVERSE
            SWAP fc, bc
            aclr = fc: GOSUB tocolor: CONOUT scrfgr, clr + cfx OR blink
            aclr = bc: GOSUB tocolor: CONOUT scrbgr, clr AND 7
          CASE 8    ' INVISIBLE
            invis = -1
          CASE 30 TO 37
            fc = parms(j) - 30
            aclr = fc: GOSUB tocolor: CONOUT scrfgr, clr + cfx OR blink
          CASE 40 TO 47
            bc = parms(j) - 40
            aclr = bc: GOSUB tocolor: CONOUT scrbgr, clr AND 7
          END SELECT
        NEXT j
        parm = -2
      CASE ELSE
        anserr = 1: parm = -2: parms(1) = -1
      END SELECT
    END SELECT
    
EXIT SUB
tocolor:
    SELECT CASE aclr
    CASE 0: clr = 0
    CASE 1: clr = 4
    CASE 2: clr = 2
    CASE 3: clr = 6
    CASE 4: clr = 1
    CASE 5: clr = 5
    CASE 6: clr = 3
    CASE 7: clr = 7
    END SELECT
RETURN

RETURN
END SUB

SUB CONOUT (func, par)
'Color console output
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  5/5/2001
'---------------------------------------------------------------------------

CONST init = &HB800, ncc = 160
CONST eos = 160 * 25
CONST fgr = &HF, bgr = &HF0
STATIC savpos, atr, posi

SELECT CASE func
    CASE scrchr
        IF par = 13 THEN posi = (posi \ ncc) * ncc: EXIT SUB
        IF par = 10 THEN posi = (posi \ ncc + 1) * ncc + (posi MOD ncc): EXIT SUB
        'deal with scrolling images bigger than 80x25
        IF posi >= eos THEN
            temp = posi - eos:
                scrll = temp \ ncc + 1
                LOCATE 25, 1: FOR i = 1 TO scrll: PRINT : NEXT
                DEF SEG = &HB800
                'patch to scroll up line 25!!!
                j = ncc * 24
                FOR i = ncc * 23 TO ncc * 24 - 1
                    POKE i, PEEK(j): POKE j, 0: j = j + 1
                NEXT
                posi = eos - ncc + (posi MOD ncc)
        END IF
        DEF SEG = &HB800
        POKE posi, par: posi = posi + 1
        POKE posi, atr: posi = posi + 1
    CASE scrfgr
        atr = atr AND bgr OR par
    CASE scrbgr
        atr = atr AND fgr OR (par * 16)
    CASE scrup
        posi = posi - (ncc * par)
    CASE scrdn
        posi = posi + (ncc * par)
    CASE scrlf
       posi = posi - (2 * par)
    CASE scrrg
        posi = posi + (2 * par)
    CASE scrcol
        posi = (posi \ ncc) * ncc + ((par - 1) * 2)
    CASE scrrow
        posi = (posi MOD ncc) + ((par - 1) * ncc)
    CASE scrcls
        CLS : posi = 0: savpos = 0: atr = 7
    CASE scrhom
        posi = 0
    CASE scrsvc
        savpos = posi
    CASE scrrtc
        posi = savpos
    CASE scrcll
        temp = CSRLIN
        PRINT SPACE$(80 - temp)
        LOCATE , temp
    END SELECT

'
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
SUB readfile (f$, dly&)
'buffered file reader. Sends single chars to ansiout at a steady pace
    f = FREEFILE
    OPEN f$ FOR BINARY AS #f
    IF LOF(f) = 0 THEN
        CLOSE f: KILL f$: CLS : PRINT "File "; f$; " does not exist": END
    END IF
    hdl = 0
    ansiout -1  'reset viewer
    WIDTH 80, 25: CLS
    blen& = 4096: buf$ = SPACE$(blen&): flen& = LOF(f)
    WHILE flen&
      IF blen& > flen& THEN blen& = flen&: buf$ = SPACE$(flen&)
      GET #f, , buf$: flen& = flen& - blen&:
      FOR i& = SADD(buf$) TO SADD(buf$) - 1& + LEN(buf$)
        DEF SEG = VARSEG(buf$)
        ansiout PEEK(i&)
        CALL timeout2(hdl, dly&)
      NEXT
    WEND
    timeout2 hdl, -1
    CLOSE f
END SUB

DEFSNG A-Z
FUNCTION TimeCalc% (res AS timetype, t1 AS timetype, t2 AS timetype, sb%)
'adds or substracts two times in timetype format
'---------------------------------------------------------------------------
'INPUT:        a,b = times in TimeType. For subst, a must be later than b
'              sb%   operation to perform   0 add | 1 substract | 2 comparation
'OUTPUT:       res=  for add      result MOD 1 day
'                    for subst:   if a>b res= a-b |if b>a res=(a+1day)-b
'                    for compare: nothing
'              Timecalc%
'                 add          1 if if res>1day
'                 substraction 1 if b>a
'                 compare      1 if b>a
'DEPENDENCIES: None
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  13/1/2001
'---------------------------------------------------------------------------

'if sb%=0 res =a + b    if res>1day  Timecalc%=1, res =res MOD 1 day
'if sb%=1 res =a - b    if b>a       Timecalc%>1, res= (a+1day)-b
'if sb%=2               if b>a       Timecalc =1
CONST ticks4day& = tick4sec# * 3600 * 24  'ticks
DIM t3 AS timetype, t4 AS timetype
SELECT CASE sb%
CASE TimeComp  '2
    IF t2.tick > t1.tick THEN
        TimeCalc% = 1: EXIT FUNCTION
    ELSEIF t2.tick = t1.tick THEN
        IF t2.pit >= t1.pit THEN TimeCalc% = 1: EXIT FUNCTION
    END IF
    TimeCalc% = 0

CASE TimeAdd   '0
    a& = CLNG(t1.pit) + t2.pit
    carry% = c& \ 65536
    IF carry THEN a& = a& MOD 65536
    res.pit = a&
    res.tick = t1.tick + t2.tick + carry
    IF res.tick \ ticks4day& THEN TimeCalc% = 1: res.tick = res.tick \ ticks4day&
CASE TimeSub    '1
    sb% = 0
    LSET t3 = t1: LSET t4 = t2
    IF t4.tick > t3.tick THEN
        t3.tick = t3.tick + ticks4day&
        sb% = 1
    ELSEIF t4.tick = t3.tick THEN
        IF t4.pit > t3.pit THEN TimeCalc% = 1: t3.tick = t3.tick + ticks4day&
    END IF
    res.pit = t3.pit - t4.pit
    IF res.pit < 0 THEN pastmidnight% = 1: res.pit = 65536 - res.pit
    res.tick = t3.tick - t4.tick
END SELECT

END FUNCTION

SUB TimeGet (t AS timetype) STATIC
'Reads hardware time. Should be a function, but QB can't return a UDT
'---------------------------------------------------------------------------
'INPUT:        nothing
'OUTPUT:       t= current time in TimeType format
'DEPENDENCIES: None
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  13/1/2001
'---------------------------------------------------------------------------
    STATIC tinit%
  
    'reinit timer so we can assume we read lo byte first and then hi byte
    IF tinit% = 0 THEN
        OUT &H43, &H34
        OUT &H40, 0
        OUT &H40, 0

        tinit% = 1
    END IF
  
    'read bios tick counter
    GOSUB TimeGetRdTick1
    SWAP b2&, b&
  
    'get PIT counter for channel 0 and negate it as it counts bacwards)
    OUT &H43, &H0: t0% = INP(&H40): t1% = INP(&H40)
    a& = 65536 - (256& * t1% + t0%)

    'get bios tick counter
    GOSUB TimeGetRdTick1
  
    'if lobite of bios tick changed, decide which value to display
    IF (b2& <> b&) THEN IF (a& AND &H8000&) THEN SWAP b2&, b&

    'fill structure
    t.tick = b&
    t.pit = a&
EXIT SUB

'read BIOS tick counter

TimeGetRdTick1:
    DEF SEG = &H40
    t5% = PEEK(&H6F): t4% = PEEK(&H6E): t3% = PEEK(&H6D): t2% = PEEK(&H6C)
    b& = 256& * (t3% + 256& * (t4% + 256& * t5%)) + t2%
    RETURN

END SUB

DEFINT A-Z
SUB timeout2 (hdl, delay&)
'Easy timeout: It handles (depending on te PC speed) tenths of microsecond)
'---------------------------------------------------------------------------
'if hdl=0   searches for a timer,and resets it with delay.Returns hdl=timer nr
'If hdl=0   and delay=0 does not search for a timer and returns hdl=-128
'if hdl=-128 returns inmediately (timer not in use)
'if hdl>0   waits until the delay from last timer reset has elapsed
'if hdl<0   resets timer with new delay,does'nt wait
'Dependencies: TimerFree,TimeCalc,TimeGet
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  5/5/2001
'---------------------------------------------------------------------------


SHARED TimArray() AS TimerType
CONST pit4mili# = clockfreq# / 10000#  'pit pulses in a tenth of milisecond

DIM tread AS timetype, tmili AS timetype, tdummy AS timetype

    IF hdl = -128 THEN EXIT SUB
    IF delay& = 0 THEN hdl = -128: EXIT SUB
    IF hdl = 0 THEN                        'new timer)
        IF delay& <= 0 THEN hdl = -128: EXIT SUB
        hdl = timerFree(0)
        IF hdl = 0 THEN EXIT SUB
        handle = hdl
        TimeGet tread
        GOSUB timeoutrestart
     ELSE
        handle = hdl AND &H7F
        IF delay& = -1 THEN dummy = timerFree(handle): EXIT SUB
        'GOSUB timeoutrestart
        IF (hdl AND &H80) = 0 THEN
            DO
                TimeGet tread
                IF TimeCalc(tdummy, TimArray(handle).tend, tread, TimeComp) THEN
                    EXIT DO
                ELSE
                    IF TimArray(handle).flag AND flPastMidnight THEN
                        IF TimeCalc(tdummy, tread, TimArray(handle).tstart, TimeComp) THEN
                            EXIT DO
                        END IF
                    END IF
                END IF
            LOOP
        END IF
        GOSUB timeoutrestart
    END IF
   
EXIT SUB
timeoutrestart:
    LSET TimArray(handle).tstart = tread
    a& = delay& * pit4mili
    tmili.pit = a& MOD 65536
    tmili.tick = a& \ 65536
    pastmnt% = TimeCalc(TimArray(handle).tend, TimArray(handle).tstart, tmili, TimeAdd) 'add times
    IF pastmnt% THEN TimArray(handle).flag = TimArray(handle).flag OR flPastMidnight
RETURN
END SUB

DEFSNG A-Z
FUNCTION timerFree% (handle%)
'Called by user to free a timer. Called by system to find a free timer
'---------------------------------------------------------------------------
'3 types of calls
'INPUT:        handle% >0 =>Release timer # handle. Called by user
'OUTPUT:       TimerFree%=0 if error
'
'INPUT:        handle =0  =>Allocate a new timer.Called by TimeOut and TimElapsed
'OUTPUT:       TimerFree%=  0 If error
'              TimerFree%=  newhandle if success
'
'INPUT:        handle% =-1 =>Release them all
'
'DEPENDENCIES: None
'---------------------------------------------------------------------------
'By Antoni Gual agual@eic.ictnet.es  13/1/2001
'---------------------------------------------------------------------------
SHARED TimArray() AS TimerType
SELECT CASE handle%
CASE 0
    i% = 0
    DO
       i% = i% + 1
  
    LOOP WHILE (i% <= NrofTimers) AND (TimArray(i%).flag AND flInUse)
    IF i% > NrofTimers THEN
        timerFree% = 0
    ELSE
        TimArray(i%).flag = TimArray(i%).flag OR flInUse
        timerFree% = i%
    END IF
CASE -1
    ERASE TimArray
CASE ELSE
    IF handle% < 0 OR handle% > NrofTimers THEN timerFree% = 0: EXIT FUNCTION
    TimArray(handle%).flag = 0
    timerFree% = 1
END SELECT

END FUNCTION

