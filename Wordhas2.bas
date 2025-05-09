'===========================================================================
' Subject: Word Hashing                       Date: 03-16-03 (  :  )     
'  Author: Antoni Gual et.al.                 Code: QB, PDS              
'  Origin: agual@eic.ictnet.es              Packet: TEXT.ABC
'===========================================================================
DECLARE SUB fastsort ()
DECLARE FUNCTION getaword2$ (a$, ENDED%)
DECLARE FUNCTION updatefreq% (a$)
DECLARE FUNCTION funFirstPrime% (threshold%)
'
'----------------------------------------------------------------------------
'-----------------------------------------------------------------------------

DEFINT A-Z
CONST TRUE = -1, FALSE = 0
CONST empty$ = "          "

TYPE stacktype
 low AS INTEGER
 hi AS INTEGER
END TYPE


DIM SHARED aa$
DIM SHARED tablesize
DIM SHARED new.words
DIM n AS LONG


Main:
 CLS
 LOCATE 1, 1
 PRINT "WORDHASH.BAS By Rich Geldreich      1992"
 PRINT "     Tweaked by Quinn Tyler Jackson 1993"
 PRINT "         and by Antoni Gual         2003"


 filename$ = RTRIM$(COMMAND$)
 IF LEN(filename$) = 0 THEN
	PRINT
	PRINT "USE: WORDHASH  textfilename"
	PRINT "Results outputted to result.txt"
	END
 END IF




 OPEN filename$ FOR INPUT AS #1 LEN = 16384


'Dont set directly the table size, for a fast hash it must be 30% bigger
' than nr of entries and be a prime number. That's what the function finds.
tablesize = funFirstPrime(10001)


REDIM SHARED wordtable(tablesize) AS STRING * 10
REDIM SHARED counts(tablesize) AS LONG
T! = TIMER

 PRINT
 PRINT "Processing "; filename$; " with a "; tablesize; " elements table"
 aa$ = SPACE$(10)
 bb$ = aa$
 FOR I = 0 TO tablesize: LSET wordtable(I) = SPACE$(10): NEXT
 DO UNTIL EOF(1)
	 LINE INPUT #1, a$
		a$ = LTRIM$(RTRIM$(a$))
		IF LEN(a$) THEN
		'PRINT A$
		a$ = UCASE$(a$)
		DO
		 b$ = getaword2$(a$, ENDED)
		 IF LEN(b$) THEN
			'PRINT B$; "<"
			LSET bb$ = b$
			wRD& = wRD& + 1
			IF updatefreq(bb$) THEN GOTO exitit
		 END IF
		LOOP UNTIL ENDED
	 END IF
	 n = n + 1
	 LOCATE 7, 1: PRINT USING "######## words ###### lines : ###### new words"; wRD&; n; new.words;
 LOOP
exitit:
	 PRINT " in "; TIMER - T!; " sec."
	 CLOSE
	 PRINT "Sorting results.."
	 fastsort
	 GOSUB printtable
	 ERASE wordtable, counts
	 PRINT
	 PRINT "Done. You can check the results in  RESULT.TXT"
	 a$ = INPUT$(1)
END

printtable:
	 OPEN "RESULT.TXT" FOR OUTPUT AS #1
	 PRINT #1, "Wordcount of the file:"; COMMAND$
	 PRINT #1, USING "######## words ###### lines : ###### new words";
  Print #1
	 j = 0
	 FOR I = 0 TO tablesize
	 IF ASC(wordtable(I)) <> 32 THEN
	 PRINT #1, USING "###### , \        \  "; counts(I); wordtable(I)
	
	 'use this for CSV output to view results with MS Excel
	 'WRITE #1, counts(I), wordtable(I)
	 j = j + 1
	 END IF
	 NEXT
   print #1;"----End---"
	 CLOSE
RETURN

SUB fastsort
	'QuickSort iterative (rather than recursive) by Cornel Huth
	DIM Lstack(1 TO 128) AS stacktype   'our stack
	DIM Sp AS INTEGER                   'out stack pointer
	Sp = 1
	Lstack(Sp).low = 0
	Lstack(Sp).hi = tablesize
	Sp = Sp + 1
	DO
		Sp = Sp - 1
		low = Lstack(Sp).low
		hi = Lstack(Sp).hi
		DO
			I = low
			j = hi
			mid = (low + hi) \ 2
			GOSUB sortcounts
			IF j - low < hi - I THEN
				IF I < hi THEN
					Lstack(Sp).low = I
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
				low = I
			END IF
		LOOP WHILE low < hi
	LOOP WHILE Sp <> 1
EXIT SUB

sortcounts:

compare& = counts(mid)
DO
	WHILE counts(I) > compare&: I = I + 1: WEND
	WHILE counts(j) < compare&: j = j - 1: WEND
	IF I <= j THEN
		SWAP wordtable(I), wordtable(j)
		SWAP counts(I), counts(j)
		I = I + 1
		j = j - 1
	END IF
LOOP WHILE I <= j
RETURN

sortwords:
compare$ = wordtable(mid)
DO
	WHILE wordtable(I) > compare$: I = I + 1: WEND
	WHILE wordtable(j) < compare$: j = j - 1: WEND
	IF I <= j THEN
		SWAP wordtable(I), wordtable(j)
		SWAP counts(I), counts(j)
		I = I + 1
		j = j - 1
	END IF
LOOP WHILE I <= j
RETURN





END SUB

' This FUNCTION returns a prime number that is at least 30% greater than
' threshold.  It will TRY to return a prime number that also fits into the
' form 4K+3, where k is any integer, but if the prime number is twice the
' size of the threshold, it will ignore this criterion.
'
'       Written by Charles Graham, Tweaked by Quinn Tyler Jackson
'
FUNCTION funFirstPrime (threshold)
CONST TRUE = -1
CONST FALSE = NOT TRUE

tp30 = INT((threshold * 1.3))
IF tp30 / 2 = tp30 \ 2 THEN
	tp30 = tp30 + 1
END IF
c = tp30 - 2
IF c < 1 THEN
	c = 1
END IF
t2 = threshold * 2
DO
	c = c + 2
	FOR z = 3 TO SQR(c)
		ind = TRUE
		IF c / z = c \ z THEN
			ind = FALSE
			EXIT FOR
		END IF
	NEXT z
	IF ind THEN
		IF (c - 3) / 4 = INT((c - 3) / 4) OR c > t2 THEN
			funFirstPrime = c
			EXIT DO
		END IF
	END IF
LOOP
END FUNCTION

FUNCTION getaword2$ (a$, ENDED)
 'Uses only a single string assign at the end so it's fast!
 'Needs the line passed to be uppercase!
 'Very buggy! Should be reworked!
 'Takes all chars>128 as valid in a word!
 '

 STATIC ptr
 IF LEN(a$) = 0 THEN ENDED = -1: EXIT FUNCTION
 ENDED = 0
 DEF SEG
 wptr& = CLNG(SADD(a$)) + ptr - 1
 DO
	 ptr = ptr + 1
	 wptr& = wptr& + 1
	 c = PEEK(wptr&)
 LOOP WHILE (c < 65 OR (c > 90 AND c < 129)) AND (ptr < LEN(a$))
 IF ptr = LEN(a$) THEN GETWORD2$ = "": ENDED = -1: ptr = 0: EXIT FUNCTION
 ptr1 = ptr
 DO
	 ptr = ptr + 1
	 wptr& = wptr& + 1
	 c = PEEK(wptr&)
 LOOP UNTIL c < 65 OR (c > 90 AND c < 129) OR ptr > LEN(a$)
 IF ptr > LEN(a$) THEN
	getaword2$ = MID$(a$, ptr1, ptr - ptr1)
	ENDED = -1: ptr = 0
 ELSE
	getaword2$ = MID$(a$, ptr1, ptr - ptr1)
 END IF
END FUNCTION

FUNCTION updatefreq (a$)
STATIC collisions AS LONG

	n = 0
	FOR I = 1 TO 10 STEP 2
	n = n XOR CVI(MID$(a$, I, 2))
	NEXT
	keyindex = n AND &H7FFF
	
	'adjust the keyindex so its within the table
	keyindex = keyindex MOD tablesize
	'calculate an offset for retries
	IF keyindex = 0 THEN
		Offset = 1
	ELSE
		Offset = tablesize - keyindex
	END IF
	'main loop of hashing
	DO
		'is this entry empty?
		IF wordtable(keyindex) = empty$ THEN
			'add this entry to the hash table
			LSET wordtable(keyindex) = a$
			counts(keyindex) = 1
			new.words = new.words + 1
			IF new.words = tablesize THEN
				updatefreq = 1
				EXIT FUNCTION
			END IF
			EXIT FUNCTION
		'is this what we're looking for?
		ELSEIF wordtable(keyindex) = a$ THEN
			'increment the frequency of the entry
			counts(keyindex) = counts(keyindex) + 1
			EXIT FUNCTION
		'this entry contains a string other than what we're looking for:
		'adjust the KeyIndex and try again
		ELSE
			collisions = collisions + 1
			keyindex = keyindex - Offset
			'wrap back the keyindex if it's <0
			IF keyindex < 0 THEN
				keyindex = keyindex + tablesize
			END IF
		END IF
	LOOP

END FUNCTION

