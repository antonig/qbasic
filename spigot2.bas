'Calculation of 1000 digits of PI using Spigot algorithm by Antoni Gual
'translated from  Rabinowitz FORTRAN optimized algorithm
'found at https://rosettacode.org/wiki/Pi#Fortran

DEFLNG A-Z
CONST bas = 100000
t! = TIMER
DIM vect(3350)  'spigot array
DIM buff(201)   'save blocks of up to 5 digits for later printing
CLS
carry = 0
FOR i = 1 TO 3350: vect(i) = 2: NEXT   'init array to all 2

FOR n = 1 TO 201  'for each block
  karray = 0
  FOR l = 3350 TO 1 STEP -1
    num = bas * vect(l) + karray * l
    karray = num \ (2 * l - 1)
    vect(l) = num MOD (2 * l - 1)
  NEXT
  k = karray \ bas
 
  buff(n) = carry + k
  carry = karray MOD bas
NEXT

'calculation ended. Print results
'integer part and dot
PRINT USING "#."; buff(1);
'print blocks of 5 digits filled by 0's at left
FOR i = 2 TO 201: PRINT RIGHT$("00000" + LTRIM$(STR$(buff(i))), 5); : NEXT
PRINT
PRINT TIMER - t!; " seconds"
SLEEP

