DECLARE SUB print6x5 (n AS INTEGER, clr AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, dohex AS INTEGER)
DECLARE SUB recoInicial (estrat%)
DECLARE SUB visualiza ()
DECLARE FUNCTION algoMiglio& (iter%)

' Versión mejorada del Problema del Viajante
DEFINT A-Z
OPTION BASE 1
RANDOMIZE TIMER
SCREEN 12
'------------------------------------
CONST visualizar = 1   '0 no se visualiza grafico haste el final

'---------------------------------------
 PRINT "PROBLEMA DEL VIAJANTE"
DIM SHARED nc, estratini
DO
INPUT "NUMERO DE CIUDADES=  < 240"; nc
LOOP UNTIL nc <= 240 AND nc > 2
DO
INPUT "Inicializacion (1=aleatoria 2=mas cercana)"; estratini
LOOP UNTIL estratini >= 1 AND estratini <= 2

DIM SHARED d(nc, nc)
DIM SHARED x(nc): DIM SHARED y(nc)
DIM SHARED rte(nc + 1)
'PRINT "GENERANDO COORDENADAS ALEATORIAS..."
DIM i, id, jd

'coords
FOR i = 1 TO nc
    x(i) = 10 + INT(630 * RND)
    y(i) = 20 + INT(450 * RND)
NEXT i

'situar y guardar pantalla con situacion ciudades
IF visualizar THEN
CLS
FOR i = 1 TO nc
    CIRCLE (x(i), y(i)), 2, 12
    PAINT (x(i), y(i)), 12
   print6x5 i, 12, y(i), x(i) + 2, 0
NEXT i
' $DYNAMIC
DIM SHARED scrn#(0 TO 19200)
' $STATIC
GET (0, 0)-(639, 479), scrn#(0)
END IF


' Calcular matriz de distancias
FOR jd = 1 TO nc: FOR id = jd + 1 TO nc
        d(id, jd) = SQR((CSNG(x(id)) - x(jd)) ^ 2 + (y(id) - y(jd)) ^ 2)
        d(jd, id) = d(id, jd)
NEXT id: NEXT jd

t! = TIMER
recoInicial (estratini)
'SLEEP
visualiza
iter = 0
LFINAL& = algoMiglio&(iter)
t! = TIMER - t!

' Mostrar resultados
PRINT "MEJOR RUTA:"
distmax = 0: dist = 0: org = 0: fin = 0: eti = 0
FOR i = 1 TO nc
    di = d(rte(i), rte(i + 1))
    IF di > distmax THEN distmax = di: org = rte(i): fin = rte(i + 1): eti = i
    PRINT USING "etapa ### DE ### A ### - DIST ### "; i; rte(i); rte(i + 1); di
NEXT i


PRINT
PRINT USING "LONGITUD MINIMA=####   MAYOR DIST ### en etapa ### entre ### y ###"; LFINAL&; distmax; eti; org; fin
PRINT USING "Num, ciudades ### Tiempo:  ###.# segundos. Iteraciones ####"; nc; t!; iter
PRINT "presione cualquier tecla para continuar"
SLEEP
' Dibujar gráficos
CLS
visualiza
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

FUNCTION algoMiglio& (iter%)
DIM test(nc + 1)
DIM LVIEJA&, LNUEVA&, i, j, k, MEJORADO

  FOR k = 1 TO nc
      LVIEJA& = LVIEJA& + d(rte(k), rte(k + 1))
  NEXT k

  ' Algoritmo 2-opt
  DO
      MEJORADO = 0:
      FOR i = 1 TO nc
          FOR j = i + 1 TO nc
              ' Crear una nueva ruta intercambiando segmentos
              FOR k = 1 TO nc + 1
                  test(k) = rte(k)
              NEXT k
              FOR k = i TO j
                  test(k) = rte(j - (k - i))
              NEXT k
              ' Calcular longitudes
              LNUEVA& = 0
              test(nc + 1) = test(1)
              FOR k = 1 TO nc
                  LNUEVA& = LNUEVA& + d(test(k), test(k + 1))
              NEXT k
             
              ' Actualizar si la nueva ruta es mejor
              IF LNUEVA& < LVIEJA& THEN
                  FOR k = 1 TO nc + 1
                      rte(k) = test(k)
                  NEXT k
                  LVIEJA& = LNUEVA&
                  IF visualizar THEN visualiza
                  MEJORADO = 1
              END IF
          NEXT j
      NEXT i
      iter% = iter% + 1
  LOOP WHILE MEJORADO
  algoMiglio& = LVIEJA&
END FUNCTION

SUB print6x5 (n AS INTEGER, clr AS INTEGER, x1 AS INTEGER, y1 AS INTEGER, dohex AS INTEGER) STATIC
DIM f(0 TO 18, 0 TO 4), m(1 TO 3), p(0 TO 4), loaded, s$, i, j, a, x
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

SUB recoInicial (estrat)
  DIM hecho(nc + 1), i, mejor, index, j, k, hecho
  SELECT CASE estrat
    CASE 1
      FOR i = 1 TO nc
        rte(i) = i
      NEXT
      rte(nc + 1) = rte(1)
    CASE 2
     
      rte(1) = 1: rte(nc + 1) = 1: hecho(nc + 1) = 1: hecho(1) = -1
      FOR i = 1 TO nc - 1
        k = rte(i): mejor = 32767
        FOR j = 1 TO nc
           IF j <> k AND NOT hecho(j) THEN
            IF d(k, j) < mejor THEN
              mejor = d(k, j)
               index = j
              END IF
             END IF
           NEXT
           rte(i + 1) = index
           hecho(index) = -1
          NEXT
          ERASE hecho
    CASE ELSE
      PRINT "estrategia "; estrat; " no implementada"
      END
    END SELECT

END SUB

SUB visualiza
  STATIC i
  WAIT 986, 8:
  IF visualizar THEN
    PUT (0, 0), scrn#(0), PSET
    'LOCATE 1, 1: PRINT iter%
  ELSE
    FOR i = 1 TO nc
        CIRCLE (x(i), y(i)), 2, 12
       ' PAINT (X(rte(i)), Y(rte(i))), 12
        print6x5 i, 12, y(i), x(i) + 2, 0
    NEXT i
     
  END IF
  PSET (x(rte(1)), y(rte(1))), 15
  FOR i = 2 TO nc + 1
      LINE -(x(rte(i)), y(rte(i))), 15
  NEXT i
  

END SUB

