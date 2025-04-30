SCREEN 13
CONST pi = 3.141592
'emulating "RGB" palette

FOR i = 1 TO 255
	SELECT CASE i
	CASE IS < 43: b = 0: r = 1: g = i / 42
	CASE IS < 86: b = 0: r = -(i - 43) / 42 + 1: g = 1
	CASE IS < 129: b = (i - 86) / 42: r = 0: g = 1
	CASE IS < 172: b = 1: r = 0: g = -(i - 129) / 42 + 1
	CASE IS < 215: b = 1: r = (i - 172) / 42: g = 0
	CASE ELSE: b = -(i - 215) / 42 + 1: r = 1: g = 0
	END SELECT
	pp = INT(63 * r) + 256 * INT(63 * g) + &H10000 * INT(63 * b)
	PALETTE i, pp
NEXT
'check palette
FOR i = 1 TO 255
	LINE (i + 32, 0)-STEP(0, 2000), i, , &HAAAA
NEXT
'the demo
WINDOW (-3, -1.8)-(3, 1.8)
FOR t = 0 TO 4 * pi + .1 STEP .0002
	LINE (COS(t), SIN(t))-((COS(t) - SIN(2 * t)), (SIN(t) + COS(t / 2))), INT(t * 60 / pi)
NEXT
SLEEP

