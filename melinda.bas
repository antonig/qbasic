'-----------------------------------------------------------------------------
'BuddahBrot for QB4.5 screen 13
'The Mandelbrot image is drawn by plotting the points in the complex plane where
'the Mandelbrot formula leads to a convergent series.
'
'Themethod invented bt Melinda Green in 1993) is drawn by cumulating all intermediate
'points of the diverting Mandelbrot points.

'
SCREEN 13
 OUT &H3C8, 0
FOR i = 0 TO 255
	 OUT &H3C9, i \ 4
	 OUT &H3C9, i \ 4
	 OUT &H3C9, i \ 4
 NEXT
COLOR 255
'If you increase niter you will have a crisper but slower rendering
CONST niter = 300

'set up arrays to save intermediate points
DIM real(niter)
DIM imag(niter)


DO
	'seed the mandelbrot series with random points of complex plane
	FOR x = -2.5 + ii! TO .9 STEP .01
		FOR j = -1.2 + ii! TO 1.2 STEP .01
			x = 4 * RND - 2
			y = 4 * RND - 2
			im = 0: re = 0: re2 = 0: im2 = 0
			diverts% = 0
			'iterate the mandelbrot formula
			FOR iter% = 0 TO niter
				im = 2 * re * im + x
				re = re2 - im2 + y
				'Save the intermediate points of the series to arrays
				real(iter%) = re
				imag(iter%) = im
				im2 = im * im
				re2 = re * re
				'If the seed point leads to a diverting series, we are interested
				IF re2 + im2 > 4 THEN diverts% = 1: EXIT FOR
			NEXT
				 
			'Plot to screen the points of the DIVERGENT series
			IF diverts% THEN
				FOR it% = 0 TO iter%
					xs% = CINT(imag(it%) * 80) + 160
					ys% = CINT(real(it%) * 80) + 150
					PSET (xs%, ys%), (POINT(xs%, ys%) + 1) AND 255
				NEXT
				n& = n& + 1
			END IF
		 NEXT
	NEXT
	ii! = ii! + .001
	'IF (n& AND 255) = 0 THEN LOCATE 1, 1: PRINT n&
LOOP UNTIL n& > 1700000 OR LEN(INKEY$)
SLEEP
'---------------------------------------------------------------------------

