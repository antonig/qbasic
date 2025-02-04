'==========================================================================
' Subject: Periodic Table of Elements         Date: 1/2005 Antoni Gual Via

'-------------------------------------------------------------------------

' Header for gprintx ----------------------------------------------------
DEFINT A-Z
TYPE regtype
	ax    AS INTEGER
	bx    AS INTEGER
	cx    AS INTEGER
	dx    AS INTEGER
	bp    AS INTEGER
	si    AS INTEGER
	di    AS INTEGER
	flags AS INTEGER
	ds    AS INTEGER
	es    AS INTEGER

END TYPE

DIM SHARED inreg AS regtype, outreg AS regtype
DECLARE SUB interruptx (inum, inreg AS regtype, outreg AS regtype)


DECLARE SUB gprintx (text$, x, Y, Colr, tsize)
CONST font8x8 = 1
CONST font14x8 = 2
CONST font16x8 = 3
CONST fontdbhe = 4
CONST fontdbwi = 8
CONST fontbgr = 16
CONST fontulin = 32
'not yet implemented

CONST fontrigh = 64
CONST fontcent = 128
'------------------------------------------------------------------------

CONST false = 0
CONST true = -1

TYPE typel
	co AS INTEGER
	ro AS INTEGER
	clr AS INTEGER
	sim AS STRING * 2
	num AS INTEGER
	pato AS SINGLE
	nom AS STRING * 10
	fusi AS SINGLE
	ebu AS SINGLE
	npi AS SINGLE
	disc AS INTEGER
END TYPE



DIM el(1 TO 118) AS typel

RESTORE elementos
FOR i = 1 TO 118
	 READ el(i).co, el(i).ro, el(i).clr, el(i).sim, el(i).num, el(i).pato, el(i).nom, a$, el(i).fusi, el(i).ebu, el(i).npi, el(i).disc
NEXT

'data check
'FOR i = 1 TO 118
'   PRINT USING "## ## ## \\ ### ####.# \        \ +####.# +####.# +####.# ####"; el(i).co; el(i).ro; el(i).clr; el(i).sim; el(i).num; el(i).pato; el(i).nom; el(i).fusi; el(i).ebu; el(i).npi; el(i).disc
'NEXT




SCREEN 12

rowh = 45
colw = 35
row1 = 40
row8 = row1 + rowh * 7 + 10
row9 = row8 + rowh + 10
col1 = 5
col8 = 8
col9 = 8


PALETTE 1, &H121212
PALETTE 2, &H60606


'strips background
LINE (0, 0)-STEP(640, 480), 1, BF
FOR i = 0 TO 480
	LINE (0, i)-STEP(640, 0), 2, , &HAAAA
NEXT

gprintx "Periodic Table of the Elements", 0, 2, 12, font14x8 OR fontdbhe OR fontdbwi OR fontulin OR fontcent


FOR i = 1 TO UBOUND(el)
	SELECT CASE el(i).ro
	CASE 1 TO 7
	 row = row1 + rowh * (el(i).ro - 1)
	 col = col1 + colw * (el(i).co - 1)

	CASE 9:
		row = row8
		col = col8 + colw * (el(i).co - 1)


	CASE 10:
		row = row9
		col = col9 + colw * (el(i).co - 1)
	END SELECT

	LINE (col, row)-STEP(colw - 1, rowh - 1), el(i).clr + 1, BF

	'simbolo
	gprintx el(i).sim, col + 2, row + 8, 11, font14x8 OR fontdbhe
 
	'num atom
	gprintx STR$(el(i).num), col + colw, row + 2, 15, font8x8 OR fontrigh  'num atom
	
	'peso atom
	 
	IF el(i).pato < 100 THEN
		a$ = STR$(INT(el(i).pato * 10) / 10)
	ELSE
		a$ = STR$(INT(el(i).pato))

	END IF
	gprintx a$, col + colw, row + 35, 14, font8x8 OR fontrigh  'peso atom
NEXT
 
DO: LOOP UNTIL (LEN(INKEY$))
END


'Chemical elements data
'Data Set by  Dieter Folger    found in the ABC Packet for 2003
'

elementos:

DATA 1,1,1,"H",1,1.0079,"Hydrogen","H",-259,-253,0.09,1776
DATA 18,1,2,"He",2,4.0026,"Helium","He",-272,-269,0.18,1895

DATA 1,2,3,"Li",3,6.941,"Lithium","Li",180,1347,0.53,1817
DATA 2,2,4,"Be",4,9.0122,"Beryllium","Be",1278,2970,1.85,1797
DATA 13,2,1,"B",5,10.811,"Boron","B",2300,2550,2.34,1808
DATA 14,2,1,"C",6,12.0107,"Carbon","C",3500,4827,2.26,-9999
DATA 15,2,1,"N",7,14.0067,"Nitrogen","N",-210,-196,1.25,1772
DATA 16,2,1,"0",8,15.9994,"Oxygen","O",-218,-183,1.43,1774
DATA 17,2,1,"F",9,18.9984,"Fluorine","F",-220,-188,1.7,1886
DATA 18,2,2,"Ne",10,20.1797,"Neon","Ne",-249,-246,0.9,1898

DATA 1,3,3,"Na",11,22.9897,"Sodium","Na",98,883,0.97,1807
DATA 2,3,4,"Mg",12,24.305,"Magnesium","Mg",639,1090,1.74,1755
DATA 13,3,5,"Al",13,26.9815,"Aluminum","Al",660,2467,2.7,1825
DATA 14,3,1,"Si",14,28.0855,"Silicon","Si",1410,2355,2.33,1824
DATA 15,3,1,"P",15,30.9738,"Phosphorus","P",44,280,1.82,1669
DATA 16,3,1,"S",16,32.065,"Sulfur","S",113,445,2.07,-9999
DATA 17,3,1,"Cl",17,35.453,"Chlorine","Cl",-101,-35,3.21,1774
DATA 18,3,2,"Ar",18,39.948,"Argon","Ar",-189,-186,1.78,1894

DATA 1,4,3,"K",19,39.0983,"Potassium","K",64,774,0.86,1807
DATA 2,4,4,"Ca",20,40.078,"Calcium","Ca",839,1484,1.55,1808
DATA 3,4,7,"Sc",21,44.9559,"Scandium","Sc",1539,2832,2.99,1879
DATA 4,4,7,"Ti",22,47.867,"Titanium","Ti",1660,3287,4.54,1791
DATA 5,4,7,"V",23,50.9415,"Vanadium","V",1890,3380,6.11,1830
DATA 6,4,7,"Cr",24,51.9961,"Chromium","Cr",1857,2672,7.19,1797
DATA 7,4,7,"Mn",25,54.938,"Manganese","Mn",1245,1962,7.43,1774
DATA 8,4,7,"Fe",26,55.845,"Iron","Fe",1535,2750,7.87,-9999
DATA 9,4,7,"Co",27,58.9332,"Cobalt","Co",1495,2870,8.9,1735
DATA 10,4,7,"Ni",28,58.6934,"Nickel","Ni",1453,2732,8.9,1751
DATA 11,4,7,"Cu",29,63.546,"Copper","Cu",1083,2567,8.96,-9999
DATA 12,4,7,"Zn",30,65.39,"Zinc","Zn",420,907,7.13,-9999
DATA 13,4,5,"Ga",31,69.723,"Gallium","Ga",30,2403,5.9,1875
DATA 14,4,5,"Ge",32,72.64,"Germanium","Ge",937,2830,5.32,1886
DATA 15,4,1,"As",33,74.9216,"Arsenic","As",81,613,5.72,-9999
DATA 16,4,1,"Se",34,78.96,"Selenium","Se",217,685,4.79,1817
DATA 17,4,1,"Br",35,79.904,"Bromine","Br",-7,59,3.12,1826
DATA 18,4,2,"Kr",36,83.8,"Krypton","Kr",-157,-153,3.75,1898

DATA 1,5,3,"Rb",37,85.4678,"Rubidium","Rb",39,688,1.63,1861
DATA 2,5,4,"Sr",38,87.62,"Strontium","Sr",769,1384,2.54,1790
DATA 3,5,7,"Y",39,88.9059,"Yttrium","Y",1523,3337,4.47,1794
DATA 4,5,7,"Zr",40,91.224,"Zirconium","Zr",1852,4377,6.51,1789
DATA 5,5,7,"Nb",41,92.9064,"Niobium","Nb",2468,4927,8.57,1801
DATA 6,5,7,"Mo",42,95.94,"Molybdenum","Mo",2617,4612,10.22,1781
DATA 7,5,7,"Tc",43,98,"Technetium","Tc",2200,4877,11.5,1937
DATA 8,5,7,"Ru",44,101.07,"Ruthenium","Ru",2250,3900,12.37,1844
DATA 9,5,7,"Rh",45,102.9055,"Rhodium","Rh",1966,3727,12.41,1803
DATA 10,5,7,"Pd",46,106.42,"Palladium","Pd",1552,2927,12.02,1803
DATA 11,5,7,"Ag",47,107.8682,"Silver","Ag",962,2212,10.5,-9999
DATA 12,5,7,"Cd",48,112.411,"Cadmium","Cd",321,765,8.65,1817
DATA 13,5,5,"In",49,114.818,"Indium","In",157,2000,7.31,1863
DATA 14,5,5,"Sn",50,118.71,"Tin","Sn",232,2270,7.31,-9999
DATA 15,5,5,"Sb",51,121.76,"Antimony","Sb",630,1750,6.68,-9999
DATA 16,5,1,"Te",52,127.6,"Tellurium","Te",449,990,6.24,1783
DATA 17,5,1,"I",53,126.9045,"Iodine","I",114,184,4.93,1811
DATA 18,5,2,"Xe",54,131.293,"Xenon","Xe",-112,-108,5.9,1898

DATA 1,6,3,"Cs",55,132.9055,"Cesium","Cs",29,678,1.87,1860
DATA 2,6,4,"Ba",56,137.327,"Barium","Ba",725,1140,3.59,1808
DATA 4,6,7,"Hf",72,178.49,"Hafnium","Hf",2150,5400,13.31,1923
DATA 5,6,7,"Ta",73,180.9479,"Tantalum","Ta",2996,5425,16.65,1802
DATA 6,6,7,"W",74,183.84,"Tungsten","W",3410,5660,19.35,1783
DATA 7,6,7,"Re",75,186.207,"Rhenium","Re",3180,5627,21.04,1925
DATA 8,6,7,"Os",76,190.23,"Osmium","Os",3045,5027,22.6,1803
DATA 9,6,7,"Ir",77,192.217,"Iridium","Ir",2410,4527,22.4,1803
DATA 10,6,7,"Pt",78,195.078,"Platinum","Pt",1772,3827,21.45,1735
DATA 11,6,7,"Au",79,196.9665,"Gold","Au",1064,2807,19.32,-9999
DATA 12,6,7,"Hg",80,200.59,"Mercury","Hg",-39,357,13.55,-9999
DATA 13,6,5,"Tl",81,204.3833,"Thallium","Tl",303,1457,11.85,1861
DATA 14,6,5,"Pb",82,207.2,"Lead","Pb",327,1740,11.35,-9999
DATA 15,6,5,"Bi",83,208.9804,"Bismuth","Bi",271,1560,9.75,-9999
DATA 16,6,5,"Po",84,209,"Polonium","Po",254,962,9.3,1898
DATA 17,6,1,"At",85,210,"Astatine","At",302,337,9999,1940
DATA 18,6,2,"Rn",86,222,"Radon","Rn", -71, -62, 9.73, 190

DATA 1,7,3,"Fr",87,223,"Francium","Fr",27,677,-,1939
DATA 2,7,4,"Ra",88,226,"Radium","Ra",700,1737,5.5,1898
DATA 4,7,7,"Rf",104,261,"Rutherfordium","Rf" ,2100 ,5500, 18.1, 1964
DATA 5,7,7,"Db",105,262,"Dubnium",Db ,2500 ,5500, 9999, 1967
DATA 6,7,7,"Sg",106,266,"Seaborgium","Sg",9999,9999,9999,1974
DATA 7,7,7,"Bh",107,264,"Bohrium","Bh",9999,9999,9999,1981
DATA 8,7,7,"Hs",108,277,"Hassium","Hs",9999,9999,9999,1984
DATA 9,7,7,"Mt",109,268,"Meitnerium","Mt",9999,9999,9999,1982
DATA 10,7,7,"Ds",110,269,"Darsmstadium","Ds",9999,9999,9999,1994
DATA 11,7,7,"Rg",111,272,"Roentgenium","Rg",9999,9999,9999,1994
DATA 12,7,7,"Cn",112,285,"Copernicium","Cn",9999,9999,9999,1996
DATA 13,7,7,"Nh",113,284,"Nihonio","Nh",9999,9999,9999,9999

DATA 14,7,7,"Fl",114, 289, "Flerovium","Fl", 9999, 9999, 9999, 9999
DATA 15,7,7,"Mc",115, 288, " Moscovium","Mc", 9999, 9999, 9999, 9999

DATA 16,7,7,"Lv",116,292,"Livermorium","Lv",9999,9999,9999,9999
DATA 17,7,7,"Ts",117,294,"Tenesium","Ts",9999,9999,9999,9999

DATA 18,7,7,"Og",118, 294,"Oganeson","Og",9999,9999,9999,9999

DATA 4,9,6,"La",57,138.9055,"Lanthanum","La",920,3469,6.15,1839
DATA 5,9,6,"Ce",58,140.116,"Cerium","Ce",795,3257,6.77,1803
DATA 6,9,6,"Pr",59,140.9077,"Praseodymium","Pr",935,3127,6.77,1885
DATA 7,9,6,"Nd",60,144.24,"Neodymium","Nd",1010,3127,7.01,1885
DATA 8,9,6,"Pm",61,145,"Promethium","Pm",1100,3000,7.3,1945
DATA 9,9,6,"Sm",62,150.36,"Samarium","Sm",1072,1900,7.52,1879
DATA 10,9,6,"Eu",63,151.964,"Europium","Eu",822,1597,5.24,1901
DATA 11,9,6,"Gd",64,157.25,"Gadolinium","Gd",1311,3233,7.9,1880
DATA 12,9,6,"Tb",65,158.9253,"Terbium","Tb",1360,3041,8.23,1843
DATA 13,9,6,"Dy",66,162.5,"Dysprosium","Dy",1412,2562,8.55,1886
DATA 14,9,6,"Ho",67,164.9303,"Holmium","Ho",1470,2720,8.8,1867
DATA 15,9,6,"Er",68,167.259,"Erbium","Er",1522,2510,9.07,1842
DATA 16,9,6,"Tm",69,168.9342,"Thulium","Tm",1545,1727,9.32,1879
DATA 17,9,6,"Yb",70,173.04,"Ytterbium","Yb",824,1466,6.9,1878
DATA 18,9,6,"Lu",71,174.967,"Lutetium","Lu",1656,3315,9.84,1907

DATA 4,10,8,"Ac",89,227,"Actinium","Ac",1050,3200,10.07,1899
DATA 5,10,8,"Th",90,232.0381,"Thorium","Th",1750,4790,11.72,1829
DATA 6,10,8,"Pa",91,231.0359,"Protactinium","Pa",1568,9999,15.4,1913
DATA 7,10,8,"U",92,238.0289,"Uranium","U",1132,3818,18.95,1789
DATA 8,10,8,"Np",93,237,"Neptunium","Np",640,3902,20.2,1940
DATA 9,10,8,"Pu",94,244,"Plutonium","Pu",640,3235,19.84,1940
DATA 10,10,8,"Am",95,243,"Americium","Am",994,2607,13.67,1944
DATA 11,10,8,"Cm",96,247,"Curium","Cm",1340,9999,13.5,1944
DATA 12,10,8,"Bk",97,247,"Berkelium","Bk",986,9999,14.78,1949
DATA 13,10,8,"Cf",98,251,"Californium","Cf",900,9999,15.1,1950
DATA 14,10,8,"Es",99,252,"Einsteinium","Es",860,9999,9999,1952
DATA 15,10,8,"Fm",100,257,"Fermium","Fm",1527,9999,9999,1952
DATA 16,10,8,"Md",101,258,"Mendelevium","Md",9999,9999,9999,1955
DATA 17,10,8,"No",102,259,"Nobelium","No",827,9999,9999,1958
DATA 18,10,8,"Lr",103,262,"Lawrencium","Lr",1627,9999,9999,1961

SUB gprintx (text$, x, Y, Colr, tsize) STATIC
'-------------------------------------------------------------------
'*** GPrintX - E.F.Deel, 1/15/93
'Adapted to QB4.5  by Andreas Hoeffken 1996
'modified by Antoni Gual 2014
'-------------------------------------------------------------------
'*** GRAPHIC TEXT PRINTING SUBROUTINE ***
'X, Y = Graphics mode pixel coordinates
'Colr = combined FG and BG (BG*16 + FG), BG = 0 leaves existing background
'TSize is coded text size: use constants
' font8x8| font8x14| font8x16 [or fontdbwi] [or fontdbhe]

' TO DO
' ok duplicacion directa scanline
' ok centrado en pantalla
' ok subrayado
' ok saltar scanlinesen blanco
' ok alinear derecha
' ok subrayado

 text$ = LTRIM$(RTRIM$(text$))
	l = LEN(text$)
	IF l = 0 THEN EXIT SUB    'abort if no text is given
		
	IF tsize <> SaveSze THEN 'another test so we don't repeat if not required
		dx = 8
		SELECT CASE tsize AND 3
			CASE font8x8: Ftype = &H300: Font = 8: dy = 8
			CASE font14x8: Ftype = &H200: Font = 14: dy = 14
			CASE font16x8: Ftype = &H600: Font = 16: dy = 16
		END SELECT
		IF tsize AND fontdbhe THEN dy = dy * 2
		IF tsize AND fontdbwi THEN dx = dx * 2


		'getting the segment and address of the font in ROM
		inreg.ax = &H1130                   'AH = 11h: access of an internal font
																				'AL = 30h: get address of a font
		inreg.bx = Ftype                    'BX = type of font
		CALL interruptx(&H10, inreg, outreg) 'INT 10h, gives:
		FontSeg = outreg.es                  ' segment
		FontAdrs = outreg.bp                 ' offset
		SaveSze = tsize                      'save the size for next time
	END IF

		IF tsize AND fontrigh THEN x = x - (l * dx)
		IF tsize AND fontcent THEN x = (640 - (l * dx)) \ 2




	'get color, draw backround if required
	FG = Colr AND 15
	IF tsize AND fontbgr THEN
		BG = Colr \ 16
		LINE (x, Y)-STEP(l * dx, dy - 1), BG, BF 'draw background
	END IF
 

	DEF SEG = outreg.es                'segment of VGA-BIOS

	FOR i = 1 TO l                     'For each character in string
		cc = ASC(MID$(text$, i, 1))      'get character code (0...255)
		Addr = Font * cc + FontAdrs      'find the address (in VGA-BIOS)
		cy = Y                           'top of character location

		FOR j = 0 TO Font - 1            'For each scanline in font
			scanline = PEEK(Addr + j)      'get the scanline byte
		 
		 
			IF tsize AND fontulin THEN
					IF j = Font - 2 THEN scanline = &HFF 'underline
			END IF
			
			IF scanline = 0 THEN
																							 'skip blank scanlines
				IF tsize AND fontdbhe THEN cy = cy + 1
			ELSE

				IF tsize AND fontdbwi THEN     'double width
					 
					GOSUB duplica ' toma scanline devuelve scanline con bits duplicados
					LINE (x, cy)-STEP(15, 0), FG, , scanline
																			 
					IF tsize AND fontdbhe THEN
					 cy = cy + 1
						LINE (x, cy)-STEP(15, 0), FG, , scanline
					END IF
					
				ELSEIF tsize AND fontdbhe THEN 'double height only    
					zwi& = CLNG(scanline) * 256  'lo byte --> hi byte
					scanline = zwi& - (65536 AND (zwi& > 32767))
						
					LINE (x, cy)-STEP(7, 0), FG, , scanline
					cy = cy + 1
					LINE (x, cy)-STEP(7, 0), FG, , scanline

				ELSE                           'normal characters
					zwi& = CLNG(scanline) * 256  'lo byte --> hi byte
					scanline = zwi& - (65536 AND (zwi& > 32767))
					LINE (x, cy)-STEP(7, 0), FG, , scanline
															
				END IF
			END IF
			cy = cy + 1
		NEXT j                           'next
		 
		x = x + dx                       'next character
		NEXT
EXIT SUB

duplica:   'duplica bits de scanline para doble ancho. De Bit Twiddling Hacks
 u& = scanline
 u& = (u& OR (u& * 16&)) AND &HF0F0F0F
 u& = (u& OR (u& * 4&)) AND &H33333333
 u& = (u& OR (u& * 2&)) AND &H55555555
 u& = (u& OR (u& * 2&))
 scanline = u& - (65536 AND (u& > 32767))
RETURN

END SUB

