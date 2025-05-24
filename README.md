I was active in the QuickBasic scene 20 years ago. After retiring from work I have intalled DOSBox in my Win10 computer, got a copy of QB4.5 and I'm back into fun. I will start to like QB64 the day it starts to do executables smaller than 100 Kb....

JPEGV4.ZIP A VESA JPEG Viewer i made in 2003. Runs in Qbasic. Displays Grayscale and YCbCr colorspace, non progresive JFIF jpegs. Fast! It allows to select any of the available VESA modes, it sets a palette and dithers the image for 8 bit modes. It fails with RGB or CMYK colorspaces or progressive images.  Includes file browser. The image included in the zip should work. Can be run in QBasic. 

![jpegv4](https://github.com/user-attachments/assets/ca87878c-6460-4250-9e4d-bba1b0e1edc0)

QBANSI2.BAS  A textmode ANSI and ASCII art viewer. It emulates the slow loading thru a 9600 baud mode so the "animated" ANSI images can be enjoyed.

![ansiview](https://github.com/user-attachments/assets/305431b8-dca8-4165-84e5-619c93fac47e)

ANIGIF99.BAS  A 2003 VESA viewer for animated GIF files. It uses eMS memory to keep the decoded individual frames of animated files. I had to adapt it to LZW delayed clear dictionnary commands that were not common back then. When dictionary is full the file should issue a clear, but someone found it could be practical to stop filling the dictionary and continue with what was already in it, this was an innovation in 2003 and i had no file samples then. Nowadays this practice is common. OTOH I added the new file selector that works with DOSBox 

![ball](https://github.com/user-attachments/assets/83135490-f0e1-436b-9b59-736fdba20e1e)

PERIOTBL.BAS Draws the Periodic Table of the Elements in SCREEN12. Uses a great print routine that gets multiple fonts from the VGA ROM and scales them, made by E.F.Deel back in 1993, I have added several options . It uses too a set of DATA for the elements properties from a code by Dieter Folger (2003) . I found both at the ABC Packets. I'm afraid QB64 will not display correctly, as it does'nt emulate the ROM fonts, it works correctly in DOSBox.

![periotbl](https://github.com/user-attachments/assets/e844291c-ffd9-4d17-90d1-60ec8e0b5cf9)


CALEND2.BAS  A perpetual calendar (Gregorian). Prints the year you choose. Month and weekday names are in DATA so it's easy to add more languages (now es,fr, en). A constant allows to choose print weeks starting on sunday. The program reads the events you set for the year in DATA and displays them. 6 colors for events. Events can be for a single year, every year, every month and the nt'h day of week) of a month or each month. Making all sundays as events took a single DATA line. Find instructions in the code.

![calend2](https://github.com/user-attachments/assets/7df8da47-1e2e-4fda-ba64-9d78f8b6b198)


VIAJANTE.BAS This is a demo of the travelling salesman problem. The solving algorithm was posted by Alberto Migliorero. I made place for up to 240 cities in QB4.5, improved the visualization by adding a small 6x5 font to label the cities and added an initialization that joins each city with the closest not visited, it makes the algoritthm's life easier. OTOH The original random initialization is funnier to watch.

![viajante](https://github.com/user-attachments/assets/c87552bf-d7af-4e12-a852-0fb3cc354aac)


MELINDA.BAS A different way to display the Mandelbrot set, discovered by Melinda Green in 1993, it accumulates the intermediate values of the points where the series diverges. It's an old SCREEN13 code that does zillions of iterations, after a couple of minutes appears an image that suggests a person meditating in the lotus position. The original name given by Melinda to the image is nowadays non-PC.

![melinda](https://github.com/user-attachments/assets/7e11e4a4-4bf1-4d90-a15a-a139737a7b7a)


WORLD.ZIP An old demo of an Earth globe.  It wraps an Earth map in PCX to a sphere and makes it turn and have sunrises and sunsets on a starred background. Made in QB4.5, it will not work in QBasic1.1 because of memory issues.

![world](https://github.com/user-attachments/assets/a14d6173-95b5-416d-9f2e-ed209aab6887)

3DVESA.ZIP Another old QB45 code. It rotates and zooms 3 d meshes in VESA mode, in bare polygon or in Gouraud textured view. It opens (obsolete) plg and o3a files with not many polygons. Its buggy, I could not manage to swap VESA pages without flickering and there are slits between the polygons. It calls interrupts. 

![3dvesa](https://github.com/user-attachments/assets/f06642c5-ab30-4da4-ab13-2198eac73eea)

REVERSI2.BAS An old code by Microsoft based on the classic board game, updated by me. I made it SCREEN 12 (originally it used modes 10, 9, 2 or 3). No more black background on texts. Pieces flicker at move or flip so you can follow the move. Crashes in expert mode solved (the original programmer coded as if QB had shortcut evaluation of expresions...). Updated help and menu options.

![reversi](https://github.com/user-attachments/assets/3632aa87-4409-4e48-b85c-1738cf391fc0)

BIORYTHM.BAS  A pretext to post a bunch of tested calendrical functions (day of week, days in month, leap year, gregorian date <--> julian day) and a 6x5 font (only numbers). The program asks the birth date then it displays the biorythm curve for the current month.

![biorythm](https://github.com/user-attachments/assets/ec54ea0b-663b-48ba-8a18-7b7a4cd3c334)

SPIGOT2.BAS Gets 1000 decimals of PI in less than 5 seconds using an optimized spigot algorithm.

![spigot2](https://github.com/user-attachments/assets/865476f5-c19c-40dc-9ce8-57695ded72f9)

FILESEL26.BAS  A file selector Function based in the FILES keyword of QB. Use cursor to change directory and select a file. Can filter the files displayed by extensions. Does'nt use shell calls so a crippled COMMAND as the one in DosBox is not a problem.

![filesel26](https://github.com/user-attachments/assets/e73109d5-bfb1-4bb3-bf21-0b8486bc5ff1)

RGBPAL.BAS A palette for SCREEN13 that displays the whole range of colors in 254 palette slots. No luminance or saturation variation...

![rgbpal](https://github.com/user-attachments/assets/3140d826-99b1-488f-b6b4-6a95fa46ff68)

WORDHAS2.BAS It tallies the word frequencies of an ASCII text. It outputs a file with a sorted list of every word and its frequency. It implements a hash table and a quicksort algorithm. It processes the Complete Works by William Shakespeare in 6 seconds.

![wordhash](https://github.com/user-attachments/assets/dc398b82-99a5-4d64-8f38-b7fc26ad0426)
