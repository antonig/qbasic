I was active in the QuickBasic scene 20 years ago. After retiring from work I have intalled DOSBox in my Win10 computer, got a copy of QB4.5 and I'm back into fun. I will start to like QB64 the day it starts to do executables smaller than 100 Kb....

PERIOTBL.BAS Draws the Periodic Table of the Elements in SCREEN12. Uses a great print routine that gets multiple fonts from the VGA ROM and scales them. It was made by E.F.Deel back in 1993, and I have added several options .  I use too a set of DATA for the elements properties from a code by Dieter Folger (2003) . I found both at the ABC Packets. I'm afraid QB64 will not display the fonts, it's allergic to Interrupt calls.

![periotbl](https://github.com/user-attachments/assets/e844291c-ffd9-4d17-90d1-60ec8e0b5cf9)


CALEND2.BAS  A perpetual calendar (Gregorian). Prints the year you choose. Month and weekday names are in DATA so it's easy to add more languages (now es,fr, en). A constant allows to choose print weeks starting on sunday. The program reads the events you set for the year in DATA and displays them. 6 colors for events. Events can be a single year, every year, every month and the nt'h day of week) of a month or each month. Making all sundays as events took a single DATA line. Find instructions in the code.

![calend2](https://github.com/user-attachments/assets/7df8da47-1e2e-4fda-ba64-9d78f8b6b198)


VIAJANTE.BAS This is a demo of the travelling salesman problem. The solving algorithm was posted by Alberto Migliorero. I made place for up to 240 cities in QB4.5, improved the visualization by adding a small font to label the cities and added an initialization that joins each city with the closest not visited that makes the algoritthm's life easier. OTOH The original random initialization is funnier to watch.

![viajante](https://github.com/user-attachments/assets/c87552bf-d7af-4e12-a852-0fb3cc354aac)


MELINDA.BAS A different way to display the Mandelbrot set, discovered by Melinda Green in 1993, it accumulates the intermediate values of the points where the series diverges. It's an old SCREEN13 code that does zillions of iterations, after a couple of minutes appears an image that suggests a person meditating in the lotus position. The original name given by Melinda to the image is nowadays non-PC.

![melinda](https://github.com/user-attachments/assets/7e11e4a4-4bf1-4d90-a15a-a139737a7b7a)


WORLD.ZIP An old demo of an Earth globe.  It maps an Earth map in PCX to a sphere and makes it turn and have sunrises and sunsets on a starred background. Made in QB4.5, will not work in QBasic1.1 because of memory issues.

![world](https://github.com/user-attachments/assets/a14d6173-95b5-416d-9f2e-ed209aab6887)

3DVESA.ZIP Another old QB45 code. It rotates and zooms the then usual 3D teapot in a VESA mode, in bare polygon or in Gouraud textured view. Its buggy, I could not manage to swap VESA pages without flickering and there are slits between the polygons. It calls interrupts.  Make the folder where the .bas and .plg are as default. 

![3dvesa](https://github.com/user-attachments/assets/f06642c5-ab30-4da4-ab13-2198eac73eea)
