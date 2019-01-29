

## 4.2 BUFRENC

BUFRENC processes an ASCII input file to produce one or more BUFR output files.



```
FXYTBL FXY table file
BUFRFIL BUFR output file
FHOUR Forecast hour
UKAFIL Intermediate input/output ASCII file
```


BUFRENC is a table-driven BUFR encoder program which reads user input parame-

ters, tables and an ASCII file to produce one or more BUFR output files. The input pa-

rameters include FXY file name, BUFR output file names, forecast hour and ASCII

file.

## Examples

1. FXYTBL = fxyswhcld;fxyswhfrt

#BUFRFIL =

#FHOUR = 24

```
UKAFIL = SIGWXHI.txt
```
#2. FXYTBL = SWH

#BUFRFIL =

#FHOUR = 18

```
UKAFIL = SIGWXHI.txt
```
### Error Messages

```
[BUFRENC-1] Invalid input for FXY file names.

[BUFRENC-2] Invalid input for the ASCII file.

[BUFRENC-3] Error opening input ASCII file.

[BUFRENC-4] Error read the header information for input ASCII file.

[BUFRENC-5] Error with the number of BUFR output files.

[BUFRENC-6] Cannot open FXY table.
```

## 4.3 CPCGSTN

CPCGSTN searches for stations located inside specified areas from a Vector Graphics

File.



```
VGFILE Vgfile | scale file | attribute file
DATTIM Date/time
STNFIL Station information file
OUTPUT Output device/filename
```


CPCGSTN lists all stations in a given station table, and indicates which stations are lo-

cated inside areas drawn in the Vector Graphics File. The areas must be drawn using

the "tick mark" special line. The file may contain multiple areas that do not overlap.

If the given polygon is not closed, then the appropriate portion of the U.S. border is

used to construct a closed polygon.

The DATTIM is only used to fill in the format of the output listing. In this way the out-

put listing may be used as input to SFEDIT.

The output includes the station number, GEMPAK time, the flag indicating if the sta-

tion is inside (1) or outside (0) of the polygon and the polygon color.

## Examples

1. Identify the stations located in the polygons in the VG file vgf.vgf. Use

the current system time and the station table stns_ll90.tbl. The output is

displayed to the terminal.

```
VGFILE = vgf.vgf
DATTIM = last
STNFIL = stns_ll90.tbl
OUTPUT = t
```
2. As in example 1, except the output is written to the file data.dat.

```
VGFILE = vgf.vgf
DATTIM = last
STNFIL = stns_ll90.tbl
OUTPUT = f/data.dat
```

#EXAMPLE OUTPUT

PARM = FLAG;ICLR

#STN YYMMDD/HHMM FLAG ICLR

#69002 010831/1822 1 17

#69007 010831/1822 1 17

#69008 010831/1822 1 17

#69012 010831/1822 0 0

#69013 010831/1822 0 0

#69014 010831/1822 0 0

#69016 010831/1822 0 0

#69017 010831/1822 0 0

#69019 010831/1822 0 0

#70026 010831/1822 0 0

.... .... .... ....

### Error Messages

```
[CPCGSTN-1] Fatal error initializing TAE.

[CPCGSTN-2] Fatal error reading TAE parameters.

[CPCGSTN-3] Error initializing GEMPLT.

[CPCGSTN]
```

## 4.4 GD2NDFD

GD2NDFD converts a GEMPAK grid to an NDFD GRIB2 file.



```
GDFILE Grid file
GFUNC Scalar grid
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GBFILE GRIB data file name
CENTER Originating Center ID #/Sub-Center ID #
WMOHDR WMO_ID/Origin_ID/DDHHMM
```


GD2NDFD converts a GEMPAK grid to an NDFD GRIB2 file.

The input grids for the computation must be in GDFILE. The converted GRIB2 mes-

sage is added to the output file, GBFILE. If GBFILE does not exist, it will be created.

The parameter CENTER allows the GD2NDFD user to specify the originating or gen-

erating center ID and sub-center ID of the GRIB2 message.

A World Meteorological Organization (WMO) header may be prefixed to the GRIB

message by giving at least the first six bytes of the header in the WMOHDR input pa-

rameter. Default values will be supplied for the originating center (KWBC) and refer-

ence time.

## Examples

Convert all times for paramter TMPK from GEMPAK grid format to NDFD

GRIB2 format. The output is written to file hrcbob.ndfd.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = tmpk
GDATTIM = all
GLEVEL = 0
GVCORD = none
GBFILE = hrcbob.ndfd
CENTER = 7/
WMOHDR = HTRE
```

### Error Messages

```
[GD2NDFD+6] WMO header center ID inconsistent with PDS byte 5.

[GD2NDFD-1] Fatal error initializing TAE.

[GD2NDFD-2] Fatal error reading TAE parameters.

[GD2NDFD-3] Error initializing GEMPLT.

[GD2NDFD-4] Grid file could not be opened.

[GD2NDFD-5] Grid navigation could not be set in GEMPLT.

[GD2NDFD-6] Grid diagnostics package initialization failed.

[GD2NDFD-7] Error interpolating to output grid.

[GD2NDFD-8] GRIB message is too long.

[GD2NDFD-9] First 6 characters of WMO header must be given.

[GD2NDFD-10] User supplied WMO header is too long.

[GD2NDFD-11] Invalid grid navigation set in PROJ.

[GD2NDFD-12] Invalid grid area set in GRDAREA or CPYFIL.

[GD2NDFD-13] Invalid grid size.

[GD2NDFD-14] (i,j) -> lat/lon transformation failed.

[GD2NDFD-15] lat/lon -> (i,j) transformation failed.

[GD2NDFD-16] Cannot set output grid navigation.

[GD2NDFD-17] Cannot set input grid navigation.

[GD2NDFD-18] Invalid horizontal interpolation type.

[GD2NDFD] BDS errors:

[GD2NDFD-21] BDS section is too long.

[GD2NDFD-22] Number of packing bits is invalid.

[GD2NDFD-23] Data range is not valid.

[GD2NDFD-24] Binary scaling is invalid.

[GD2NDFD-25] Computation of the reference value failed.

[GD2NDFD-26] BDS array accomodation is too small.
```

[GD2NDFD-27] The calculation of the # of bits needed failed.

[GD2NDFD-28] All data is missing -- no GRIB message made.

[GD2NDFD] BMS errors:

[GD2NDFD-41] BMS section is too long.

[GD2NDFD-42] BMS array allocation is too small.

[GD2NDFD] GDS errors:

[GD2NDFD-61] Not enough bytes for the GDS.

[GD2NDFD-62] Number in i direction is too large.

[GD2NDFD-63] Number in j direction is too large.

[GD2NDFD-64] Latitude 1 is invalid.

[GD2NDFD-65] Longitude 1 is invalid.

[GD2NDFD-66] Latitude 2 is invalid.

[GD2NDFD-67] Longitude 2 is invalid.

[GD2NDFD-68] Rotated CED projection is not supported.

[GD2NDFD-69] Rotated STR projection is not supported.

[GD2NDFD-70] DX grid increment is invalid.

[GD2NDFD-71] DY grid increment is invalid.

[GD2NDFD-72] Central longitude is invalid.

[GD2NDFD-73] True latitudes are invalid.

[GD2NDFD-74] Rotated MER projection is not supported.

[GD2NDFD-75] Grid projection is not supported.

[GD2NDFD] PDS errors:

[GD2NDFD-83] Cannot find parameter in tables.

[GD2NDFD-84] Parameter # found is not valid in GRIB.

[GD2NDFD-85] Vertical coordinate not found in table.

[GD2NDFD-86] Vertical coordinate is not valid in GRIB.

[GD2NDFD-87] Level value is too large for GRIB.


[GD2NDFD-88] Level is less than zero.

[GD2NDFD-89] Dual GEMPAK times not supported.

[GD2NDFD-90] 4-digit year required in in-line (^) DATTIM.

[GD2NDFD-91] Forecast must be in hours.

[GD2NDFD-92] Array allocation for PDS is too small.

[GD2NDFD-93] Decimal scale factor is too large.

[GD2NDFD-94] Parameter name is too long to be in table.


## 4.5 GDBIINT

GDBIINT interpolates grids from one projection to another



```
GDFILE Grid file
GDOUTF Output grid file
GFUNC Scalar grid
GLEVEL Grid level
GVCORD Grid vertical coordinate
GDATTIM Grid date/time
GDNUM Grid numbers
```


GDBIINT uses bi-linear interpolation to convert grid point data from one projection to

another.

The output file must be created first using GDCFIL. The program determines the loca-

tion of the output grid point locations withing the input grid file domain. Each grid in

the input file is interpolated to the output file.

If GDNUM is ALL, all grids from the input file will be interpolated to the output file.

Otherwise, the single grid specified by GFUNC, GDATTIM, GLEVEL and GVCORD

will be interpolated.

## Examples

1. Convert a data set from grid 211 to grid 87. First create a new grid file

with gdcfil using CPYFIL = #87.

```
GDFILE = $HDS/98033112_grid211.gem
GDOUTF = $HDS/98033112_grid87.gem
GDNUM = all
```
2. Convert the LAND mask from grid projection 212 to grid projection 211.

```
GDFILE = $HDS/98033112_grid211.gem
GDOUTF = $HDS/98033112_grid87.gem
GFUNC = mask
GDATTIM = f
GLEVEL = 0
GVCORD = none
GDNUM =
```

### Error Messages

```
[GDBIINT-1] Fatal error initializing TAE.

[GDBIINT-2] Fatal error reading TAE parameters.
```

## 4.6 GDCFIL

GDCFIL creates a GEMPAK grid file.



```
GDOUTF Output grid file
PROJ Map projection/angles/margins|drop flag
GRDAREA Area covered by grid
KXKY Number of grid points in x;y
MAXGRD Maximum number of grids
CPYFIL Grid file whose navigation is to be used in new grid file |
ANLYSS Grid analysis block
```


This program creates a GEMPAK grid file. Information about the file to be created is

received from the input values, from the grid navigation table, or from an existing grid

file.

Each grid file must contain navigation information. It may also contain analysis block

information which is used when an objective analysis program writes to the grid file.

If the value in CPYFIL begins with a #, the rest of the string specifies a grid number or

name contained in the grid navigation table. Any other non-blank entry in CPYFIL is

taken as the name of an existing grid file, whose navigation and analysis blocks will be

copied to the new grid file.

If CPYFIL is blank, the navigation and analysis information will be obtained from

PROJ, GRDAREA, KXKY, and ANLYSS. KXKY contains the number of points in

the x and y directions. The two integers must be separated with a semicolon. If the grid

projection type specified in PROJ is CED, the grid spacing consisting of deltax and

deltay may be entered in KXKY by prefixing the numbers with a #. DELTAX and

DELTAY are in degrees longitude and degrees latitude, respectively. The analysis in-

formation specifies DELTAN, which is the station spacing, and the extend area sepa-

rated by a slash. The extend area is four integers separated by semicolons. If ANLYSS

is blank, default values will be assigned. If there is no analysis block in the file to be

copied, ANLYSS is used to create one.

MAXGRD is the maximum number of grids which will be allowed in the grid file.

## Examples

1. Create a grid file called lfm.grd from the information in the grid

navigation table, allowing up to 1000 grids in the file. PROJ, GRDAREA

and KXKY are ignored since this information will be obtained from the

grid navigation table.


```
GDOUTF = lfm.grd
PROJ =
GRDAREA =
KXKY =
MAXGRD = 1000
CPYFIL = #lfm
ANLYSS =
```
2. Create a grid file called sound.grd with a maximum of 15 grids. The

grid dimensions are 20 by 30; the area is US in a Mercator projection.

The station spacing is 5 degrees of latitude and the grid extension for

objective analysis is 3 grid points in each direction.

```
GDOUTF = sound.grd
PROJ = mer
GRDAREA = us
KXKY = 20;
MAXGRD = 15
CPYFIL =
ANLYSS = 5/3;3;3;
```
3. Create a file for evenly spaced lat/lon grids over area IL- using a grid

spacing of .75 degrees in each direction. Compute reasonable values for

the analysis block.

```
GDOUTF = il.grd
PROJ = ced
GRDAREA = il-
KXKY = #.75;.
MAXGRD = 100
CPYFIL =
ANLYSS =
```
4. Create a grid file called new.grd from the information within grid file

old.grd.

```
GDOUTF = new.grd
PROJ =
GRDAREA =
KXKY =
MAXGRD = 100
CPYFIL = old.grd
ANLYSS =
```

### Error Messages

```
[GDCFIL+1] WARNING: This grid is too large for GEMPAK programs.

[GDCFIL-1] Fatal error initializing TAE.

[GDCFIL-2] Fatal error reading TAE parameters.

[GDCFIL-3] Fatal error initializing GEMPLT.

[GDCFIL-4] Navigation information is invalid.

[GDCFIL-5] Grid area ... is invalid.

[GDCFIL-6] Grid size is invalid.

[GDCFIL-7] The grid file name may not be blank.

[GDCFIL-8] Navigation table cannot be read.

[GDCFIL-9] Grid name ... cannot be found in grid table.

[GDCFIL-10] Extend region is invalid. Try 2;2;2;2.
```

## 4.7 GDCNTR

GDCNTR draws contour lines through a scalar grid.



```
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GDFILE Grid file
CINT Contour interval/min/max
LINE Color/type/width/label/smth/fltr
MAP Map color/dash/width/filter flag
TITLE Title color/line/title
DEVICE Device|name|x size;y size|color type
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
PROJ Map projection/angles/margins|drop flag
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
SCALE Scalar scale / vector scale
LATLON Line color/dash/width/freq/inc/label/format
HILO Color/symbol/rng/rad/cnt/intp
HLSYM HILO txt size/posn/font/wdth/hw
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CONTUR Subbox/smooth
SKIP Skip_cntr/skip_plt_x;skip_plt_y
FINT Fill interval/min/max
FLINE Fill colors/fill types
CTYPE Contour type: C/F
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
```


GDCNTR draws contour lines through a scalar grid computed using the GEMPAK grid

diagnostic functions. Contours may be drawn in any valid GEMPAK projection and

may be overlaid on images.

A list of times may be given in GDATTIM allowing animation of the contours.

Contours may be plotted in different display windows by specifying a name for the XW

driver in DEVICE.

Contours may be displayed as lines or as a color fill. If CTYPE is C, contour lines are

drawn using input from CINT and LINE. If CTYPE is F, filled contours are drawn us-


ing specifications from FINT and FLINE. Both contour lines and filled contours are

drawn if CTYPE is F/C.

The attributes of the contour lines, including the color, line type, line width, and label

frequency are specified in LINE. The four attributes must be separated with slashes;

semicolons separate the values for each attribute. If the line type is set to a single neg-

ative number, negative contour values will have the absolute value of the line type and

positive values will be solid. If the label frequency is set to a single number, n, then

every nth value will be labeled.

CINT specifies the contour interval, minimum value, and maximum value separated

with slashes. A scaling factor may be entered in SCALE. The data in the grid file will

be multiplied by 10 ** SCALE before the contour levels are selected. If no contour in-

terval is entered, a default value which will generate 5 to 10 contour levels will be se-

lected.

The contour fill intervals are specified in FINT; the attributes for the fill are specified

in FLINE. The first color specified in FLINE fills values less than the first level; while

the last color fills values greater than the last level. Therefore, n levels require n+1 col-

ors.

A range of colors may be specified in either FLINE or LINE by specifying starting, end-

ing and increment values in that order separated by dashes. If the increment is missing,

a default of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3 (slanted line). If fill type is

set to 0, solid fill is used. If the fill type is set to a single negative number, negative

values will use the absolute value of the fill type, and positive values will be solid.

The HILO and HLSYM parameters control marking and labeling the values of relative

maxima and minima. CLRBAR allows a color bar to be added for color fill contours.

The SKIP parameter specifies the number of grid points to skip in generating contours.

For example, if SKIP = 1, every other point is used to generate the contours.

## Examples

1. Draw contours every 2 degrees through the 700 mb temperature field for

all of the forecast times. Negative values will be dashed using line type

3; every other contour line will be labeled; all the contour lines will be

drawn in color number 3. The display area will be a enlarged Mercator

map centered on New York. The contours are drawn on a clear screen

with a map in dash pattern 7, dotted lat/lon lines every 5 degrees and no

title.

```
GDATTIM = all
```

#GLEVEL = 700

```
GVCORD = pres
GFUNC = tmpc
GDFILE = $GEMDATA/hrcbob.grd
CINT = 2
LINE = 3/-3/1/
MAP = 1/
TITLE = 0
DEVICE = xw
SATFIL =
RADFIL =
IMCBAR =
PROJ = mer
GAREA = ny-
CLEAR = yes
PANEL = 0
TEXT = 1
SCALE = 999
LATLON = 2/10/1/1/5;
HILO =
HLSYM =
CLRBAR =
CONTUR = 3
SKIP = 0
FINT = 0
FLINE = 10-
CTYPE = C
LUTFIL =
STNPLT =
```
2. Now overlay dewpoint lines on the above plots. The contour interval is

set to 5. The lines will be drawn in color 17, with labeling and line

types as above. The map and lat/lon lines will not be drawn for the

overlay. The title, which includes the date, level and the string

"TEMPERATURE AND DEWPOINT", is added in color 1.

```
GDATTIM = all
GLEVEL = 700
GVCORD = pres
GFUNC = dwpc
GDFILE = $GEMDATA/hrcbob.grd
CINT = 5
LINE = 17/-3/1/
MAP = 0
TITLE = 1//~ @ TEMPERATURE AND DEWPOINT
DEVICE = xw
SATFIL =
RADFIL =
IMCBAR =
PROJ = mer
GAREA = ny-
CLEAR = no
PANEL = 0
```

#TEXT = 1

#SCALE = 999

#LATLON = 0

#HILO =

#HLSYM =

#CLRBAR =

#CONTUR = 3

#SKIP = 0

#FINT = 0

#FLINE = 10-

#CTYPE = C

#LUTFIL =

#STNPLT =

3. Now clear the screen and draw a color fill of the divergence of the

gridded wind, alternating between color 23 and 19, for the 24 hour

forecast time. Scale the data by 10**5 and use a contour interval of 0.5.

Draw contour lines in color 2 using heavy, solid lines, labeling every

contour level. Draw a color bar of the fill colors using the default

conditions. The display area is changed to Missouri and the lat/lon lines

are plotted at 5 degree intervals.

```
GDATTIM = f
GLEVEL = 700
GVCORD = pres
GFUNC = div(wnd)
GDFILE = $GEMDATA/hrcbob.grd
CINT =.
LINE = 2/1/7/
MAP = 1
TITLE = 1
DEVICE = xw
SATFIL =
RADFIL =
IMCBAR =
PROJ = mer
GAREA = mo
CLEAR = yes
PANEL = 0
TEXT = 1
SCALE = 5
LATLON = 0
HILO =
HLSYM =
CLRBAR = 1
CONTUR = 3
SKIP = 0
FINT =.
FLINE = 23;
CTYPE = f/c
LUTFIL =
STNPLT =
```

4. Clear the screen and draw contours of absolute vorticity. Label vorticity

maxima with a red X and minima with a cyan N. Plot the values of the

extrema. Exclude minima less than 4 * 10 ** -5. The search radius for

finding extrema is 5 grid points. Interpolate the extrema to off-grid point

locations. Plot the values in font number 2 on the right beneath the

marking symbol. Plot the marking label in size 1.5, the values in size 1.

```
GDATTIM = f
GLEVEL = 700
GVCORD = pres
GFUNC = avor(wnd)
GDFILE = $GEMDATA/hrcbob.grd
CINT = 2
LINE = 2/1/
MAP = 1
TITLE = 1
DEVICE = xw
SATFIL =
RADFIL =
IMCBAR =
PROJ = mer
GAREA = ri
CLEAR = yes
PANEL = 0
TEXT = 1
SCALE = 5
LATLON = 0
HILO = 2;6/X#;N#/;4-28/5//yes
HLSYM = 1.5;1/3/1;
CLRBAR = 1
CONTUR = 3
SKIP = 0
FINT = 2
FLINE = 23;
CTYPE = f/c
LUTFIL =
STNPLT =
```

### Error Messages

```
[GDCNTR+1] WARNING. There are no contour levels.

[GDCNTR-1] Fatal error initializing TAE.

[GDCNTR-2] Fatal error reading TAE parameters.

[GDCNTR-3] Fatal error initializing GEMPLT.

[GDCNTR-4] Grid requested is not available.

[GDCNTR-5] Error setting grid navigation for file ....

[GDCNTR-6] There are no grids in grid file.

[GDCNTR-13] There are no times in the grid file
```

## 4.8 GDCROSS

GDCROSS displays a vertical cross section of scalar and/or vector grids.



```
CXSTNS Cross-section station line
GDATTIM Grid date/time
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GVECT Vector grid
GDFILE Grid file
WIND Wind symbol/siz/wdth/typ/hdsz
REFVEC Mag;x;y;txtsiz/font/wdth/HW;labl
PTYPE Plot type/h:w ratio/margins
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
CINT Contour interval/min/max
SCALE Scalar scale / vector scale
LINE Color/type/width/label/smth/fltr
BORDER Background color/type/width
TITLE Title color/line/title
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
PANEL Panel loc/color/dash/width/regn
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CONTUR Subbox/smooth
FINT Fill interval/min/max
FLINE Fill colors/fill types
CTYPE Contour type: C/F
```


GDCROSS draws a vertical cross section between two points in a grid field. The cross-

section path is a line segment on the grid projection plane joining the two points. Grid-

ded data are interpolated to the cross-section plane at intervals corresponding to ap-

proximately one grid increment.

CXSTNS specifies the beginning and ending points of the cross section. Each point

may be entered as a latitude and longitude separated by a semicolon, as station identi-

fiers or numbers or as x and y grid coordinates separated by a semicolon and preceded

by @. The two points are separated by >. The cross section end points may also be

selected graphically by setting CXSTNS with the CURSOR command. The end points

are then selected by clicking on a horizontal map displayed in another GEMPAK XW

window.

The vertical coordinate, set in GVCORD, can be PRES, THTA, HGHT, or SGMA. The

data must be stored in that vertical coordinate in the grid file. No automatic vertical

interpolation is included. The vertical axis scaling, set in PTYPE, can be LIN, LOG,


KAP or STUVE. STUVE and KAP are the same; SKEWT may not be entered. The

plot aspect ratio and margins may also be entered in PTYPE.

Both scalar and vector fields can be displayed in the cross-section plane. Contour lines

are drawn through the scalar field. Contours may be color filled. The line contours and

filled contours are specified as in GDCNTR. CLRBAR allows a color bar to be added

for color fill contours. Vector fields may be depicted using arrows or barbs.

If M is entered in WIND, winds will be displayed in meters per second unless the

KNOTV operator has been specified in GVECT, in which case the winds will be dis-

played in knots. IF K is entered in WIND, the wind is displayed in knots.

Circulations can be displayed in the cross-section plane by specifying GFUNC = CIRC

( V, W ), where V is a vector field and W is the scalar vertical motion in the GVCORD

coordinate. The horizontal component of the circulation is the tangential component of

V. The vertical component is W scaled up to account for the exaggerated aspect ratio

of the display relative to that of the real atmosphere. For the CIRC operator, W is as-

sumed to be pressure velocity in mb/s for PRES and THTA coordinates and cm/s for

the HGHT coordinate. If the vertical component does not require scaling, the circula-

tion can be specified explicitly in the form GVECT = VECR ( TANG ( V ), W )/VERT,

where VERT is a flag indicating that the vector components should not be rotated to be

north-relative.

## Examples

1. Plot temperature in Celsius on a log-P chart along the cross section from

LAX to BWI. Plot the filled contours using every other color from 30 to

8. Plot the contour lines in color number 32, using solid lines. Plot

wind barbs for the wind. Plot a horizontal color bar centered under the

cross section.

```
CXSTNS = lax>bwi
GDATTIM = f06
GVCORD = pres
GFUNC = tmpc
GVECT = wnd
GDFILE = $GEMDATA/hrcbob.grd
WIND = bm6//2
REFVEC = 10
PTYPE = log
YAXIS =
CINT =
SCALE =
LINE = 32/1/3
BORDER = 1
TITLE = 1
CLEAR = yes
DEVICE = xw
```

#TEXT = 1

#PANEL = 0

```
CLRBAR = 1/h/cc/.5;.03/.6;.01
CONTUR = 3
FINT =
FLINE = 30-8-2
CTYPE = c/f
```
2. Now, using the same file, plot a log pressure cross section of the

vorticity advection, scaled by 10**9. Also, plot the ageostrophic

circulation using arrows. Use a cross-section line from grid point (1,1) to

grid point (10,20). Use a height-to-width plot ratio of 0.25.

#CXSTNS = @1;1>@10;20

```
GDATTIM = f06
GVCORD = pres
GFUNC = adv(avor(wnd),wnd)
GVECT = circ(age;omeg)
GDFILE = $GEMDATA/hrcbob.grd
WIND = am6//2//.75
REFVEC = 5;.05;.2
PTYPE = log/.25
YAXIS =
CINT = .5
SCALE = 9
LINE = 32/1/2
BORDER = 1
TITLE = 1
CLEAR = yes
DEVICE = xw
TEXT = 1
PANEL = 0
CLRBAR = 1/h/cc/.5;.2/.1;.01
CONTUR = 3
FINT = 0/0/0
FLINE = 4;2
CTYPE = c/f
```

### Error Messages

```
[GDCROSS+2] Surface value of GVCORD is not available.

[GDCROSS+1] WARNING. There are no contour levels.

[GDCROSS-1] Fatal error initializing TAE.

[GDCROSS-2] Fatal error reading TAE parameters.

[GDCROSS-3] Fatal error initializing GEMPLT.

[GDCROSS-4] Input for CXSTNS is invalid.

[GDCROSS-5] Input for GDATTIM is invalid.

[GDCROSS-6] Input for GVCORD is invalid.

[GDCROSS-7] Input for PTYPE is invalid.

[GDCROSS-8] Graph coordinates are incorrectly defined.

[GDCROSS-9] No points found for cross section.

[GDCROSS-10] LOG is not possible for this vertical coordinate.

[GDCROSS-11] Cross-section coordinates are invalid.

[GDCROSS-12] No levels--check input for GDATTIM and GVCORD.

[GDCROSS-13] @LEVEL in-line parameter is not allowed.

[GDCROSS-14] %VCORD in-line parameter is not allowed.

[GDCROSS-18] GVECT cannot be evaluated.

[GDCROSS-19] GFUNC cannot be evaluated.

[GDCROSS-20] Input for YAXIS is invalid.
```

## 4.9 GDDELT

GDDELT deletes grids from GEMPAK grid files.



```
GDFILE Grid file
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
```


GDDELT deletes grids from GEMPAK grid files.

The user can specify a particular grid by making appropriate inputs for GVCORD,

GLEVEL, GDATTIM, and GFUNC.

GDATTIM allows several different input options. If LIST is entered, a list of dates/

times will be provided after running the program. Upon choosing one of these times, a

data search will be conducted. Entering ALL will allow the program to match data per-

taining to all of the dates/times in the file.

GLEVEL also has several options unique to matching grid headers. Entering ALL will

match data pertaining to all of the levels in the file. If two levels are entered separated

by a dash, this gives all of the levels within that range, including the bounding levels.

Entering MAN will match and copy all of the mandatory pressure levels into the output

file.

GVCORD controls the vertical coordinate search. One vertical coordinate can be en-

tered for the search, or ALL may be entered which will match all of the vertical coor-

dinates in the file.

GFUNC specifies the grid parameter name. If one parameter is entered, the program

searches for that parameter only. If ALL is entered, the program will match all of the

parameters in its search. Entering several parameters separated by semicolons will

match those parameters only.

## Examples

1. Delete the temperature in Kelvin at the 500-millibar level after choosing

from a list of times within NGM.GRD.

```
GDFILE = ngm.grd
GVCORD = pres
GLEVEL = 500
GDATTIM = list
```

```
GFUNC = tmpk
```
2. Delete the relative humidity value at 950 millibars from SFC.GRD.

```
GDFILE = sfc.grd
GVCORD = pres
GLEVEL = 950
GDATTIM = 961031/0000f000
GFUNC = relh
```
3. Delete all of the relative wind values on the sigma coordinate from all

levels from TODAY.GRD.

```
GDFILE = today.grd
GVCORD = sgma
GLEVEL = all
GDATTIM = all
GFUNC = urel;vrel
```
4. Delete the 500-millibar height grid for forecast hour 00 from the file

MRF.GRD.

```
GDFILE = mrf.grd
GVCORD = pres
GLEVEL = 500
GDATTIM = f00
GFUNC = hght
```
### Error Messages

```
[GDDELT-1] Fatal error initializing TAE.

[GDDELT-2] Fatal error reading TAE parameters.

[GDDELT-3] Invalid grid range entered.

[GDDELT-4] Grid number ... is invalid.

[GDDELT-5] There are no grids in file ....

[GDDELT-6] No valid grids in list.

[GDDELT-7] Input grid ... cannot be found.
```

## 4.10 GDDIAG

GDDIAG computes a scalar diagnostic grid and adds it to the grid file.



```
GDFILE Grid file
GDOUTF Output grid file
GFUNC Scalar grid
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GRDNAM Grid parameter name
GPACK Packing type/number of bits
GRDHDR Grid Header Flags
```


GDDIAG computes a diagnostic grid and saves the result in a GEMPAK grid file.

The input grids for the computation must be in GDFILE. The resulting grid is added to

the output file, GDOUTF. The input and output files may be the same.

The name of the output grid is given in GRDNAM. If this value is blank, the name gen-

erated by the grid diagnostic package will be used. The time, level, and vertical coor-

dinate associated with the grid may also be set in

GRDNAM using the inline parameter specifications: ^time,

@level, %coordinate name. These specifications follow the grid name.

GPACK defines grid packing. If GPACK is blank, no packing will be done. Grid pack-

ing details are documented in the GPACK variable documentation.

## Examples

1. Compute the divergence of the gridded wind at 850 mb from the 48-h

forecast for the last model run and save the result in MYFILE.GRD. Use

the default name for the grid. Do not pack the grid.

```
GDFILE = ngm.grd
GDOUTF = myfile.grd
GFUNC = div [wnd]
GDATTIM = f48
GLEVEL = 850
GVCORD = pres
GRDNAM =
GPACK = none
```
2. Compute the magnitude of the gradient of the dewpoint depression at

500 mb for the last time in the grid file and store the result in the input


file. Name the output grid DEWGRAD. Use GRIB packing with 16 bits

per grid point.

```
GDFILE = ngm.grd
GDOUTF = ngm.grd
GFUNC = mag ( grad ( sub ( tmpc, dwpt ) ) )
GDATTIM = last
GLEVEL = 500
GVCORD = pres
GRDNAM = dewgrad
GPACK = grib/16
```
3. Compute the 12-h time change of the layer difference of potential

temperature between 700 and 500 mb. Assign the difference to 600 mb

at a time between the two difference times. Name the grid DTHTDP.

```
GDFILE = ngm.grd
GDOUTF = ngm.grd
GFUNC = sub ( ldf ( thta ), ldf ( thta^F06 ) )
GDATTIM = f18
GLEVEL = 500:700
GVCORD = pres
GRDNAM = dthtdp^f12@600
GPACK = grib/16
```

### Error Messages

```
[GDDIAG-1] Fatal error initializing TAE.

[GDDIAG-2] Fatal error reading TAE parameters.

[GDDIAG-3] Error initializing GEMPLT.

[GDDIAG-4] There is no input file specified.

[GDDIAG-5] Navigation in input and output files differs.

[GDDIAG-6] Error opening input files.

[GDDIAG-7] Error writing output grid.

[GDDIAG-8] Only one output file is permitted.

[GDDIAG-9] Output file open failure.

[GDDIAG-10] Grid files have different navigations.

[GDDIAG-11] Output level is invalid.

[GDDIAG-12] Output time is invalid.

[GDDIAG-13] Output vertical coordinate is invalid.
```

## 4.11 GDEDIT

GDEDIT reads grids from a sequential edit file and adds them to a GEMPAK grid file.



```
GDEFIL Grid edit file
GDFILE Grid file
GPACK Packing type/number of bits
```


GDEDIT reads grids from a sequential text edit file and adds them to a GEMPAK grid

file. The input edit file can be created using a text editor or using the OUTPUT = F

option in GDLIST.

Certain header information must be included before the grid data. Required informa-

tion includes the grid size and the grid identifier. Scaling information may also be in-

cluded.

The grid size must be included on a line:

#COLUMNS: 1 33 ROWS: 1 14

The edit grid must begin at column = 1 and row = 1 and include the entire grid. WARN-

ING: the edit grid size must match the size specified in the grid file. Otherwise, a fatal

error is generated.

The grid identifier is found on a line such as:

#921027/1200F48 850 PRES DIVWND

The first line containing a slash ( / ) is assumed to be a grid identifier. The order of

values is: time 1, time 2 (optional), level 1, level 2 (optional), vertical coordinate, pa-

rameter name.

If the data were scaled before being listed, scaling information must be included.

This is of the form:

Scale factor: 10**5

The scaling information is found by searching for "factor" and "**".

Data follow this header information. The rows are assumed to read from the last (top)

to the first. Each row of data contains:

#ROW 14 0.27...


The word ROW and the row number must be included. The row number is NOT

checked to see that this is the correct row. Data for each row may wrap to the next line.

An example of a grid edit file follows:

Grid file: SMALL.GRD

#GRID: TIME1 TIME2 LEVEL1 LEVEL2 VCORD PARM

#921025/0000 0 PRES TMPC

#AREA: DSET GRID SIZE: 7 6

#COLUMNS: 1 7 ROWS: 1 6

Scale factor : 10**0

#COLUMN: 1 234567

#ROW 6 -12.81 -12.40 -11.98 -11.30 -10.82 -11.18 -12.25

#ROW 5 -9.89 -9.90 -9.41 -8.26 -7.00 -6.43 -6.80

#ROW 4 -2.14 -2.94 -3.66 -3.43 -2.36 -1.29 -0.92

#ROW 3 4.12 4.25 3.23 2.35 2.63 3.51 3.98

#ROW 2 6.39 7.35 7.03 6.29 6.31 6.85 7.14

#ROW 1 7.26 7.95 7.80 7.57 7.87 8.49 8.96

## Examples

1. Add the grids in the edit file GDLIST.FIL to the grid file

25DEC12Z.GRD. Do not pack the data.

```
GDEFIL = gdlist.fil
GDFILE = 25dec12z.grd
GPACK =
```
2. Add the grid in TMPC.FIL to the grid file NMC.GRD. Pack the data

using the GRIB format. Select the number of bits so that precision to

two digits after the decimal place will be retained.

```
GDEFIL = tmpc.fil
GDFILE = nmc.grd
GPACK = +2/dec
```

### Error Messages

```
[GDEDIT-1] Fatal error initializing TAE.

[GDEDIT-2] Fatal error reading TAE parameters.

[GDEDIT-3] Valid grid identifier not found.

[GDEDIT-4] Grid size cannot be determined.

[GDEDIT-5] The grid size in the edit file is incorrect.

[GDEDIT-6] Error reading grid data.

[GDEDIT-7] The edit file ... is invalid.

[GDEDIT-8] The output grid file ... is invalid.
```

## 4.12 GDGRIB

GDGRIB computes a scalar diagnostic grid and adds it to a GRIB file.



```
GDFILE Grid file
GFUNC Scalar grid
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GBTBLS Input GRIB decoding tables
GBFILE GRIB data file name
VERCEN PDS byte_4/byte_5/byte_6/byte_26
PDSVAL GRIB PDS grid identifier overrides
PRECSN Packing precision
WMOHDR WMO_ID/Origin_ID/DDHHMM
CPYFIL Grid file whose navigation is to be used in new grid file |
PROJ Map projection/angles/margins|drop flag
GRDAREA Area covered by grid
KXKY Number of grid points in x;y
```


GDGRIB computes a diagnostic grid and saves the result in a GRIB file.

The input grids for the computation must be in GDFILE. The resulting GRIB message

is added to the output file, GBFILE. If GBFILE does not exist, it will be created.

The parameters PDSVAL and VERCEN allow control over how the grid is identified

in the Product Definition Section (PDS) of the GRIB message. The PRECSN parame-

ter allows the user to specify the precision of the data packing in two different

ways: either in terms of binary precision with rounding to

the nearest power of two or decimal precision in terms of the number of significant dig-

its to preserve.

A World Meteorological Organization (WMO) header may be prefixed to the GRIB

message by giving at least the first six bytes of the header in the WMOHDR input pa-

rameter. Default values will be supplied for the originating center (KWBC) and refer-

ence time.

GDGRIB will interpolate scalar fields horizontally to a different output grid. The nav-

igation for the output grid is specified using CPYFIL (see below) or by setting the

PROJ, GRDAREA, and KXKY parameters. The Grid Description Section (GDS) in

the GRIB message will represent this navigation.

Do not attempt to interpolate grid-relative vector components to another grid because

this requires a rotation of a vector, which cannot be done without both vector compo-

nents and is beyond the capability of the current version of GDGRIB. North- relative


components may be interpolated. Wind direction should NOT be interpolated in any

case. GDS byte 17 is currently set for north-relative (meteorological coordinate) wind

components if the output grid navigation (specifically, the projection type, central lon-

gitude, and true latitudes) is different from that of the input grid. If the output grid nav-

igation is not changed, always specify grid-relative wind components in GFUNC when

writing wind components to GRIB messages.

CPYFIL provides the only means of setting PDS byte number 7 (grid identification

number) to something other than 255, which indicates that the grid is defined in the

GDS. CPYFIL may be set to the name of a GEMPAK grid file (other than the file spec-

ified in GDFILE), in which case the output grid is interpolated to the grid whose navi-

gation is defined in the GEMPAK grid file; however, byte number 7 will be set to 255

and the navigation will be available only from the GDS. If CPYFIL is set to #NNN,

grid number NNN is found in grdnav.tbl, and NNN is less than 255, then PDS byte

number 7 will be set to NNN. Even in this case, a GDS is provided.

The CPYFIL parameter supersedes PROJ, GRDAREA, and KXKY input. If CPYFIL

is valid, PROJ, GRDAREA, and KXKY are ignored. If CPYFIL is invalid, PROJ, GR-

DAREA, and KXKY will be used if they are valid.

GBTBLS allows for specification of the GRIB decoding tables. The defaults are:

$GEMTBL/grid/wmogribX.tbl, $GEMTBL/grid/ncepgribX.tbl, $GEMTBL/grid/

vcrdgrib1.tbl,

where X is replaced by the value of byte 4 of the PDS, which, if not specified in VER-

CEN, defaults to 2.

## Examples

1. Compute the average absolute vorticity of the 250- and 300- mb wind for

the 24-h forecast. Assign this to the 275-mb level in the GRIB message

PDS. Also assign the appropriate parameter number for absolute vorticity

(AVOR). Use decimal precision to pack the data so as to preserve 4

decimal significant digits. Change PDS bytes 6 and 26 to reflect that the

data source is from a high resolution eta model run diagnosed at the

Storm Prediction Center (sub-center number 9). Interpolate the grid to

standard AWIPS grid 212, without using CPYFIL so that PDS byte 7 will

be 255. Do not make a WMO header. The output is written to file

hrcbob.pgrb.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = avg(avor(wind@300),avor(wind@250))
GDATTIM = f24
GLEVEL = 300
GVCORD = pres
```

#GBTBLS =

```
GBFILE = hrcbob.pgrb
VERCEN = //110/9
PDSVAL = AVOR@275
PRECSN = d/4
WMOHDR =
CPYFIL =
PROJ = lcc/25;-95;25
GRDAREA = 12.190;-133.459;57.290;-49.385
KXKY = 185;129
```
2. Transfer the 24-h forecast of the 500-mb temperature grid into a GRIB

file. Use binary precision to pack the data to the nearest 1/8 K. The

output GRIB message is to be added to existing file hrcbob.pgrb. Accept

default entries for the PDS, with the parameter number coming from a

specific lookup table (wmogrib3.tbl) in $GEMTBL. Also, add the WMO

header for 500-mb temperature destined for Family of Services, and

interpolate to grid 212 using CPYFIL so that PDS byte 7 is set to 212.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = tmpk
GDATTIM = f24
GLEVEL = 500
GVCORD = pres
GBTBLS = wmogrib3.tbl
GBFILE = hrcbob.pgrb
VERCEN =
PDSVAL =
PRECSN = b/-3
WMOHDR = HTRE50
CPYFIL = #212
PROJ =
GRDAREA =
KXKY =
```
3. Transfer the tropopause temperature from a 24-h model forecast into a

GRIB file. Reassign the parameter and the vertical coordinate name

properly to treat this as a tropopause temperature. Use the default

parameter lookup table for the GRIB parameter (TMPK) identification

number. Use the vertical coordinate table named vcrdgrib1.tbl in

$GEMTBL to lookup the vertical coordinate identification number. Do

not do horizontal interpolation, but make sure that PDS byte 7 denotes

that this is grid number 6. Since hrcbob.grd data is already on grid #6,

GDGRIB will detect this and not do interpolation.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = tmpktpps
GDATTIM = f24
GLEVEL = 0
GVCORD = none
GBTBLS = ;;vcrdgrib1.tbl
```

```
GBFILE = tmpk_trop.grb
VERCEN =
PDSVAL = tmpk%trop
PRECSN = b/-3
WMOHDR =
CPYFIL = #6
PROJ =
GRDAREA =
KXKY =
```
4. Transfer the 12-hour precipitation at forecast hour 18 to a GRIB file.

Specifically denote the generating process identifier as 84. Interpolate to

1- by 1-degree resolution global grid #3. Note that many points on this

grid will have missing values. Pack the grid to a precision of 1/4

millimeter.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = p12m
GDATTIM = f18
GLEVEL = 0
GVCORD = none
GBTBLS =
GBFILE = p12m.grb
VERCEN = //84
PDSVAL =
PRECSN = b/-2
WMOHDR =
CPYFIL = #3
PROJ =
GRDAREA =
KXKY =
```
5. Compute an "off-time" 6-hour precipitation amount using the average of

two accumulations. Use the PDSVAL in-line time specification given

after ^ to assign the GRIB time properly as a 21-hour forecast. Note the

use of the 4-digit year in the in-line specification in PDSVAL.

Interpolate to high- resolution grid 215 so that PDS byte 7 is set to 215.

Use default packing precision.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = avg(P06M^F24,P06M^F18)
GDATTIM = f18
GLEVEL = 0
GVCORD = none
GBTBLS =
GBFILE = p06m.grb
VERCEN =
PDSVAL = P06M^19910819/0000F21
PRECSN =
WMOHDR =
CPYFIL = #215
PROJ =
```

#GRDAREA =

#KXKY =

6. Transfer the 12-hour forecast of the 850-mb grid-relative U-wind

component to a GRIB file. Pack the data to the nearest 1/8 m/s. DO

NOT INTERPOLATE!

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = urel
GDATTIM = f12
GLEVEL = 850
GVCORD = pres
GBTBLS =
GBFILE = wind1.grb
VERCEN =
PDSVAL =
PRECSN = b/-3
WMOHDR =
CPYFIL =
PROJ =
GRDAREA =
KXKY =
```
7. Complete the transfer of the 850-mb wind by transfering the grid-relative

V-wind component.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = vrel
GDATTIM = f12
GLEVEL = 850
GVCORD = pres
GBTBLS =
GBFILE = wind1.grb
VERCEN =
PDSVAL =
PRECSN = b/-3
WMOHDR =
CPYFIL =
PROJ =
GRDAREA =
KXKY =
```
8. Transfer the 12-hour forecast of the 850-mb north-relative U-wind

component to a GRIB file. Interpolate to grid 212. Make sure that it is

labeled as a u-wind component in the GRIB PDS. Pack the data to the

nearest 1/8 m/s.

```
GDFILE = $GEMDATA/hrcbob.grd
GFUNC = un(wind)
GDATTIM = f12
GLEVEL = 850
GVCORD = pres
```

#GBTBLS =

```
GBFILE = wind2.grb
VERCEN =
PDSVAL = 33
PRECSN = b/-3
WMOHDR =
CPYFIL = #212
PROJ =
GRDAREA =
KXKY =
```
9. Complete the transfer of the 850-mb wind by transfering the north-

relative V-wind component. Note that this wind will not be handled

correctly by NAGRIB because NAGRIB disregards GDS byte 17 and

always assumes grid relative vectors. To display these winds correctly

after running NAGRIB, use the following specification in grid diagnostic

```
programs: VECN(UREL,VREL).

GDFILE = $GEMDATA/hrcbob.grd
GFUNC = vn(wind)
GDATTIM = f12
GLEVEL = 850
GVCORD = pres
GBTBLS =
GBFILE = wind2.grb
VERCEN =
PDSVAL = 34
PRECSN = b/-3
WMOHDR =
CPYFIL = #212
PROJ =
GRDAREA =
KXKY =
```

### Error Messages

```
[GDGRIB+6] WMO header center ID inconsistent with PDS byte 5.

[GDGRIB+5] CPYFIL not used to determine GRIB GDS navigation.

[GDGRIB+4] Warning: grid not found in grdnav.tbl...continuing.

[GDGRIB+3] Warning: decoding error in grdnav.tbl...continuing.

[GDGRIB+2] Warning: error reading grdnav.tbl...continuing.

[GDGRIB+1] Warning: cannot open grdnav.tbl...continuing.

[GDGRIB-1] Fatal error initializing TAE.

[GDGRIB-2] Fatal error reading TAE parameters.

[GDGRIB-3] Error initializing GEMPLT.

[GDGRIB-4] Grid file could not be opened.

[GDGRIB-5] Grid navigation could not be set in GEMPLT.

[GDGRIB-6] Grid diagnostics package initialization failed.

[GDGRIB-7] Error interpolating to output grid.

[GDGRIB-8] GRIB message is too long.

[GDGRIB-9] First 6 characters of WMO header must be given.

[GDGRIB-10] User supplied WMO header is too long.

[GDGRIB-11] Invalid grid navigation set in PROJ.

[GDGRIB-12] Invalid grid area set in GRDAREA or CPYFIL.

[GDGRIB-13] Invalid grid size.

[GDGRIB-14] (i,j) -> lat/lon transformation failed.

[GDGRIB-15] lat/lon -> (i,j) transformation failed.

[GDGRIB-16] Cannot set output grid navigation.

[GDGRIB-17] Cannot set input grid navigation.

[GDGRIB-18] Invalid horizontal interpolation type.

[GDGRIB-19] CPYFIL entry is not valid.

[GDGRIB] BDS errors:
```

[GDGRIB-21] BDS section is too long.

[GDGRIB-22] Number of packing bits is invalid.

[GDGRIB-23] Data range is not valid.

[GDGRIB-24] Binary scaling is invalid.

[GDGRIB-25] Computation of the reference value failed.

[GDGRIB-26] BDS array accomodation is too small.

[GDGRIB-27] The calculation of the # of bits needed failed.

[GDGRIB-28] All data is missing -- no GRIB message made.

[GDGRIB] BMS errors:

[GDGRIB-41] BMS section is too long.

[GDGRIB-42] BMS array allocation is too small.

[GDGRIB] GDS errors:

[GDGRIB-61] Not enough bytes for the GDS.

[GDGRIB-62] Number in i direction is too large.

[GDGRIB-63] Number in j direction is too large.

[GDGRIB-64] Latitude 1 is invalid.

[GDGRIB-65] Longitude 1 is invalid.

[GDGRIB-66] Latitude 2 is invalid.

[GDGRIB-67] Longitude 2 is invalid.

[GDGRIB-68] Rotated CED projection is not supported.

[GDGRIB-69] Rotated STR projection is not supported.

[GDGRIB-70] DX grid increment is invalid.

[GDGRIB-71] DY grid increment is invalid.

[GDGRIB-72] Central longitude is invalid.

[GDGRIB-73] True latitudes are invalid.

[GDGRIB-74] Rotated MER projection is not supported.

[GDGRIB-75] Grid projection is not supported.


[GDGRIB] PDS errors:

[GDGRIB-83] Cannot find parameter in tables.

[GDGRIB-84] Parameter # found is not valid in GRIB.

[GDGRIB-85] Vertical coordinate not found in table.

[GDGRIB-86] Vertical coordinate is not valid in GRIB.

[GDGRIB-87] Level value is too large for GRIB.

[GDGRIB-88] Level is less than zero.

[GDGRIB-89] Dual GEMPAK times not supported.

[GDGRIB-90] 4-digit year required in in-line (^) DATTIM.

[GDGRIB-91] Forecast must be in hours.

[GDGRIB-92] Array allocation for PDS is too small.

[GDGRIB-93] Decimal scale factor is too large.

[GDGRIB-94] Parameter name is too long to be in table.

[GDGRIB-95] GRID # in CPYFIL does not match navigation.

[GDGRIB-96] GRID # in CPYFIL is an invalid PDS entry.


## 4.13 GDGSFC

GDGSFC computes grid data and interpolates to stations in a GEMPAK surface file.



```
GDFILE Grid file
GDATTIM Grid date/time
GVCORD Grid vertical coordinate
GLEVEL Grid level
GFUNC Scalar grid
SCALE Scalar scale / vector scale
SFFILE Surface data file
SFPARM Surface parameter list
```


This program interpolates computed grid data to stations in a GEMPAK surface file.

The program computes the requested grid data from the given grid file. GFUNC can

be any valid GEMPAK grid function. The other grid related parameters are used to

specify the grid to calculate.

The output surface file, SFFILE, must already exist. The output parameter, SFPARM,

must also exist in the surface file. The program reads each successive station from the

surface file and overwrites the existing data with the interpolated data from the grid.

## Examples

1. Read the surface temperature in Celsius from the 12 hour forecast from

the latest ETA model and interpolate the values to the stations in the

surface file new.sfc. The scaling fator is defaulted to 0.

```
GDFILE = eta
GDATTIM = f12
GVCORD = none
GLEVEL = 0
GFUNC = tmpc
SCALE =
SFFILE = new.sfc
SFPARM = tmpc
```
2. Read the 500 mb vorticity from the 24 hour forecast from the latest

GFS model and interpolate the values to the stations in the surface file

vort.sfc with the scaling factor equal to 5.

```
GDFILE = gfs
GDATTIM = f24
GVCORD = pres
```

#GLEVEL = 500

```
GFUNC = vor(wnd)
SCALE = 5
SFFILE = vort.sfc
SFPARM = v500
```
### Error Messages

```
[GDGSFC-1] Fatal error initializing TAE.

[GDGSFC-2] Fatal error reading TAE parameters.

[GDGSFC-3] Fatal error initializing GEMPLT.

[GDGSFC-4] Cannot find SFPARM "..." in this surface file.
```

## 4.14 GDINFO

GDINFO lists information about GEMPAK grid files.



```
GDFILE Grid file
LSTALL Full list flag
OUTPUT Output device/filename
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
```


GDINFO lists information about GEMPAK grid files.

The user can specify a particular grid by making appropriate inputs for GVCORD,

GLEVEL, GDATTIM, and GFUNC.

GDATTIM allows several different input options. If LIST is entered, a list of dates/

times will be provided after running the program. Upon choosing one of these times, a

data search will be conducted. Entering ALL will allow the program to match data per-

taining to all of the dates/times in the file.

GLEVEL also has several options unique to matching grid headers. Entering ALL will

match data pertaining to all of the levels in the file. If two levels are entered separated

by a dash, this gives all of the levels within that range, including the bounding levels.

Entering MAN will match and copy all of the mandatory pressure levels into the output

file.

GVCORD controls the vertical coordinate search. One vertical coordinate can be en-

tered for the search, or ALL may be entered which will match all of the vertical coor-

dinates in the file.

GFUNC specifies the grid parameter name. If one parameter is entered, the program

searches for that parameter only. If ALL is entered, the program will match all of the

parameters in its search. Entering several parameters separated by semicolons will

match those parameters only.

The navigation information and grid analysis information will be listed. If requested,

the grids in the file will also be listed. If LSTALL = YES, all the grids in the file will

be listed. If LSTALL = NO, only the navigation and analysis information will be dis-

played.


## Examples

1. List all of the latest NGM Model information to the screen for the 500-

millibar temperature in Kelvin from the 00 time period.

```
GDFILE = ngm
LSTALL = YES
OUTPUT = t
GDATTIM = f00
GLEVEL = 500
GFUNC = TMPK
GVCORD = PRES
```
### Error Messages

```
[GDINFO-1] Fatal error initializing TAE.

[GDINFO-2] Fatal error reading TAE parameters.
```

## 4.15 GDLIST

GDLIST lists data from a scalar grid.



```
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GDFILE Grid file
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SCALE Scalar scale / vector scale
OUTPUT Output device/filename
```


GDLIST lists a diagnostic grid computed from the grids in a GEMPAK grid file.

The data may be listed over a subgrid. The variables GAREA and PROJ specify the

data subset area. This area is only approximate; the subgrid selected will cover the sub-

set area. GAREA must be specified as a geographic area, as an area centered on a sta-

tion, as latitude/longitude bounds or as DSET or GRID. If DSET or GRID is chosen,

the entire grid will be printed.

Output to the terminal or to a file will be 80 columns wide. If the output is sent to a file,

the file will be named gdlist.fil if no name is specified.

## Examples

1. List the 850 mb dewpoint temperature for the 24 hour forecast time. The

data subset area is a zoomed area centered on New York.

```
GDATTIM = /f24
GLEVEL = 850
GVCORD = PRES
GFUNC = dwpc
GDFILE = $GEMDATA/HRCBOB.GRD
GAREA = ny*
PROJ = lcc
SCALE = 999
OUTPUT = T
```
2. Compute the divergence at 300 mb and list the output over the Eastern

US. Write the output to a file called div.east.

```
GDATTIM = /f24
GLEVEL = 300
```

#GVCORD = PRES

```
GFUNC = div(wnd)
GDFILE = $GEMDATA/HRCBOB.GRD
GAREA = east
PROJ = lcc
SCALE = 999
OUTPUT = f/div.east
```
### Error Messages

```
[GDLIST-1] Fatal error initializing TAE.

[GDLIST-2] Fatal error reading TAE parameters.
```

## 4.16 GDMAP

GDMAP plots data from a scalar grid.



```
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GDFILE Grid file
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
SKIP Skip_cntr/skip_plt_x;skip_plt_y
POSN Position
COLORS Color list
MARKER Marker color/type/size/width/hw
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
CINT Contour interval/min/max
TITLE Title color/line/title
SCALE Scalar scale / vector scale
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
CLEAR Clear screen flag
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
GRDLBL Grid point label color
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
```


GDMAP plots data computed from GEMPAK grid files on a map. Data may be plotted

in any valid GEMPAK projection and may be overlaid on images.

A list of times may be given in GDATTIM allowing animation of the data plots.

The variable, POSN, is used to select the position for the data relative to the grid point

using the following position numbers:

#7

#13

#204

#56

#8


Position 0 will plot data centered at the station. If an invalid position number is entered,

position 0 will be used.

The variable, SCALE, is used to scale the data by 10 ** SCALE. The data will be

scaled and then rounded to the nearest integer before it is plotted.

SKIP specifies the number of grid points to skip when displaying the data. For exam-

ple, SKIP = 1 displays data at every other grid point in each coordinate direction. SKIP

= 0 displays all points.

If GRDLBL is not 0, the grid point row and column numbers will be displayed using

the color number specified. The grid rows and columns will be centered on the plot ar-

ea.

A map and title may also be included.

## Examples

1. Plot the dewpoint depression at 850 mb in color 3 at position 0. The

data will be plotted at every point for an area centered on Nebraska. The

row and column labels will be plotted in color 17. The title will be

written in color 1.

```
GDATTIM = f18
GLEVEL = 850
GVCORD = pres
GFUNC = sub(tmpc,dwpc)
GDFILE = $GEMDATA/hrcbob.grd
GAREA = ne
SATFIL =
RADFIL =
IMCBAR =
SKIP = 0
POSN = 0
COLORS = 3
MARKER = 0
MAP = 1/7
LATLON = 2/10/1/1/5;5
PANEL = 0
CINT =
TITLE = 1/-3/~ @ DEWPOINT DEPRESSION
SCALE = 0
DEVICE = xw
PROJ = nps
CLEAR = yes
TEXT = 1
GRDLBL = 17
LUTFIL =
STNPLT =
```

2. Do not clear the screen and replot the same function in color 7, for the

same area. The minimum value to plot is set to 10 in CINT. The text

of the title is changed to reflect the colored values.

```
GDATTIM = f18
GLEVEL = 850
GVCORD = pres
GFUNC = sub(tmpc,dwpc)
GDFILE = $GEMDATA/hrcbob.grd
GAREA = ne
SATFIL =
RADFIL =
IMCBAR =
SKIP = 0
POSN = 0
COLORS = 7
MARKER = 0
MAP = 1/7
LATLON = 2/10/1/1/5;5
PANEL = 0
CINT = /10
TITLE = 1/-2/(MAGENTA >10; GREEN <10)
SCALE = 0
DEVICE = xw
PROJ = nps
CLEAR = no
TEXT = 1
GRDLBL = 17
LUTFIL =
STNPLT =
```
### Error Messages

```
[GDMAP-1] Fatal error initializing TAE.

[GDMAP-2] Fatal error reading TAE parameters.

[GDMAP-3] Fatal error initializing GEMPLT.
```

## 4.17 GDMOD

GDMOD moves grids from one GEMPAK grid file to another.



```
GDFILE Grid file
GDOUTF Output grid file
GDATTIM Grid date/time
GLEVEL Grid level
GFUNC Scalar grid
GVCORD Grid vertical coordinate
GPACK Packing type/number of bits
GRDHDR Grid Header Flags
```


GDMOD moves grids from an input grid file to an output file. The input grid file name

is specified in GDFILE. The output grid file name is specified in GDOUTF. The input

and output grid files must have the same navigation information.

GDATTIM allows several different input options. If LIST is entered, a list of dates/

times will be provided after running the program. Upon choosing one of these times, a

data search will be conducted. Entering ALL will allow the program to match data per-

taining to all of the dates/times in the file.

GLEVEL also has several options unique to matching grid headers. Entering ALL will

match data pertaining to all of the levels in the file. If two levels are entered separated

by a dash, this gives all of the levels within that range, including the bounding levels.

Entering MAN will match and copy all of the mandatory pressure levels into the output

file.

GVCORD controls the vertical coordinate search. One vertical coordinate can be en-

tered for the search, or ALL may be entered which will match all of the vertical coor-

dinates in the file.

GFUNC specifies the grid parameter name. If one parameter is entered, the program

searches for that parameter only. If ALL is entered, the program will match all of the

parameters in its search. Entering several parameters separated by semicolons will

match those parameters only.

The output grids will be packed using the information in GPACK.

## Examples

1. Adds grids for lifted index value for the layer between 1000 millibars and

500 millibars from model file $MODEL/ngm/ngm_96100200, to file


NEW.GRD. Only grids at the analysis time are added. Pack the data

using 16 bits for each data value.

```
GDFILE = $MODEL/ngm/ngm_96100200
GDOUTF = new.grd
GDATTIM = 961002/0000F000
GLEVEL = 500:1000
GFUNC = lift
GVCORD = pres
GPACK = 16/grib
```
2. Copy grids with the temperature in Kelvin for all of the pressure levels

from TODAY.GRD to DEC.GRD. Do not pack any data.

```
GDFILE = today.grd
GDOUTF = dec.grd
GDATTIM = 961002/0000F000
GLEVEL = all
GFUNC = tmpk
GVCORD = pres
GPACK =
```
3. For all of the times in the grid file, copy grids with the u- and v-

components of the wind using sigma coordinates matching the 8967 level

from OLD.GRD to NEW.GRD. Do not pack any data.

```
GDFILE = old.grd
GDOUTF = new.grd
GDATTIM = all
GLEVEL = 8967
GFUNC = urel;vrel
GVCORD = sgma
GPACK =
```
4. Add grids from the latest GFS model file to OUT.GRD from 1000 to

500 millibars and the 250-millibar levels matching all possible parameters.

A list of grid times is displayed for user selection. Do not pack any

data.

```
GDFILE = gfs
GDOUTF = out.grd
GDATTIM = list
GLEVEL = 1000-500;250
GFUNC = all
GVCORD = pres
GPACK =
```

### Error Messages

```
[GDMOD-1] Fatal error initializing TAE.

[GDMOD-2] Fatal error reading TAE parameters.

[GDMOD-3] Invalid grid range entered.

[GDMOD-4] Grid number ... is invalid.

[GDMOD-5] There are no grids in file ....

[GDMOD-6] No valid grids in list.

[GDMOD-7] Error reading input grid.

[GDMOD-8] Error writing output grid.

[GDMOD-9] Error opening files.

[GDMOD-10] Files contain different navigations.

[GDMOD-11] Too many grids to transfer.

[GDMOD-12] Cannot parse user input.

[GDMOD-13] Cannot match user input.
```

## 4.18 GDOBSCNT

GDOBSCNT creates a gridded sampling of the number of surface observations within

a specified radius of each grid point.



```
SFFILE Surface data file
DATTIM Date/time
GDFILE Grid file
RADIUS Radius (in meters) to search
```


GDOBSCNT produces a grid for each time found in a surface file.

SFFILE specifies the input surface file. Each station in the surface file will be checked

to see if it is within the specified radius of every grid point in the output grid file.

GDFILE specifies the output grid file. It must already exist.

RADIUS is the distance in meters for which a surface station will contribute to the ob-

served density at a grid point.

## Examples

1. Create a grid for each time within the NLDN lightning file showing the

number of strikes within 50km of each grid point. The grid file (obs.grd)

has already been created with GDCFIL.

```
SFFILE = nldn
DATTIM = all
GDFILE = obs.grd
RADIUS = 50000
```
### Error Messages

```
[GDOBSCNT0] Using: ...
```

## 4.19 GDOMEG

GDOMEG computes grids of vertical motion and adds them to the grid file.



```
GDFILE Grid file
GDATTIM Grid date/time
GPACK Packing type/number of bits
```


GDOMEG computes kinematic vertical motions from gridded wind data by vertically

integrating the continuity equation in pressure coordinates. An O'Brien correction is

applied to assure zero vertical motion at the top pressure level.

The O'Brien correction is based on the solution of a variational problem which is for-

mulated to minimize the squared difference between the observed divergence and the

adjusted divergence while simultaneously satisfying the isobaric continuity equation

with zero vertical motion at the top. The error in the divergence is assumed to increase

linearly with decreasing pressure. The effect of the correction is to adjust the OMEG

values at every level by a fraction of the excess OMEG at the top pressure level. The

fraction increases from nearly zero at the bottom to one at the top.

The boundary condition at the surface is:

#OMEG=-G*RHO*DOT(V,GRAD (Z) )

where, G is the acceleration of gravity, RHO is density, V is either the surface wind or

an estimated surface wind and Z is the terrain elevation. If there is insufficient data, the

surface OMEG is set to zero.

The values of omega are computed on the existing pressure levels. A weighted average

in ln p of divergence in the layer is used in the vertical integration.

The computed grids are in mb/s and are named OMEG.

## Examples

1. Create vertical motion grids in file obs.grd at the last time using GRIB

packing.

```
GDFILE = obs.grd
GDATTIM = last
GPACK = grib/16
```
2. Create vertical motion grids in file model.grd at the 12-h forecast time.


```
GDFILE = model.grd
GDATTIM = f12
GPACK = grib/16
```
### Error Messages

```
[GDOMEG+2] No correction pass will be done.

[GDOMEG+1] WARNING--no surface pressure grid exists.

[GDOMEG-1] Fatal error initializing TAE.

[GDOMEG-2] Fatal error reading TAE parameters.

[GDOMEG-3] Error initializing GEMPLT.

[GDOMEG-4] Requested time is not available.

[GDOMEG-5] No pressure levels exist.

[GDOMEG-6] The packing information is erroneous.

[GDOMEG-7] Could not write output grid to the file.
```

## 4.20 GDPLOT

GDPLOT draws contour lines through scalar grids and/or wind barbs or arrows through

vector grids. Multiple sets of contours and vectors can be generated for each frame.



```
GDFILE Grid file
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
PANEL Panel loc/color/dash/width/regn
SKIP Skip_cntr/skip_plt_x;skip_plt_y
SCALE Scalar scale / vector scale
GFUNC Scalar grid
CTYPE Contour type: C/F
CONTUR Subbox/smooth
CINT Contour interval/min/max
LINE Color/type/width/label/smth/fltr
FINT Fill interval/min/max
FLINE Fill colors/fill types
HILO Color/symbol/rng/rad/cnt/intp
HLSYM HILO txt size/posn/font/wdth/hw
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
GVECT Vector grid
WIND Wind symbol/siz/wdth/typ/hdsz
REFVEC Mag;x;y;txtsiz/font/wdth/HW;labl
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
PROJ Map projection/angles/margins|drop flag
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
DEVICE Device|name|x size;y size|color type
STNPLT Txtc/txt attr|marker attr|stnfil#col
```


GDPLOT draws contours through scalar grids and wind barbs or arrows at grid points

for vector grids. Plots are generated for any field computed using the GEMPAK grid

diagnostic functions. The program can generate multiple sets of contours and vector

plots for each frame. It can also generate plots for multiple times.

A list of times may be given in GDATTIM allowing animation.

Exclamation points are used in GFUNC and GVECT to delimit multiple sets of con-

tours and vectors, respectively. GLEVEL, GVCORD, PANEL, SKIP, SCALE,

GFUNC, CTYPE, CONTUR, CINT, TITLE, LINE, FINT, FLINE, GVECT, WIND,

REFVEC, HILO, and HLSYM may contain exclamation points to delimit specifica-


tions for the fields defined by GFUNC and GVECT. If any parameter contains more

specifications than the number of plots specified in GFUNC or GVECT, they will be

ignored. Positions between exclamation points may be left blank. A trailing exclama-

tion point will be treated as a blank. If there is no trailing exclamation point, the last

specification will be repeated for subsequent plots.

Contours may be displayed as lines or as a color fill. If CTYPE is C, contour lines are

drawn using input from CINT and LINE. If CTYPE is F, filled contours are drawn us-

ing specifications from FINT and FLINE. Both contour lines and filled contours are

drawn if CTYPE is F/C.

The attributes of the contour lines, including the color, line type, line width, and label

frequency are specified in LINE. The four attributes must be separated with slashes;

semicolons separate the values for each attribute. If the line type is set to a single neg-

ative number, negative contour values will have the absolute value of the line type and

positive values will be solid. If the label type is set to a single number, n, then every

nth value will be labeled.

The contour fill intervals are specified in FINT; the attributes for the fill are specified

in FLINE. The first color specified in FLINE fills values less than the first level; while

the last color fills values greater than the last level. Therefore, n levels require n+1 col-

ors.

A range of colors may be specified in either FLINE or LINE by specifying starting, end-

ing and increment values in that order separated by dashes. If the increment is missing,

a default of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3 (slanted line). If fill type is

set to 0, solid fill is used. If the fill type is set to a single negative number, negative

values will use the absolute value of the fill type, and positive values will be solid.

If M is entered in the wind symbol specification of WIND, winds will be displayed in

m/s, unless the KNOTV operator has been specified in GVECT, in which case the

winds will be displayed in knots. If K is entered in WIND, the wind is displayed in

knots.

For contours, SKIP specifies the number of grid points to skip in generating contours.

For example, if SKIP = 1, every other point is used to generate the contours.

For vectors, SKIP specifies the number of points to skip in both coordinate directions.

For example, SKIP = /1;2 will display winds at every other grid point in the x direction

and every third grid in the y direction. SKIP = /0 displays winds at every grid point.

Wind barbs or arrows can also be staggered by specifying negative values for SKIP.

The HILO and HLSYM parameters control marking and labeling the values of relative

maxima and minima. CLRBAR allows a color bar to be added for color fill contours.


## Examples

1. Plot the 700 mb temperature, dewpoint and wind barbs. Plot the

temperature in color 3 and the dewpoint in color 17. For both sets of

contours, the lines of negative value are drawn in pattern 3 and the those

of positive value are solid. Every other contour is labeled. The wind

barbs are plotted in m/s in color 6. The display area is an enlarged area

centered on New York.

```
GDFILE = $GEMDATA/hrcbob.grd
GDATTIM = f24
GLEVEL = 700
GVCORD = pres
PANEL = 0
SKIP = 0
SCALE = 0
GFUNC = tmpc! dwpc
CTYPE = C
CONTUR = 3
CINT = 2! 5
LINE = 3/-3/3/2! 17/-3/2/2
FINT = 0
FLINE = 10-20
HILO =
HLSYM =
CLRBAR =
GVECT = wnd
WIND = bm6//2
REFVEC = 10
TITLE = 1/-2/~ @ TEMPERATURE, DEWPOINT AND WIND! 0
TEXT = 1/22//hw
CLEAR = yes
GAREA = ny-
PROJ = mer
MAP = 1/7
LATLON = 2/10/1/1/5;5
DEVICE = xw
STNPLT =
```
2. Draw fills and contours of the magnitude of the wind (knots), contours

of height and wind barbs (knots) at 250 mb. The contour and fill

intervals are specified in CINT and FINT. The display area is the entire

grid on a stereographic map.

```
GDFILE = $GEMDATA/hrcbob.grd
GDATTIM = f24
GLEVEL = 250
GVCORD = pres
PANEL = 0
SKIP = 0/1
```

#SCALE = 0! -1

```
GFUNC = knts(mag(wnd))! hght
CTYPE = c/f! c
CONTUR = 1
CINT = 30;50;70;90;110;130;150! 12
LINE = 27/5/2/1! 20/1/2/1
FINT = 70;90;110;130;150
FLINE = 0;25;24;29;7;15
HILO =
HLSYM =
CLRBAR = 1
GVECT = kntv(wnd)
WIND = bk18//1
REFVEC =
TITLE = 5/-2/~ @ HEIGHTS, ISOTACHS AND WIND (KTS)! 0
TEXT = 1/21//hw
CLEAR = yes
GAREA = grid
PROJ = str/90;-105;0
MAP = 1
LATLON = 0
DEVICE = XW
STNPLT =
```
### Error Messages

```
[GDPLOT+2] The requested scalar/vector cannot be computed.

[GDPLOT+1] WARNING. There are no contour levels.

[GDPLOT-1] Fatal error initializing TAE.

[GDPLOT-2] Fatal error reading TAE parameters.

[GDPLOT-3] Fatal error initializing GEMPLT.

[GDPLOT-4] Grid requested is not available.

[GDPLOT-5] Error setting grid navigation for file ....

[GDPLOT-6] There are no grids in grid file.
```

## 4.21 GDPLOT2

GDPLOT2 draws contour lines through scalar grids and/or wind barbs or arrows or

streamlines through vector grids. Multiple sets of contours, vectors and/or streamlines

can be generated for each frame.



```
GDFILE Grid file
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
PANEL Panel loc/color/dash/width/regn
SKIP Skip_cntr/skip_plt_x;skip_plt_y
SCALE Scalar scale / vector scale
GDPFUN Scalar grid or vector grid function
TYPE GDPLOT2 function processing type
CONTUR Subbox/smooth
CINT Contour interval/min/max
LINE Color/type/width/label/smth/fltr
FINT Fill interval/min/max
FLINE Fill colors/fill types
HILO Color/symbol/rng/rad/cnt/intp
HLSYM HILO txt size/posn/font/wdth/hw
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
WIND Wind symbol/siz/wdth/typ/hdsz
REFVEC Mag;x;y;txtsiz/font/wdth/HW;labl
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
PROJ Map projection/angles/margins|drop flag
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
DEVICE Device|name|x size;y size|color type
STNPLT Txtc/txt attr|marker attr|stnfil#col
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
LUTFIL Enhancement lookup table filename
STREAM lines/arrows/stop/slow/scale
POSN Position
COLORS Color list
MARKER Marker color/type/size/width/hw
GRDLBL Grid point label color
FILTER Filter data factor
```


GDPLOT2 draws contours through scalar grids and streamlines wind barbs or arrows

at grid points for vector grids. Plots are generated for any field computed using the


GEMPAK grid diagnostic functions. The program can generate multiple sets of scalar

and vector plots for each frame.

GDPLOT2 can also generate plots for multiple times. The list of times may be given

in GDATTIM allowing animation.

Exclamation points are used in GDPFUN to delimit multiple overlays of scalar and vec-

tor fields, respectively. All other parameters (except DEVICE, CLEAR and LUTFIL)

may contain exclamation points to delimit specifications for the fields defined by GD-

PFUN. If any parameter contains more specifications than the maximum number of

plots specified in GDPFUN or GDFILE, they will be ignored. Positions between ex-

clamation points may be left blank. Most parameters will replace the blank with the

previous value (repeat). A trailing exclamation point will be treated as a blank. If there

is no trailing exclamation point, the last specification will be repeated for subsequent

plots.

SCALAR QUANTITIES

Scalar contours may be displayed as lines or as a color fill. If TYPE is C, contour lines

are drawn using input from CINT and LINE. If TYPE is F, filled contours are drawn

using specifications from FINT and FLINE. Both contour lines and filled contours are

drawn if TYPE is F/C or C/F.

Scalar grid point values may also be plotted by specifying TYPE to be P. If TYPE M

is also specified, then markers will be plotted at the grid point locations according to

the MARKER parameter. The value's location relative to the actual grid point location

is set by the POSN parameter.

All scalar types ( C, F and P) may be requested at one time, e.g., TYPE=C/F/P. The

order is irrelevant, however, contour fills (F) will be done first followed by contour

lines (C), the map background, grid point indices (M), markers (G) and values (P).

The attributes of the contour lines, including the color, line type, line width, and label

frequency are specified in LINE. The four attributes must be separated with slashes;

semicolons separate the values for each attribute. If the line type is set to a single neg-

ative number, negative contour values will have the absolute value of the line type and

positive values will be solid. If the label type is set to a single number, n, then every

nth value will be labeled.

The contour fill intervals are specified in FINT; the attributes for the fill are specified

in FLINE. The first color specified in FLINE fills values less than the first level; while

the last color fills values greater than the last level. Therefore, n levels require n+1 col-

ors.


A range of colors may be specified in either FLINE or LINE by specifying starting, end-

ing and increment values in that order separated by dashes. If the increment is missing,

a default of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3 (slanted line). If fill type is

set to 0, solid fill is used. If the fill type is set to a single negative number, negative

values will use the absolute value of the fill type, and positive values will be solid.

For contours, SKIP specifies the number of grid points to skip in generating contours.

For example, if SKIP = 1, every other point is used to generate the contours.

The HILO and HLSYM parameters control marking and labeling the values of relative

maxima and minima. CLRBAR allows a color bar to be added for color fill contours.

IMCBAR allows a color bar to be added for images.

VECTOR QUANTITIES

Vector grids may be displayed as either a field of wind barbs or arrows, or as a stream-

line display. If TYPE is A, wind arrows will be displayed; if TYPE is B, wind barbs

will be displayed. If both A and B are specified, B will prevail. If TYPE is N and nei-

ther A nor B is also indicated, then non-unit wind barbs will be assumed. If TYPE is

N in combination with either A or B, then non-unit arrows or barbs, respectively, will

be plotted. If TYPE is S, streamlines will be drawn.

As with scalar functions, the order of the TYPE values is irrelevant. The order of plot-

ting is the map background, grid point indices (G), markers (M), vector arrows/barbs

(A or B) and streamlines (S).

Winds will be displayed in m/s, unless the KNTV operator has been specified in GDP-

FUN, in which case the winds will be displayed in knots.

For vectors, SKIP specifies the number of points to skip in both coordinate directions.

For example, SKIP = /1;2 will display winds at every other grid point in the x direction

and every third grid in the y direction. SKIP = /0 displays winds at every grid point.

Wind barbs or arrows can also be staggered by specifying negative values for SKIP.

The FILTER parameter filters gridded wind arrows/barbs in the same way as it filters

station models in the program SFMAP. If FILTER=yes, the default filter value is set

to 1.0. If FILTER=no, then the SKIP parameter is used to filter the winds. Otherwise

FILTER is expected to be a numeric value greater than zero which controls the density

of plotted winds.

MISCELLANEOUS


Grid point markers may be plotted by setting TYPE=M along with either a scalar TYPE

or vector TYPE and valid GDPFUN. Marker attributes will be determined via the

MARKER parameter.

Grid index values ( row and column numbers ) may be plotted by setting TYPE=G

along with either a scalar TYPE or vector TYPE and valid GDPFUN. Attributes will

be determined via the GRDLBL parameter.

Station locations and identifiers may be plotted using the STNPLT parameter.

## Examples

1. Plot the 700 mb temperature, dewpoint and wind barbs. Plot the

temperature in color 3 and the dewpoint in color 17. For both sets of

contours, the lines of negative value are drawn in pattern 3 and the those

of positive value are solid. Every other contour is labeled. The wind

barbs are plotted in m/s in color 6 and are filtered according to the

FILTER value. The display area is an enlarged area centered on New

York.

```
GDFILE = $GEMDATA/hrcbob.grd
GDATTIM = f24
GLEVEL = 700
GVCORD = pres
PANEL = 0
SKIP = 0
SCALE = 0
GDPFUN = tmpc! dwpc! wnd
TYPE = C!! B
CONTUR = 3
CINT = 2! 5
LINE = 3/-3/3/2! 17/-3/2/2
FINT = 0
FLINE = 10-20
HILO =
HLSYM =
CLRBAR =
WIND = bm6//2
REFVEC = 10
TITLE = 1/-2/~ @ TEMPERATURE, DEWPOINT AND WIND! 0
TEXT = 1/22//hw
CLEAR = yes
GAREA = ny-
PROJ = mer
MAP = 1/7
LATLON = 2/10/1/1/5;5
DEVICE = xw
STNPLT =
SATFIL =
RADFIL =
```

#IMCBAR =

#FILTER = 1.2

2. Draw fills and contours of the magnitude of the wind (knots), contours

of height and wind barbs (knots) at 250 mb and plots the value of the

wind magnitude at the grid points. Winds are filtered according to the

FILTER value. The contour and fill intervals are specified in CINT and

FINT. The display area is the entire grid on a stereographic map.

```
GDFILE = $GEMDATA/hrcbob.grd
GDATTIM = f24
GLEVEL = 250
GVCORD = pres
PANEL = 0
SKIP = 0/1
SCALE = 0! -1
GDPFUN = knts(mag(wnd))! hght! kntv(wnd)
TYPE = c/f/p! c! b
CONTUR = 1
CINT = 30;50;70;90;110;130;150! 12
LINE = 27/5/2/1! 20/1/2/1
FINT = 70;90;110;130;150
FLINE = 0;25;24;29;7;15
HILO =
HLSYM =
CLRBAR = 1
WIND = bk18//1
REFVEC =
TITLE = 5/-2/~ @ HEIGHTS, ISOTACHS AND WIND (KTS)! 0
TEXT = 1/21//hw
IMCBAR =
CLEAR = yes
GAREA = grid
PROJ = str/90;-105;0
MAP = 1
LATLON = 0
DEVICE = XW
STNPLT =
SATFIL =
RADFIL =
IMCBAR =
FILTER = yes
```

### Error Messages

```
[GDPLOT2+2] The requested scalar/vector cannot be computed.

[GDPLOT2+1] WARNING. There are no contour levels.

[GDPLOT2-1] Fatal error initializing TAE.

[GDPLOT2-2] Fatal error reading TAE parameters.

[GDPLOT2-3] Fatal error initializing GEMPLT.

[GDPLOT2-4] Grid requested is not available.

[GDPLOT2-5] Error setting grid navigation for file ....

[GDPLOT2-6] There are no grids in grid file.
```

## 4.22 GDPROF

GDPROF draws profiles of a scalar grid and/or a vector grid.



```
GPOINT Grid point
GDATTIM Grid date/time
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GVECT Vector grid
GDFILE Grid file
LINE Color/type/width/label/smth/fltr
MARKER Marker color/type/size/width/hw
BORDER Background color/type/width
PTYPE Plot type/h:w ratio/margins
SCALE Scalar scale / vector scale
XAXIS Xstrt/xstop/xinc/lbl;gln;tck
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
WIND Wind symbol/siz/wdth/typ/hdsz
REFVEC Mag;x;y;txtsiz/font/wdth/HW;labl
WINPOS Wind position
FILTER Filter data factor
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
CLEAR Clear screen flag
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
DEVICE Device|name|x size;y size|color type
OUTPUT Output device/filename
THTALN THTA color/dash/width/mn/mx/inc
THTELN THTE color/dash/width/mn/mx/inc
MIXRLN MIXR color/dash/width/mn/mx/inc
```


GDPROF draws vertical profiles at a point. Data from each level in the grid file are

interpolated to the point.

GPOINT specifies the location for the profile. It may be entered as a latitude and lon-

gitude separated with a semicolon, as a station character identifier or station number, or

as a set of x and y grid points separated with a semicolon and preceded with an @. The

profile location may also be selected graphically with the CURSOR command. The lo-

cation of the profile is then selected by clicking on a horizontal map in another GEM-

PAK XW window.

Profiles are plotted in an animation sequence by specifying more than one time in

GDATTIM.

The vertical coordinate, set in GVCORD, can be PRES, THTA, or HGHT. The vertical

axis type, set in PTYPE, can be LIN, LOG, KAP, or SKEW. Only temperatures vs.


pressure may be plotted on a skew t plot. The height-to-width ratio of the plot may be

specified in PTYPE following a /. If no value is entered, a value of 0 will be used.

GFUNC is computed at every level in the data set. No errors are returned if the function

cannot be computed at any level, except at the surface. If data at the surface cannot be

computed, a warning message is written and plotting continues. GVECT specifies a

vector to be plotted in the right margin.

If GFUNC is a temperature or dewpoint temperature, background lines of potential

temperature, equivalent potential temperature and mixing ratio can be displayed by

specifying THTALN, THTELN, and MIXRLN. If these background lines are request-

ed and GFUNC is not a temperature, an error message is generated.

GVECT specifies a vector grid to be displayed as arrows or barbs in one of three display

columns on the right of the profile plot. For a wind vector field, the default is north

relative direction. A "/G" indicates grid relative direction. The number of the column

is specified in WINPOS.

If M is entered in WIND, winds will be displayed in m/s unless the KNOTV operator

has been specified in GVECT, in which case the winds will be displayed in knots. If K

is entered in WIND, the wind is displayed in knots.

## Examples

1. Plot temperature in Celsius on a log P chart at IAD for the latest time in

the file. Label the Y axis from 1000 mb to 100 mb in increments of

100 mb. Label the X axis from -40 degrees to +40 degrees in

increments of 10 degrees. Add all the background lines. Plot wind barbs

in the right margin.

```
GPOINT = iad
GDATTIM = last
GVCORD = pres
GFUNC = tmpc
GVECT = wnd
GDFILE = $GEMDATA/hrcbob.grd
LINE = 2//3
MARKER = 5//1.25
BORDER = 1
PTYPE = skewt
SCALE = 0
XAXIS = -40/40/10
YAXIS = 1000/100/100/;1
WIND = bm6//2
REFVEC = 10
WINPOS = 1
FILTER = no
TITLE = 1/3
```

#PANEL = 0

```
CLEAR = yes
TEXT = 1
DEVICE = xw
OUTPUT = n
THTALN = 8/3/1
THTELN = 23/1/1
MIXRLN = 16/10/2
```
2. Overlay the dewpoint for IAD. Do not redraw the background lines.

Plot the wind in grid relative direction.

```
GPOINT = iad
GDATTIM = last
GVCORD = pres
GFUNC = dwpc
GVECT = wnd/G
GDFILE = $GEMDATA/hrcbob.grd
LINE = 3//3
MARKER = 5//1.25
BORDER = 1
PTYPE = skewt
SCALE = 0
XAXIS = -40/40/10
YAXIS = 1000/100/100/;1
WIND = 0
REFVEC = 10
WINPOS = 1
FILTER = no
TITLE = 0
PANEL = 0
CLEAR = no
TEXT = 1
DEVICE = xw
OUTPUT = n
THTALN = 0
THTELN = 0
MIXRLN = 0
```
3. Clear the screen and plot the vorticity at BWI in color number 2 using a

dashed line. Do not plot any winds. Have the graph plot in a second

window named "profile2".

```
GPOINT = bwi
GDATTIM = last
GVCORD = pres
GFUNC = vor(wnd)
GVECT =
GDFILE = $GEMDATA/hrcbob.grd
LINE = 2/2/3
MARKER = 0
BORDER = 1
PTYPE = log
```

#SCALE = 5

#XAXIS =

#YAXIS = 1000/300/100

#WIND =

#REFVEC = 10

#WINPOS = 1

```
FILTER = no
TITLE = 1/3
PANEL = 0
CLEAR = yes
TEXT = 1
DEVICE = xw|profile2
OUTPUT = t
THTALN = 0
THTELN = 0
MIXRLN = 0
```
### Error Messages

```
[GDPROF+1] Invalid parameter for background lines.

[GDPROF-1] Fatal error initializing TAE.

[GDPROF-2] Fatal error reading TAE parameters.

[GDPROF-3] Fatal error initializing GEMPLT.

[GDPROF-4] Input for GPOINT is invalid.

[GDPROF-5] Input for GDATTIM is invalid.

[GDPROF-6] Input for GVCORD is invalid.

[GDPROF-7] Input for PTYPE is invalid.

[GDPROF-8] Error defining graph coordinates.

[GDPROF-9] No points found for profile.

[GDPROF-10] There are no levels at this time.

[GDPROF-11] Input ... for YAXIS is invalid.

[GDPROF-12] Input ... for XAXIS is invalid.
```

## 4.23 GDRADR

GDRADR creates a gridded composite of NEXRAD level III products.



```
GRDAREA Area covered by grid
PROJ Map projection/angles/margins|drop flag
KXKY Number of grid points in x;y
GDPFUN Scalar grid or vector grid function
GDFILE Grid file
RADTIM Radar composite current/dattim
RADDUR Radar time window (minutes prior to RADTIM)
RADFRQ Update Frequency
CPYFIL Grid file whose navigation is to be used in new grid file |
STNFIL Station information file
MAXGRD Maximum number of grids
RADMODE Radar operational mode
NDVAL Replacement value for "ND" (default RMISSD)
```


GDRADR samples NEXRAD Level III (NIDS) products to a common grid projection.

GDFILE specifies the output grid file. If the file does not already exist, the file is cre-

ated using the grid defined by CPYFIL, or if CPYFIL is not defined then by PROJ, GR-

DAREA, and KXKY.

CPYFIL may provide either an existing grid file to read the projection information

from, or a grid number (#nnn) defined in grdnav.tbl.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if the output file

does not already exist, and CPYFIL is blank.

STNFIL is the station table which supplies radar IDs to be searched for the composite.

If STNFIL is blank, then 'nexrad.tbl' is used by default.

GDPFUN is a list of data parameters for which composites are created. The NEXRAD

file naming is assumed to be such that the site identifier and the product type are both

present in the directory/file naming structure. The datatype.tbl template NEXRIII is

used to provide the file naming convention used. If NEXRIII is not found in the tem-

plate database, a default directory structure for NEXRAD data is assumed where the

root directory $RAD/NIDS contains a tree structure supporting %SITE%/%PROD%/

%PROD%_YYYYMMDD_HHNN file names. The %SITE% template will be re-

placed by the site IDs in the STNFIL table. The %PROD% will be replaced by the GD-

PFUN product name. The GEMPAK data/time template will be used with RADTIM

and RADDUR to determine which NEXRAD products are in the valid time range.


RADTIM determined the output grid time for the radar composite. The value of

RADTIM may either be 'current', or a GEMPAK dattim. If 'current' is selected for

RADTIM, then the current system clock time is used. No data files later than RADTIM

will be included in the composite. RADDUR provides the time window previous to

RADTIM in order to include data for each site. The time closest to RADTIM will be

used. A default RADDUR of 30 minutes is used if RADDUR is blank.

RADFRQ is the frequency in minutes at which the program will run. When RADFRQ

is defined, GDRADR will wait for the specified time before rerunning. This option is

most useful when RADTIM is set to 'current'. When the program is sleeping, ctrl-c can

be used to exit the loop and return to the dynamic tutor. If RADFRQ is not set, the dy-

namic tutor will be re-entered at the end of processing the radar mosaic.

RADMODE allows the user to select whether to include radar data from sites operating

in (P) precipitation/storm mode, (C) clear air mode, and/or (M) maintainence mode.

The default, if none are specified is data from all 3 modes (PCM). Multiple modes may

be specified.

## Examples

1. Create a 4km National composite of NEXRAD base reflectivity (N0R) and

echo tops (NET). Use the current time with a 30 minute window for

data. Rerun the mosaic creation continuously with a 5 minute wait period

between each update. Use the nexrad.tbl station table. Create the grid

output file using the YYYYMMDD_radar.gem file name template. Accept

data only from radars operating in precipitation mode.

```
PROJ = lcc/25;-103;60
GRDAREA = 23.0;-120.0;47.0;-65.0
KXKY = 720;500
GDPFUN = n0r! net
GDFILE = YYYYMMDD_radr.gem
RADTIM = current
RADDUR = 30
RADFRQ = 5
CPYFIL =
STNFIL = nexrad.tbl
MAXGRD = 1000
RADMODE = P
```
2. Create a radar mosaic using grid #218 for the total precipitation nexrad

product (NTP). Use the default radar station table. Use a 15 minute

time window ending at to 010302/1200.

#PROJ =

#GRDAREA =

#KXKY =

```
GDPFUN = ntp
```

```
GDFILE = YYYYMMDD_ntp.gem
RADTIM = 010302/1200
RADDUR = 15
RADFRQ =
CPYFIL = #218
STNFIL =
MAXGRD = 500
RADMODE =
```
### Error Messages

```
[GDRADR+5] Mode rejected: ...

[GDRADR+4] Write grid ...

[GDRADR+3] Using default station file ...

[GDRADR+2] NEXRIII template not found using ...

[GDRADR+1] Too old: ...

[GDRADR0] Using: ...

[GDRADR-1] Fatal error initializing TAE.

[GDRADR-2] Fatal error reading TAE parameters.

[GDRADR-3] Fatal error initializing GEMPLT.

[GDRADR-4] Failed to read grid projection ...

[GDRADR-5] Could not create grid file ...

[GDRADR-6] Could not open grid file ...

[GDRADR-7] Warning: navigation block does not match file

[GDRADR-8] Station table ... not found
```

## 4.24 GDSTAT

GDSTAT computes statistics on a time series of scalar grids.



```
GDFILE Grid file
GDOUTF Output grid file
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GRDNAM Grid parameter name
```


GDSTAT computes the maximum, minimum, average, standard deviation, and number

of reporting times at each grid point for a time series of grids. The output grids will be

written to the output grid file.

The input grid files are specified in GDFILE. The output file is specified in GDOUTF.

The output file may be one of the input files. A total of three files may be entered.

GFUNC specifies the grid diagnostic function to be computed. GDATTIM must be a

time range. The output grid names will be the name in GRDNAM prefixed by MAX,

MIN, AVG, STD, and CNT. If GRDNAM is blank, the default name from the grid di-

agnostic computation will be used.

## Examples

1. Compute the grid statistics for the 850 mb vorticity field using all the

times in the grid file. Name the output grids MAXVOR, MINVOR,

AVGVOR, STDVOR and CNTVOR and write them into the same file.

Write the output grids into the same file.

```
GDFILE = ngm.grd
GDOUTF = ngm.grd
GFUNC = vor(wnd)
GVCORD = pres
GLEVEL = 850
GDATTIM = all
GRDNAM = vor
```
2. Compute the grid statistics for the surface temperature data using the data

for every 12 hours from Jan 1 to Jan 15. The input and output files are

different. The default names will be used.

```
GDFILE = jan90sfc.grd
GDOUTF = stat.grd
```

```
GFUNC = tmpc
GVCORD = none
GLEVEL = 0
GDATTIM = 920101/0000-920115/0000-12
GRDNAM =
```
### Error Messages

```
[GDSTAT-1] Fatal error initializing TAE.

[GDSTAT-2] Fatal error reading TAE parameters.

[GDSTAT-3] Fatal error initializing GEMPLT.

[GDSTAT-4] No valid times found in grid file.

[GDSTAT-6] There are fewer than four grids.

[GDSTAT-7] Error writing grid ... to output file.

[GDSTAT-8] Grid navigation has changed.
```

## 4.25 GDSTREAM

GDSTREAM draws streamlines through a vector grid.



```
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GVECT Vector grid
GDFILE Grid file
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
LINE Color/type/width/label/smth/fltr
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
CLEAR Clear screen flag
WIND Wind symbol/siz/wdth/typ/hdsz
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
STREAM lines/arrows/stop/slow/scale
```


GDSTREAM draws streamlines through any vector grid computed by the grid diagnos-

tics package. The vector grid is specified in GVECT.

Streamlines are plotted in an animation sequence by specifying more than one time in

GDATTIM.

The line color, dash pattern and line width are set in LINE. A map, title and latitude/

longitude lines may also be included. The size of the arrow heads may be controlled

by specifying the fifth value in WIND.

The STREAM parameter controls several parameters dealing with the overall stream-

line calculation and display. "Lines" is a real number multiplier which controls the

number (density) of streamlines drawn (default 1.0); "arrows" is a real number multi-

plier which controls the number of arrowheads displayed (default 1.5*lines); "stop" is

a real number multiplier which controls how close a streamline comes to another

streamline before it is stopped being drawn (default 0.5); "slow" is a real number mul-

tiplier which controls the minimum vector speed threshold for stopping a streamline


(default 0.67); "scale" is a real number multiplier which controls how much the input

vector field is scaled prior to streamline calculation (default 0.33).

## Examples

1. Draw streamlines through the gridded wind field at 850 mb using color 3.

Add a North Polar Stereographic map in color 1 and line type 7 and a

title in color 5. Center the graphics area on Ohio.

```
GDATTIM = f24
GLEVEL = 850
GVCORD = pres
GVECT = wnd
GDFILE = $GEMDATA/hrcbob.grd
GAREA = oh-
SATFIL =
RADFIL =
IMCBAR =
LINE = 3//2
MAP = 1/7
LATLON = 2/10/1/1/5;5
TITLE = 5
PANEL = 0
TEXT = 1
DEVICE = xw
PROJ = nps
CLEAR = yes
WIND =
LUTFIL =
STNPLT =
```
2. Similar to 1), except for a global 1x1 degree dataset over the Pacific

Ocean, specifying the STREAM parameter to properly thin and display the

field given the overall density of grid boxes.

```
GDFILE = $MODEL/fnl/fnl_96052800
GDATTIM = f00
GLEVEL = 250
GAREA = -25;-180;55;-45
PROJ = mer
LATLON = 31/12/2//10
STREAM = 0.4/0.8/3//
WIND = am1/2/3//0.8
TITLE = 31/-2/@ FNL ANALYSIS VALID ~ NHC MIAMI FLA
```
3. Clear the screen and draw the streamlines of the thermal wind. Make

the arrowheads 0.75 of the default size. Plot the streamlines in a second

window named "window2".

```
GDATTIM = f24
```

#GLEVEL = 500:850

```
GVCORD = pres
GVECT = thrm
GDFILE = $GEMDATA/hrcbob.grd
GAREA = oh-
SATFIL =
RADFIL =
IMCBAR =
LINE = 3//2
MAP = 1/7
LATLON = 2/10/1/1/5;5
TITLE = 5
PANEL = 0
TEXT = 1
DEVICE = xw|window2
PROJ = nps
CLEAR = yes
WIND = an3////.75
LUTFIL =
STNPLT =
```
### Error Messages

```
[GDSTREAM-1] Fatal error initializing TAE.

[GDSTREAM-2] Fatal error reading TAE parameters.

[GDSTREAM-3] Fatal error initializing GEMPLT.
```

## 4.26 GDTHGT

GDTHGT draws contours and wind barbs or arrows on a time section at a point within

a grid.



```
GPOINT Grid point
GDATTIM Grid date/time
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GVECT Vector grid
GDFILE Grid file
PTYPE Plot type/h:w ratio/margins
TAXIS Time1-time2-tinc;lbl;gln;tck
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
BORDER Background color/type/width
LINE Color/type/width/label/smth/fltr
CINT Contour interval/min/max
CONTUR Subbox/smooth
WIND Wind symbol/siz/wdth/typ/hdsz
TITLE Title color/line/title
CLEAR Clear screen flag
SCALE Scalar scale / vector scale
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
```


GDTHGT is a user contributed program that draws a vertical time section at a location

in a grid field. The location is specified by setting GPOINT. Gridded data are interpo-

lated to the time-section plane at regular intervals of time specified in GDATTIM.

The grid vertical coordinate (GVCORD) may be PRES, THTA, or HGHT, depending

on what is available in the grid file. The vertical axis scaling, set in PTYPE, can be

LIN, LOG, KAP or STUVE. STUVE and KAP are the same; SKEWT may not be en-

tered. The plot aspect ratio and margins may also be entered in PTYPE.

Vector fields may be specified in GVECT and displayed as arrows or barbs. For a wind

vector field, the default is north relative direction. A "/G" indicates grid relative direc-

tion. The vector function CIRC cannot be computed for time sections.

If M is entered in WIND, winds will be displayed in meters per second unless the

KNOTV operator has been specified in GVECT, in which case the winds will be dis-

played in knots. IF K is entered in WIND, the wind is displayed in knots.


## Examples

1. Plot temperature in Celsius on a log-P chart on a time section for all

times at BWI. Plot the contours in color number 2, using thick, solid

lines. Plot wind barbs for the forecast wind. Plot the chart to 50

millibars using the default labels.

```
GPOINT = bwi
GDATTIM = all
GVCORD = PRES
GFUNC = temp
GVECT = wnd
GDFILE = $GEMDATA/HRCBOB.GRD
PTYPE = log
TAXIS =
YAXIS = /50
BORDER = 1
LINE = 2/2/3/1
CINT = 5
WIND = bm1
TITLE = 1
CLEAR = YES
SCALE = 999
PANEL = 0
DEVICE = xw
TEXT = 1/21//hw
```
2. Now, using the same file, plot a log pressure time section of the

vorticity advection. Positive values are contoured with solid lines,

negative values are contoured with dashed lines. Plot the wind in grid

relative direction.

```
GPOINT = bwi
GDATTIM = all
GVCORD = PRES
GFUNC = adv(avor(obs);obs)
GVECT = wnd/G
GDFILE = $GEMDATA/HRCBOB.GRD
PTYPE = log
TAXIS =
YAXIS = /50
BORDER = 1
LINE = 2/-2/3/1
CINT = 5
WIND = bm1
TITLE = 1
CLEAR = YES
SCALE = 999
PANEL = 0
DEVICE = xw
TEXT = 1/21//hw
```

### Error Messages

```
[GDTHGT-1] Fatal error initializing TAE.

[GDTHGT-2] Fatal error reading TAE parameters.

[GDTHGT-3] Fatal error initializing GEMPLT.

[GDTHGT-4] Invalid vertical coordinate.

[GDTHGT-7] Invalid axis type.

[GDTHGT-8] Invalid vertical coordinate type.

[GDTHGT-20] Input for YAXIS is invalid.
```

## 4.27 GDTOPO

GDTOPO creates a GEMPAK GRID file from a topographic raster.



```
GDFILE Grid file
GAREA Graphics area
GDATTIM Grid date/time
TOPOFL Topographic file
```


GDTOPO will create a GEMPAK GRID file of topographic values. A new grid file

will be created. The grid can be displayed in grid programs as parameter TOPO, verti-

cal coordinate PRES, and level 0.

Topography files can be obtained from: 5 minute dataset: http://my.unidata.ucar.edu/

downloads/168/world_topo.5min.gz 30 second dataset: http://my.unidata.ucar.edu/

downloads/169/world_topo.30s.gz

Topography files can be placed in $GEMTBL/unidata/.

## Examples

1. Create a topographic grid for the US (CONUS) region using the 5 minute

resolution dataset.

```
GDFILE = topo_us.gem
GAREA = 22;-125;57;-57
GDATTIM = 010101/0000
TOPOFL = dem5
```
2. Create a topographic grid for the PA region using the 30 second

resolution dataset.

```
GDFILE = testme.gem
GAREA = unv+
GDATTIM = 010101/0000
TOPOFL = dem30
```

### Error Messages

```
[GDTOPO-1] Fatal error initializing TAE.

[GDTOPO-2] Fatal error reading TAE parameters.

[GDTOPO-3] Error initializing GEMPLT.

[GDTOPO-4] There is no input file specified.

[GDTOPO-5] Different navigation in input, output files.

[GDTOPO-6] Error opening input files.

[GDTOPO-7] Error writing output grid.

[GDTOPO-8] Only one output file is permitted.

[GDTOPO-9] Output file open failure.

[GDTOPO-10] Grid files have different navigations.

[GDTOPO-11] Number of grid points is too large.

[GDTOPO-12] Topography file does not exist.

[GDTOPO-13] Grid file already exists.
```

## 4.28 GDTSER

GDTSER draws a time series of a scalar at a single level.



```
GPOINT Grid point
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GFUNC Scalar grid
GDFILE Grid file
PTYPE Plot type/h:w ratio/margins
TAXIS Time1-time2-tinc;lbl;gln;tck
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
BORDER Background color/type/width
LINE Color/type/width/label/smth/fltr
MARKER Marker color/type/size/width/hw
TITLE Title color/line/title
CLEAR Clear screen flag
SCALE Scalar scale / vector scale
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
```


GDTSER draws a time series of a scalar parameter at a fixed level. The variable may

be any scalar computed by the grid diagnostics package.

GPOINT specifies the location for the time series. It may be entered as a latitude and

longitude separated with a semicolon, as a station character identifier or station number,

or as a set of x and y grid points separated with a semicolon and preceded with an @.

The location may also be selected graphically by setting GPOINT using the CURSOR

command. The location of the time series is then selected by clicking on a horizontal

map in another GEMPAK XW window.

The times to plot are specified in GDATTIM. Only those times specified will be plot-

ted. Lines will be drawn connecting the data points provided no more than two points

are missing between segments. Note that the times may represent a series of base times

with the same or no forecast time, or a series with the same base time and a list of fore-

cast times.

The time axis is specified in TAXIS using the usual GEMPAK date/time conventions,

including FIRST and LAST. If TAXIS has no increment, a reasonable value is chosen.

If TAXIS is blank, the limits are taken from the first and last valid times set in GDAT-

TIM.


If the user desires multiple lines on the same graph, TAXIS and YAXIS should be ex-

plicitly set; then LINE, MARKER, and TITLE can be varied for successive combina-

tions of GLEVEL, GVCORD, GPOINT, and GFUNC.

The axis type, height-to-width ratio of the plot and the margins may be specified in

PTYPE.

## Examples

1. Plot the BWI 500 mb temperature for the all of the forecast times in the

file. Label the Y axis from -15 to 15 in increments of 3 degrees. Label

the time axis from 18/21 to 20/03 in increments of 12 h. Make the plot

half as high as wide.

```
GPOINT = bwi
GDATTIM = all
GLEVEL = 500
GVCORD = pres
GFUNC = tmpc
GDFILE = $GEMDATA/hrcbob.grd
PTYPE = lin/.5
TAXIS = 18/21-20/03
YAXIS = -15/15/3
BORDER = 1
LINE = 3//3
MARKER = 2/13/1.25/2
TITLE = 5//^ _ @ #
CLEAR = yes
SCALE = 0
PANEL = 0
DEVICE = xw
TEXT = 1
```
2. Overlay the 700 mb temperature for all of the forecast times in the file.

Use the above specifications for the other parameters.

```
GPOINT = bwi
GDATTIM = all
GLEVEL = 700
GVCORD = pres
GFUNC = tmpc
GDFILE = $GEMDATA/hrcbob.grd
PTYPE = lin/.5
TAXIS = 18/21-20/03
YAXIS = -15/15/3
BORDER = 1
LINE = 2//3
MARKER = 3/13/1.25/2
TITLE = 5/-2/^ _ @ #
CLEAR = no
```

#SCALE = 0

#PANEL = 0

```
DEVICE = xw
TEXT = 1
```
### Error Messages

```
[GDTSER-1] Fatal error initializing TAE.

[GDTSER-2] Fatal error reading TAE parameters.

[GDTSER-3] Fatal error initializing GEMPLT.

[GDTSER-4] Input for GPOINT is invalid.

[GDTSER-5] Input for GLEVEL is invalid.

[GDTSER-6] Input for GVCORD is invalid.

[GDTSER-7] Input for PTYPE is invalid.

[GDTSER-8] Error defining graph coordinates.

[GDTSER-9] No points found for plot of ....

[GDTSER-10] Some points missing for plot of ....

[GDTSER-11] Input for ... in TAXIS is invalid.

[GDTSER-12] Time range is size zero.

[GDTSER-13] No grids found with times in GDATTIM.
```

## 4.29 GDVINT

GDVINT performs interpolation between vertical coordinates.



```
GDFILE Grid file
GDOUTF Output grid file
GDATTIM Grid date/time
GVCORD Grid vertical coordinate
GLEVEL Grid level
MAXGRD Maximum number of grids
GAREA Graphics area
VCOORD Vertical coordinate type
```


This program performs vertical interpolations from one input vertical coordinate to an-

other. The output grid file may be the same as the input file or be a separate file. If

areal subsetting is specified, output must be to another GEMPAK file.

The input file is specified in GDFILE and the output file in GDOUTF. If GDOUTF

does not exist, then a new file will be created with the maximum number of grids given

in MAXGRD. GDATTIM specifies the time at which to perform the interpolation.

GVCORD specifies the input and output vertical coordinates separated by a slash. The

coordinates supported are:

```
PRES -- pressure
THTA -- potential temperature
HGHT -- height above sea level
SGMA -- sigma p
ETA -- eta
ZAGL -- height above ground level
```
When SGMA is specified, the pressure on SGMA=0 is given following a semicolon.

For example, if the pressure at SGMA=0 is 50 mb, the specification in GVCORD is

#SGMA:50

GLEVEL specifies the vertical coordinate levels to interpolate. This may be a single

level, a list of levels separated by semicolons or a range with an increment separated by

dashes.

GAREA specifies areal subsetting. It can be specified as either lower-left and upper-

right grid coordinates, separated by semicolons and preceded by a single @, or as low-

er-left and upper-right latitude and longitude coordinates separated by semicolons. In

the latter case, the locations are rounded to the nearest grid points. The navigation pro-

jection and its parameters are used along with the specified corner points to set the nav-


igation in the output grid file. If areal subsetting is used, the output file must be

different from the input file.

VCOORD specifies a list of GEMPAK coordinate names. All grids at the specified

time having one of these coordinate names will be transfered to the output file. This

allows surface grids with vertical coordinate types NONE or ESFC to be transfered into

the output file. It also allows subsetting of grids in other vertical coordinates to be put

into the output file. VCOORD is also used to specify the Lorenz condition for interpo-

lation into THTA coordinate. To specify the Lorenz condition, conclude the VCOORD

input with /L.

If the output vertical coordinate is THTA and the Lorenz condition is specified, under-

ground isentropic surfaces are assigned surface values, and PSYM is built down hydro-

statically using the surface pressure in the Exner function. In all other cases,

underground surfaces are assigned missing values.

If the output vertical coordinate is PRES, underground pressure levels are assigned a

height value computed by extrapolating downward using the surface values of pressure,

temperature, and height. A constant lapse rate atmosphere with the standard lapse rate

of 6.5 K / 1000 m is assumed.

If surface values of the interpolated parameters are not available, GDVINT extrapolates

values to the surface from two levels above the surface. These values are used in the

interpolation and are placed in the output file where they are assigned the output verti-

cal coordinate and GLEVEL=0.

If heights are not available in the input grid file, a hydrostatic integration is done in the

input coordinate to generate values for the interpolation.

## Examples

1. Interpolate from sigma, with P(SGMA=0) = 0 to theta over a subgrid

defined by the latitude and longitude bounds. The actual subgrid will be

defined by the grid points closest to the latitude longitude bounds. Also

transfer all grids with vertical coordinates NONE, MSLV and ESFC into

the output file. Apply the Lorenz condition for underground surfaces.

```
GDFILE = z00jun16.ngmsgm
GDOUTF = z00jun16.ngmtht
GDATTIM = f24
GLEVEL = 256-412-4
GVCORD = sgma:0/thta
MAXGRD = 2000
GAREA = 30;-100;45;-70
VCOORD = none;mslv;esfc/L
```

2. Interpolate from pressure to the height above ground coordinate over a

subset of the grid specified by grid index bounds.

```
GDFILE = z00jun16.ngmprs
GDOUTF = z00jun16.ngmzag
GDATTIM = f36
GLEVEL = 500-15000-500
GVCORD = pres/zagl
MAXGRD = 3200
GAREA = @12;23;34;78
VCOORD = mslv;esfc
```
3. Interpolate from isentropic coordinates to sigma (ptop=50) over the entire

grid. Put the output in the same file as the input. Note that sigma

values are scaled by 10000. In this example, maxgrd is ignored.

```
GDFILE = z00jun16.ngm
GDOUTF = z00jun16.ngm
GDATTIM = f48
GLEVEL = 9500-500-500
GVCORD = thta/sgma:50
MAXGRD = 3200
GAREA = grid
VCOORD = none;esfc
```

### Error Messages

```
[GDVINT+4] Warning--no surface elevation grid exists.

[GDVINT+3] No pressure on this level.

[GDVINT+2] No output vertical coordinate value on this level.

[GDVINT+1] All values are missing.

[GDVINT-1] Fatal error initializing TAE.

[GDVINT-2] Fatal error reading TAE parameters.

[GDVINT-3] Fatal error initializing GEMPLT.

[GDVINT-4] No thermodynamic data in the input file.

[GDVINT-5] Navigation blocks are different.

[GDVINT-6] Required surface pressure is absent.

[GDVINT-7] Required surface elevation is absent.

[GDVINT-8] Too few levels to interpolate this parameter.

[GDVINT-9] No parameters to interpolate -- check GVCORD, GDATTIM.

[GDVINT-10] Top pressure for SGMA or ETA missing.

[GDVINT-11] Subset region is invalid.

[GDVINT-12] File ... cannot be opened.

[GDVINT-13] User specified coordinates are invalid.

[GDVINT-14] Levels not correctly specified.

[GDVINT-15] Grid size is too large.
```

## 4.30 GDWIND

GDWIND displays a vector grid using wind barbs or arrows.



```
GDATTIM Grid date/time
GLEVEL Grid level
GVCORD Grid vertical coordinate
GVECT Vector grid
GDFILE Grid file
GAREA Graphics area
IJSKIP Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
SKIP Skip_cntr/skip_plt_x;skip_plt_y
WIND Wind symbol/siz/wdth/typ/hdsz
REFVEC Mag;x;y;txtsiz/font/wdth/HW;labl
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
CLEAR Clear screen flag
SCALE Scalar scale / vector scale
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
```


GDWIND draws wind barbs or arrows at each grid point of a vector grid. Vector plots

can be drawn for any vector field computed using the GEMPAK grid diagnostic func-

tions. Vectors may be drawn in any valid GEMPAK projection and may be overlaid on

images.

A list of times may be given in GDATTIM allowing animation of the plots.

SKIP specifies the number of points to skip in both coordinate directions. For example,

SKIP = /1;2 will display winds at every other grid point in the x direction and every

third grid point in the y direction. SKIP = /0 displays winds at every grid point. Wind

barbs or arrows can also be staggered by specifying negative values for SKIP.

IF M is entered as part of the wind symbol, winds will be displayed in m/s, unless the

KNOTV operator has been specified in GVECT, in which case the winds will be dis-

played in knots. If K is entered in WIND, the wind is displayed in knots.


## Examples

1. Draw wind barbs in m/s in a staggered array. The vector field is the

gridded wind at 850 mb for the 18 hour forecast. The barbs are plotted

as an overlay on the entire visible satellite image for 18Z.

```
GDATTIM = f18
GLEVEL = 850
GVCORD = pres
GVECT = wnd
GDFILE = $GEMDATA/hrcbob.grd
GAREA = dset
SATFIL = $GEMDATA/VIS_910819_1801
RADFIL =
IMCBAR = 1/V/LL/0;.05/.90
SKIP = /-1
WIND = bk6/1.5/3
REFVEC = 10
MAP = 1/7
LATLON = 2/10/1/1/5;5
PANEL = 0
TITLE = 1
DEVICE = xw
PROJ = sat
CLEAR = yes
SCALE =
TEXT = 1
LUTFIL =
STNPLT =
```
2. Using the values of the variables supplied above, change the GAREA to

Rhode Island. The satellite image is subset for the given area and the

data is replotted.

```
GDATTIM = f18
GLEVEL = 850
GVCORD = pres
GVECT = wnd
GDFILE = $GEMDATA/hrcbob.grd
GAREA = ri
SATFIL = $GEMDATA/VIS_910819_1801
RADFIL =
IMCBAR = 1/V/LL/0;.05/.90
SKIP = 0
WIND = bk6/1.5/3
REFVEC = 10
MAP = 1/7
LATLON = 2/10/1/1/5;5
PANEL = 0
TITLE = 1
DEVICE = xw
PROJ = sat
```

```
CLEAR = yes
SCALE =
TEXT = 1
LUTFIL =
STNPLT =
```
3. Plot arrows for the temperature gradient at 850 mb. Scale the data by

10**6. Also, plot a reference vector with the label "degrees C / meter".

The display area is centered on New York.

```
GDATTIM = f18
GLEVEL = 850
GVCORD = pres
GVECT = grad(tmpc)
GDFILE = $GEMDATA/hrcbob.grd
GAREA = ny-
SATFIL =
RADFIL =
IMCBAR =
SKIP = 0
WIND = am7//3
REFVEC = 10;;;;degrees C / meter
MAP = 1/7
LATLON = 2/10/1/1/5;5
PANEL = 0
TITLE = 1
DEVICE = xw
PROJ = nps
CLEAR = yes
SCALE = /6
TEXT = 1
LUTFIL =
STNPLT =
```
### Error Messages

```
[GDWIND-1] Fatal error initializing TAE.

[GDWIND-2] Fatal error reading TAE parameters.

[GDWIND-3] Fatal error initializing GEMPLT.

[GDWIND-13] No times in grid file.
```

## 4.31 GPANOT

GPANOT will allow the user to place objects at any location on the current graphic de-

vice.



```
GDFILE Grid file
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
PROJ Map projection/angles/margins|drop flag
GAREA Graphics area
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
CLEAR Clear screen flag
SHAPE Object to draw
INFO Shape information
LOCI Point(s) associated with SHAPE
LINE Color/type/width/label/smth/fltr
CTYPE Contour type: C/F
```


This program will plot an object at any location on the graphic device. It uses Normal,

Map and Grid coordinates. See the help file on the specific parameters, since the defi-

nitions change for each shape specified.

## Examples

1. Plot the text string "This is plot 1" centered at normal coordinates .5;.5,

in color 15, with a filled box of color 25 around the text.

```
DEVICE = xw
PANEL = 0
SHAPE = text
INFO = 1/3////c/hw/This is plot 1
LOCI = .5;.5
LINE = 15///25
CTYPE = f
```
2. Plot a square centered at grid location 40;23, with radius 2 grid points,

and rotated 45 degrees. Use color 8 for the unfilled square, and use line

type 5.

```
SHAPE = regpoly
INFO = 5/2/4/45
LOCI = @40;23
LINE = 8
CTYPE = c
```

### Error Messages

```
[GPANOT-1] Fatal error initializing TAE.

[GPANOT-2] Fatal error reading TAE parameters.

[GPANOT-3] Fatal error initializing GEMPLT.

[GPANOT-4] No symbol specified.

[GPANOT-5] No front type specified.

[GPANOT-6] Must have at least 2 pips.
```

## 4.32 GPBOX

GPBOX draws a box around a region.



```
LINE Color/type/width/label/smth/fltr
REGION Region type
DEVICE Device|name|x size;y size|color type
```


This program draws a box around a region on the current graphics device. REGION

can be specified as VIEW, PLOT, or DEVICE. VIEW is the view region of the graph-

ics device. The view region may be changed in GEMPAK programs using the param-

eter PANEL. The PLOT region is the area used for the data plot, excluding margins.

DEVICE is the entire device space.

Note that a box may also be drawn around the VIEW region using the PANEL variable.

## Examples

1. Draw a solid line of width 1 around the view region on the xw device.

#LINE = 1

```
REGION = view
DEVICE = xw
```
2. Draw a line around the device region in color 2, using line type 3 and

width 5.

#LINE = 2/3/5

```
REGION = device
```
### Error Messages

```
[GPBOX-1] Fatal error initializing TAE.

[GPBOX-2] Fatal error in reading the TAE parameters.

[GPBOX-3] Fatal error initializing GEMPLT.
```

## 4.33 GPCLEAR

GPCLEAR clears the current graphics device.



```
DEVICE Device|name|x size;y size|color type
```


This program clears the graphics device. On an interactive device, the screen is erased.

On a hardcopy device, the next plot will be on a new page.

### Error Messages

```
[GPCLEAR-1] Fatal error initializing TAE.

[GPCLEAR-2] Fatal error reading TAE parameters.

[GPCLEAR-3] Fatal error initializing GEMPLT.
```

## 4.34 GPCLOSE

GPCLOSE closes the specified graphics output window or file.



```
DEVICE Device|name|x size;y size|color type
```


GPCLOSE closes the graphics output window or file specified in the DEVICE param-

eter.

## Examples

1. Close the window named "window2".

```
DEVICE = xw|window2
```
### Error Messages

```
[GPCLOSE1] Cannot close window. Blank name is invalid.

[GPCLOSE-1] Fatal error initializing TAE.

[GPCLOSE-3] Fatal error initializing GEMPLT.
```

## 4.35 GPCOLOR

GPCOLOR changes the colors on a color device.



```
COLORS Color list
DEVICE Device|name|x size;y size|color type
```


This program changes the color components of color numbers on color devices where

the components can be set. See the help information on COLORS for more informa-

tion.

## Examples

1. Set color number 4 to violet.

```
COLORS = 4=violet
```
2. Set color number 2 to yellow by specifying the red, green, and blue

components.

#COLORS = 2=1:1:0

### Error Messages

```
[GPCOLOR-1] Fatal error initializing TAE.

[GPCOLOR-2] Fatal error reading TAE parameters.

[GPCOLOR-3] Fatal error initializing GEMPLT.
```

## 4.36 GPEND

GPEND terminates the GEMPLT subprocesses.



None



This program terminates the GEMPLT plotting package. It stops the GEMPLT subpro-

cesses and should be run at the end of each GEMPAK plotting session.

### Error Messages

```
[GPEND-1] Fatal error initializing TAE.

[GPEND-3] Fatal error initializing GEMPLT.
```

## 4.37 GPFAX

GPFAX creates a Postscript, GIF, or TIFF file, or an X Windows display from a 6-bit

fax file.



```
DEVICE Device|name|x size;y size|color type
FAXFIL 6-bit FAX product filename
```


GPFAX reads a 6-bit FAX file and creates an output product in either the PostScript,

GIF, or TIFF format. The program can also display the 6-bit FAX product as image

using XW device driver.

Only one 6-bit FAX product can be processed at a time. If there are multiple products

in a single file, then only the first product is read.

The output product size, specified in the DEVICE parameter, should be properly set to

correctly scale the FAX product. For the GIF and TIFF devices, the xsize and ysize

should be obtained from the entries in the table $GEMTBL/pgen/faxprod.tbl that cor-

respond to the input FAX product identification number. For example, the x and y di-

mensions for the 411X FAX product are 1728 and 864, respectively. These are

obtained from the fourth and fifth columns corresponding to the 411X entry in the table.

For the TIFF device driver, the output file is always named as FAX.tiff. The output file

must be renamed, otherwise it will be overwritten by subsequent executions of the pro-

gram.

## Examples

1. Create a GIF file of 5-Day MRF MOS Mean chart, (6-bit FAX ID 411X).

```
DEVICE = gif|411X.gif|1728;864
FAXFIL = 411X.6bt
```
2. Create a TIFF file of the four panel low-level significant weather chart,

(6-bit FAX ID 501X).

```
DEVICE = tiff||1728;1133
FAXFIL = 501X.6bt
```

### Error Messages

```
[GPFAX-1] Fatal error.

[GPFAX-3] Fatal error initializing GEMPLT.
```

## 4.38 GPFRONT

GPFRONT is a version of GPMAP that plots map symbols interpreted from the ASUS1

bulletins. Forecast front positions from FSUS2 bulletins can be plotted by specifying

the forecast hour desired from the bulletin (typically a bulletin contains 12 and 24 hour

forecast positions).



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
LUTFIL Enhancement lookup table filename
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
```


GPFRONT is derived from the standard GEMPAK program GPMAP. The behaviour

is similar to gpmap, with the addition of plotting map symbols defined in an ASUS bul-

letin, with the map area specified. Satellite and radar data may be used as backgrounds.

For ASUS1 bulletins which contain a single analysis time, ASHR is ignored. For

FSUS2 bulletins that contain forecast positions for 12 and 24 hour progs, ASHR should

be the desired forecast hour (eg 12 or 24). ASATT can be used to specify the scaling

of symbols drawn (default is 1.0), and whether to label High and Low values (default

is Y).

## Examples

1. Draw a Lambert Conformal map of the US. Display frontal position from

the file 98032016_frt.wmo.

```
GAREA = uslcc
PROJ = lcc
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 2/-1/Frontal Positions
TEXT = 3/31/2/hw
CLEAR = yes
DEVICE = xw
ASFIL = 98032016_frt.wmo
```

#ASHR =

#ASATT =

```
LUTFIL = default
IMCBAR =
```
### Error Messages

```
[GPFRONT-1] Fatal error initializing TAE.

[GPFRONT-2] Fatal error reading TAE parameters.

[GPFRONT-3] Fatal error initializing GEMPLT.

[GPFRONT-4] Frontal position file not found
```

## 4.39 GPKGRF

GPKGRF generates the track error watch/warn plot for the TPC. It also creates the ex-

perimental tropical cyclone watch/warning text message.



```
STRMID Storm id / Advisory num / Device / Num fcst days / Text flag
```


GPKGRF will plot the track error watch/warn plot for each tropical system specified.

The system(s) to be plotted may be specified in one of two ways:

(1) A storm identifier and advisory number may be explicitly

```
specified using the STRMID parameter.
```
(2) Storm identifiers and advisory numbers may be gotten by

```
checking the storm history file against the contents of
the storm advisory directory. Only the latest advisory
for each storm in the directory (which is not found in the
history file) will be processed. This method of running is
specified by leaving the storm id and/or advisory number
blank in the STRMID parameter, and is the default mode.
See the help for STRMID for further details.
```
The track error watch/warn plot displays the current location and the forecasted track

for the tropical system. It also shows the track error envelope. Active hurricane watch-

es and warnings and tropical storm watches and warnings, if any, will be displayed. If

the advisory watch/warn text is identical to that of the immediately preceding advisory,

the breakpoints from the previous advisory will be used. If the text differs, the user is

given the option of entering new breakpoints, using the previous breakpoints, or show-

ing no breakpoints.

The program can generate XW, GIF, or PostScript files, depending on the value of the

third field of the STRMID parameter. The default is GIF files (with an XW display of

the track error watch/warn file to aid in confirming breakpoints). See the help for STR-

MID for further details.

The program will display either a 3 day or a 5 day forecast, depending on the value of

the fourth field of the STRMID parameter. See the help for STRMID for further details.

An experimental watch/warning text message may also be generated for each plotted

Atlantic storm having breakpoint information for the continental U.S., depending on

the value of the fifth field of the STRMID parameter. This message, which may be seg-

mented, includes UGC county and marine zone information, VTEC information, and

breakpoint names and locations. The message is generated automatically by comparing

previous and current breakpoint information for the storm, and is in a file named KN-


HCTCVATn, where n varies from 1 to 5 depending on the storm number. See the help

for STRMID for further details.

In order to compare the previous and current breakpoint information, a breakpoint file

is generated and saved for each advisory number. (Before the watch/warning text mes-

sage was added to the program, a single breakpoint file existed for the entire life of the

storm,

and was updated as needed.) The breakpoint file name format is

'wwBBNNYY.AAA', where BBNNYY is the storm identifier (basin, storm number,

year) AAA is the 3-digit storm advisory number e.g., 'wwal0403.007' is the breakpoint

file for Atlantic storm number 4 for 2003, advisory number 7.

## Examples

1. Use the history file to determine which new advisories are to be plotted.

GIF files will be created, preceded by an XW display of the track error

watch/warn file. A 5 day forecast is displayed.

#STRMID =

2. Plot the graphic for advisory number 8 of storm al892001. Display the

graphic in XW only, prompting the user to select the plot for display. A

5 day forecast is displayed.

```
STRMID = al892001/8/test
```
3. Plot the graphic for advisory number 1 of storm al882001 in an XW

window. Processing is automatic unless breakpoints are to be entered. A

3 day forecast is displayed.

```
STRMID = al882001/1/xw/3
```
4. Plot the graphic for advisory number 20 of storm ep1900 (which could

also have been specified as ep192000). A GIF file will be created,

preceded by an XW display of the track error watch/warn file. A 5 day

forecast is displayed.

```
STRMID = ep1900/20/gf
```

### Error Messages

```
[GPKGRF4] No breakpoints for this storm.

[GPKGRF3] Too few points in track error table.

[GPKGRF2] Cannot change ... in color table.

[GPKGRF1] No new storms to process.

[GPKGRF-1] Fatal error initializing TAE.

[GPKGRF-2] Fatal error reading TAE parameters.

[GPKGRF-3] Error in reading advisory.

[GPKGRF-5] Error in opening history_kg file.

[GPKGRF-6] No advisories found in directory.

[GPKGRF-7] Error in opening breakpoint file ... - file may not exist.

[GPKGRF-8] Error in opening average track error table.

[GPKGRF-9] Error in opening breakpoint station plotting table.

[GPKGRF-10] Invalid storm identifier in advisory message.

[GPKGRF-12] Error in opening file name table.
```

## 4.40 GPLTLN

GPLTLN draws a map, latitude/longitude lines with a selected marker, and various im-

age and graphic products.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
LAT Latitude
LON Longitude
PANEL Panel loc/color/dash/width/regn
MARKER Marker color/type/size/width/hw
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
VGFILE Vgfile | scale file | attribute file
PLUS Plus sign size/width
```


GPLTLN draws a map and/or latitude/longitude lines in every 10 degrees with a select-

ed marker for a specified graphics area. A plus sign is placed at the latitude/longitude

intersections. Plots may be drawn in any GEMPAK projection and may be overlaid on

images. GPLTLN also post-processes NMAP Vector Graphics Files (VGFs) to create

products in any format supported by GEMPAK device drivers.

Images are animated if more than one image file is specified in SATFIL or RADFIL.

Images are sampled to correspond to the geographic area specified by GAREA. The

geographic area may be defined graphically by setting GAREA using the CURSOR

command. This command allows for interactive zooming of images and the corre-

sponding map and latitude/longitude plots.

GPLTLN can post-process NMAP VGFs to create products in different formats includ-

ing AFOS, AWIPS Redbook, GIF, TIFF, 6 bit FAX, and PostScript. The VGFILE vari-

able specifies the input VGF and the DEVICE variable specifies the desired output

format. VGF object attributes may be modified when creating products by specifying

a table (attribute file) in the VGFILE variable. This table uses the same format as the

$GEMTBL/pgen/setting.tbl to alter object attributes. For example, the color, width,

and smoothing flag attributes can be changed for a solid line by setting them in the at-

tribute file. The table, $GEMTBL/pgen/uattribd.tbl, provides a template for this capa-

bility. Refer to this table for additional details. GPLTLN can also scale attributes for


an entire class of objects by specifying a table (scale file) in the VGFILE variable. Re-

fer to the scale.fax table in $GEMTBL/pgen for additional information.

## Examples

1. Draw a Polar Stereographic map of the Northern Hemisphere. Draw the

map in color 1. Draw lat/lon lines with marker 15 in color 3. Place a

plus sign on lat/lon intersections every 10 degrees in size 1 and width 3.

The color is same with the marker.

#MAP = 1

#GAREA = -10;-130;-10;50

```
PROJ = str/90;-80;0
SATFIL =
RADFIL =
IMCBAR =
LAT =
LON =
PANEL = 0
MARKER = 3/15
TITLE = 1
TEXT = 1
CLEAR = yes
DEVICE = xw
LUTFIL =
STNPLT =
VGFILE =
PLUS = 1/3
```
### Error Messages

```
[GPLTLN-1] Fatal error initializing TAE.

[GPLTLN-2] Fatal error reading TAE parameters.

[GPLTLN-3] Fatal error initializing GEMPLT.
```

## 4.41 GPMAP

GPMAP draws a map, latitude/longitude lines, and various image and graphic products.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
VGFILE Vgfile | scale file | attribute file
AFOSFL AFOS Graphics File
AWPSFL AWIPS Graphics File
LINE Color/type/width/label/smth/fltr
WATCH End time|Wtch clrs|Tm|Lb|Clr
WARN End time|TS;TN;FF clrs|Tm|Lb|Outline
HRCN End time|colors|syms|Tm|Lb|Mt|Qw|Ft|Name
ISIG End time|colors|Sym|Tm|Id|Mv|Fl
LTNG End time|time ints/colors|markers
ATCF Time|colors|models|Tm|Id|Mv|Mkr|Name
AIRM Airmet Plotting Attributes
NCON NCON attributes
CSIG End time|0_Hr;1_Hr;2_Hr;OL clrs|Seq0|Tm|Mv|Fl|Insty|Seq1|Seq
SVRL End time|SVRL clrs|Tm|Lb|Outline|Clr
BND Bnd name/color/fillsiz/fillpat/filt/minpts!lincol/linpat/lin

TCMG End time|colors|center
QSCT QuickScat Plotting Attributes
WSTM End time|WN;WT;AD clrs|Tm|Lb|Outline
WOU End time|Cnty bnds clrs|Tm|Lb|Wt|Otln|Clr
WCN End time|Cnty bnds clrs|Tm|Lb|Wt|Otln|Clr
```


GPMAP draws a map and/or latitude/longitude lines for a specified graphics area. Plots

may be drawn in any GEMPAK projection and may be overlaid on images. GPMAP

also post-processes NMAP Vector Graphics Files (VGFs) to create products in any for-

mat supported by GEMPAK device drivers. In addition, GPMAP displays various

types of graphics products including AWIPS, AFOS, watches, warnings, tropical or

hurricane storm tracks, international SIGMETs, lightning, ATCF tracks, airmets, non-

convective sigmets, severe local storm watches, winter storm warning, watch and advi-

sory, and convective sigmets.


Images are animated if more than one image file is specified in SATFIL or RADFIL.

Images are sampled to correspond to the geographic area specified by GAREA. The

geographic area may be defined graphically by setting GAREA using the CURSOR

command. This command allows for interactive zooming of images and the corre-

sponding map and latitude/longitude plots.

GPMAP can post-process NMAP VGFs to create products in different formats includ-

ing AFOS, AWIPS Redbook, GIF, TIFF, 6 bit FAX, and PostScript. The VGFILE vari-

able specifies the input VGF and the DEVICE variable specifies the desired output

format. VGF object attributes may be modified when creating products by specifying

a table (attribute file) in the VGFILE variable. This table uses the same format as the

$GEMTBL/pgen/setting.tbl to alter object attributes. For example, the color, width,

and smoothing flag attributes can be changed for a solid line by setting them in the at-

tribute file. The table, $GEMTBL/pgen/uattribd.tbl, provides a template for this capa-

bility. Refer to this table for additional details. GPMAP can also scale attributes for an

entire class of objects by specifying a table (scale file) in the VGFILE variable. Refer

to the scale.fax table in $GEMTBL/pgen for additional information.

GPMAP can also plot the current thunderstorm and tornado watches. The parameter

WATCH sets the ending time and the colors for plotting the watches. The parameter

WARN sets the ending time and the colors for plotting warnings. The parameter

WSTM sets the ending time and the colors for plotting winter storm warning, watch,

and advisory. The parameter CSIG sets the ending time and the colors for plotting con-

vective sigmets. The parameter HRCN sets the ending time and the colors for plotting

tropical depressions, storms, and hurricane positions. Optionally, a specific storm

name may be entered to display only that specific tropical disturbance. The parameter

ISIG sets the ending time and the colors for plotting international SIGMETs. The pa-

rameter LTNG sets the ending time, time increments, colors and markers for plotting

lightning data. The parameter ATCF sets the time, models and colors for plotting

ATCF (Automated Tropical Cyclone Forecast) tracks. As with HRCN, a specific storm

name may be entered. The parameter AIRM sets the ending time and the colors for

plotting airmets. The parameter NCON sets the ending time and the colors for plotting

non- convective sigmets. The parameter SVRL sets the ending time and the colors for

plotting severe local storms watches. The parameter WOU sets the ending time and the

colors for plotting the watch outline update (WOU). The parameter WCN sets the end-

ing time and the colors for plotting the watch county notificatioin(WCN).

## Examples

1. Draw a Polar Stereographic map of the Northern Hemisphere. Draw the

map in color 1 and lat/lon lines in color 2 every 15 degrees.

#MAP = 1

#GAREA = -10;-130;-10;50

```
PROJ = str/90;-80;0
```

#SATFIL =

#RADFIL =

#IMCBAR =

#LATLON = 2/10/1/1/15;15

#PANEL = 0

#TITLE = 1

#TEXT = 1

```
CLEAR = yes
DEVICE = xw
LUTFIL =
STNPLT =
VGFILE =
AFOSFL =
AWPSFL =
LINE =
WATCH =
WARN =
HRCN =
ISIG =
LTNG =
ATCF =
AIRM =
NCON =
CSIG =
SVRL =
WSTM =
WOU =
WCN =
```
2. Display two infrared satellite images in a second window named "IR".

Overlay the VGF test.vgf. In addition, display the latest watches,

warnings, international SIGMETs, lightning, ATCF tracks, airmets and non-

convective and convective sigmets, SLS watches, winter storm messages,

and all tropical disturbances.

#MAP = 1/7

```
GAREA = dset
PROJ = sat
SATFIL = $GEMDATA/IR_910819_1801;19
RADFIL =
IMCBAR = 1/V/LL/0.001;0.04/0.925;0.0125/1
LATLON = 2/10/1/1/10;10
PANEL = 0
TITLE = 1
TEXT = 1
CLEAR = yes
DEVICE = xw|IR
LUTFIL =
STNPLT =
VGFILE = test.vgf
AFOSFL =
AWPSFL =
LINE =
```

```
WATCH = last
WARN = last
HRCN = all
ISIG = last
LTNG = last
ATCF = last
AIRM = last
NCON = last
CSIG = last
SVRL = last
WSTM = last
WOU =
WCN =
```
3. Display two visible satellite images in a third window named "VIS".

Overlay the AFOS test product T01 in color 6 with line width 2.

#MAP = 1/7

```
GAREA = dset
PROJ = sat
SATFIL = $GEMDATA/VIS_910819_1801;19
RADFIL =
IMCBAR = 1/V/LL/0.001;0.04/0.925;0.0125/1
LATLON = 2/10/1/1/10;10
PANEL = 0
TITLE = 1
TEXT = 1
CLEAR = yes
DEVICE = xw|VIS
LUTFIL =
STNPLT =
VGFILE =
AFOSFL = NMCGPHT01
AWPSFL =
LINE = 6//2
WATCH =
WARN =
HRCN =
ISIG =
LTNG =
ATCF =
AIRM =
NCON =
CSIG =
SVRL =
WSTM =
WOU =
WCN =
```
4. Postprocess an NMAP VGF to create an AWIPS Redbook Graphics file.

Change selected object attributes by using the table attrib.xmp

#MAP = 1


```
GAREA = us
PROJ =
SATFIL =
RADFIL =
IMCBAR =
LATLON = 1
PANEL = 0
TITLE = 1
TEXT = 1
CLEAR = y
DEVICE = RBK
LUTFIL = grey
STNPLT =
VGFILE = symbols_big.vgf||attrib.xmp
AFOSFL =
AWPSFL =
LINE = 3
WATCH =
WARN =
HRCN =
ISIG =
LTNG =
ATCF =
AIRM =
NCON =
CSIG =
SVRL =
WSTM =
WOU =
WCN =
```
Where attrib.xmp contains:

#CLASS_SYMBOLS -99 -99 3 3 0 2.0

#CLASS_SYMBOLS SPSYM_ELM 12 0 0 0 5.0

which means set all symbols to GEMPAK color 3 and size 2.0 except for

symbol type SPSYM_ELM, sub-type 12 (High symbol) which will retain

default color and be size 5.

### Error Messages

```
[GPMAP-1] Fatal error initializing TAE.

[GPMAP-2] Fatal error reading TAE parameters.

[GPMAP-3] Fatal error initializing GEMPLT.
```

## 4.42 GPNEXR2

GPNEXR2 displays NEXRAD level II products.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
RADFIL Radar image filename(s)
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
LUTFIL Enhancement lookup table filename
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
TILT Radar beam elevation/tilt angle
RADPARM Radar parameter (dz, vr, sw)
RADTIM Radar composite current/dattim
```


GPNEXR2 displays NEXRAD Level II products in ARCHIVE2 format.

RADFIL specifies the input level II file. A template may be specified, with an optional

site name (eg NEXRII|KDDC). If a site name is provided, it will be used to replace the

%SITE% alias in the template name. The NEXRII template is provided for

ARCHIVE2 format files.

RADPARM is the Radar parameter to be displayed. Valid values are dz (reflectivity),

vr (radial velocity), sw (spectrum width).

TILT is the Radar beam elevation/tilt angle. TILT is a real number that specifies the

mean sweep evelation that is closest to the specified value. If TILT is LIST, then the

available levels will be displayed and the user will be prompted for a value.

RADTIM is a valid GEMPAK date/time string or abbreviation. A time range may be

specified.

LUTFIL specifies the color enhancement table.

## Examples

1. Display the most recent reflectivity for KDDC using the sweep with mean

elevation angle is closest to .5 degrees.

#MAP = 6/1/1+3/1/2


```
GAREA = dset
PROJ = rad
RADFIL = NEXRII|KDDC
LATLON = 0
PANEL = 0
TITLE = 31
TEXT = 1
CLEAR = YES
DEVICE = xw
LUTFIL = default
IMCBAR = 31/V/LL/.005;.05/.85;.01|.7/1/1/hw
TILT = .5
RADPARM = dz
RADTIM = last
```
2. Create an animation sequence for KDDC using data from 2330Z

yesterday (July 29th) to 0030Z today (July 30th).

```
GAREA = dset
PROJ = rad
RADFIL = NEXRII|KDDC
LATLON = 0
PANEL = 0
TITLE = 31
TEXT = 1
CLEAR = YES
DEVICE = xw
LUTFIL = default
IMCBAR = 31/V/LL/.005;.05/.85;.01|.7/1/1/hw
TILT = .5
RADPARM = dz
RADTIM = 29/2330-30/0030
```
### Error Messages

```
[GPNEXR2-1] Fatal error initializing TAE.

[GPNEXR2-2] Fatal error reading TAE parameters.

[GPNEXR2-3] Fatal error initializing GEMPLT.
```

## 4.43 GPNIDS

GPNIDS plots NEXRAD Level III products.



```
RADFIL Radar image filename(s)
RADTIM Radar composite current/dattim
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
CLEAR Clear screen flag
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
COLORS Color list
WIND Wind symbol/siz/wdth/typ/hdsz
LINE Color/type/width/label/smth/fltr
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
GAREA Graphics area
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
OUTPUT Output device/filename
```


GPNIDS plots NEXRAD Level III products.

RADFIL is the NEXRAD Level III input file. A template may be specified, with an

optional site name and product type (eg NEXRIII|DDC|NWV). If a site name is pro-

vided, it will be used to replace the %SITE% alias in the template name. If a product

type is provided, it will be used to replace the %PROD% alias in the template string if

present. Products will br plotted in graph or map coordinates depending on the type of

product. For VAD wind profile products (NVW) the display is graph coordinates,

while raster and radial images, and overlay products (eg mesocyclone, storm track, tvs

etc) are displayed in map coordinates. The NEXRIII template is provided for

NEXRAD Level III files.

RADTIM is a valid GEMPAK date/time string or abbreviation. A time range may be

specified.

CLRBAR specifies the attributes used to plot the RMS ranges of the VAD winds.

IMCBAR displays the coresponding image calibration bar for raster and radial prod-

ucts.

WIND can be used to select the type and plot size of barbs or arrows for the vectors in

the VAD display.


TEXT can be used to control the attributes used to plot the text symbols contained in

the NIDS product file.

COLORS provides the color used for the TEXT symbols. Colors for the axes and wind

vectors for VAD products (NVW) are specified in the gpvad.config file. If the file does

not exist, a default set of colors will be used. The config file will be searched for in the

standard locations, including the current working directory in order to allow the user to

tailor colors for the individual application. $GEMTBL/unidata/gpvad.config is provid-

ed.

LINE is used to control the attributes of lines displayed in the products.

OUTPUT is used to direct display of the tablular and alphanumeric portions of the

NIDS products.

## Examples

1. Plot the VAD profile for radar location FFC using the NEXRIII template

for the most recent time. Use wind barbs, and plot the RMS bar along

the altitude axis.

#RADFIL = NEXRIII|FFC|NVW

```
RADTIM = last
WIND = bk1
TITLE = 1/-2/VAD DISPLAY ~
PANEL = 0
DEVICE = XW
CLEAR = y
TEXT = 1/1/1/hw
CLRBAR = 1/v/cl/.05;.5/.3;.01
MAP = 0
```
2. Plot an animated VAD profile series for radar location FFC using the

NEXRIII template and specifying the time range from 21Z to 23Z today.

#RADFIL = NEXRIII|FFC|NVW

#RADTIM = 2200-2300

```
WIND = bk1
TITLE = 1/-2/VAD DISPLAY ~
PANEL = 0
DEVICE = XW
CLEAR = y
TEXT = 1/1/1/hw
CLRBAR = 1/v/cl/.05;.5/.3;.01
MAP = 0
```
3. Plot the Composite Reflectivity product for SJT and display the text

storm attribute table to the terminal.


#RADFIL = NEXRIII|SJT|NCR

```
RADTIM = last
WIND =
TITLE = 1/-2
PANEL = 0
DEVICE = xw
CLEAR = Y
TEXT = 1/2/2/hw
CLRBAR =
IMCBAR = 5/v/LL/.005;.6/.4;.01
OUTPUT = t
GAREA = dset
COLORS = 7
MAP = 6
LATLON = 0
LINE =
```
4. Overlay the Storm Tracking Information product for SJT on the

composite reflectivity display in #3.

#RADFIL = NEXRIII|SJT|STI

```
RADTIM = last
TITLE = 1/-2
PANEL = 0
DEVICE = xw
CLEAR = n
TEXT = 1/2/2/hw
COLORS = 7
LINE = 31/1/4
WIND =
CLRBAR =
IMCBAR = 5/v/LL/.005;.6/.4;.01
GAREA = dset
MAP = 6
LATLON = 0
OUTPUT = t
```
### Error Messages

```
[GPNIDS+2] NEXRIII template not found using ...

[GPNIDS-1] Fatal error initializing TAE.

[GPNIDS-2] Fatal error reading TAE parameters.

[GPNIDS-3] GEMPLT initialization error.
```

## 4.44 GPRCM

GPRCM is a version of GPMAP that plots Radar Coded Message (RCM) bulletins.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
RCMFIL RCM data file
RADTIM Radar composite current/dattim
RADDUR Radar time window (minutes prior to RADTIM)
DITHER dither level
MESO meso color/marker/size/width/hw;filter
TVS tvs color/marker/size/width/hw;filter
CNTR Centroid barb color/size/width;filter
MAXTOP Maxtop color;filter
RADINFO Radar status color
ECHO Plot radar echoes (0,1,2)
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
LUTFIL Enhancement lookup table filename
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
OUTPUT Output device/filename
```


GPRCM is derived from the standard GEMPAK program GPMAP. The behavior is

similar to gpmap, with the addition of reading Radar Coded Message (RCM) bulletins

and optionally plotting radar echoes and/or annotations from the remarks included

within the RCM bulletin.

RCM bulletins contain digitized reflectivity values derived from the 2.2 x 2.2 nmi

NIDS Composite reflectivity product over the radar area of coverage out to 248 nmi

based on grid squares approximately 5.4 nmi on a side. The reflectivity intensity value

for each grid box is determined by assigning the maximum value of all Composite Re-

flectivity boxes whose centers are contained within the grid square. The RCM message

comprises 9 intesity categories as follows:


```
Code Color Plotted
0ND
1 15-30 dBZ 23
2 30-40 dBZ 22
3 40-45 dBZ 21
4 45-50 dBZ 20
5 50-55 dBZ 17
6 >= 55 dBZ 15
7 data beyond 124 nmi that are above threshold
8 data beyond 124 nmi that are below threshold
```
RCM data within a user specified time range are plotted.

RCMFIL is the RCM data file. Bulletins with radar observation times in the RCM file

will be plotted if the observation time is within the range specified by the starting and

ending times.

RADTIM (current or dattim) specifies the ending time for accepted data. RADDUR is

the number of minutes prior to the ending time to begin accepting data.

DITHER is used to specify the plotting behavior of the reflectivity intensities. A value

of 0 specifies no dithering, the grid box is completely filled. Values 1-4 increase the

number of dither points per grid box, approaching an increasingly opaque value. Dither

can be used to overlay image data with radar reflectivity in order to simulate transpar-

ency. A dither value >= 5 will outline the grid box.

MESO specifies the color/marker/size/width/hw for mesocyclones to be marked. A

color value of 0 is used to turn off mesocyclone plotting. Marker attributes behave as

MARKER for other plotting programs.

TVS specifies the color/marker/size/width/hw for tornado vortex signatures to be

marked. A color value of 0 is used to turn off TVS plotting.

CNTR specifies the color/size/width for Cell Centroid movement barbs (knots). A val-

ue of 0 is used to turn off centroid plotting.

MAXTOP specifies the color for cell top annotations (feet). A value of 0 is used to turn

off cell top annotations.

RADINFO specifies the color for radar site operational status annotations. NA, data

not available; OM, out for maintenance; NE, no echoes.

ECHO specifies whether to plot the grid box intesities. A value of 0 specifies no plot-

ting of echoes; 1 specifies plotting only those echoes from radars in precipitation mode;

2 specifies plotting of echoes from radars in both precipitation and clear air mode.


RCM summary information is directed to OUTPUT. Summary information includes

the station ID, reorting time, operational mode, and mesocyclone and tornado vortex

signature locations (if present).

GPRCM utilizes the nexrad.tbl station table to determine the radar location (not all

RCM bulletins contain radar location as of Nov. 1999). The radars listed in the station

table are also used to determine which locations are missing (Not Available: NA).

## Examples

1. Draw a Lambert Conformal map of the US. Display RCM data from the

file rcm.dat. Plot radar observations for the past 60 minutes. Display the

echo intensities (no dithering), maxtop values in yellow, and radar status

in red. Do not plot mesocyclone, or TVS locations. Plot the cell

centroid motion barbs in white. Use echoes from radar sites operating in

precipitation mode only. Use the default CLRBAR positioning.

```
GAREA = us
PROJ = str/90;-105;0
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1/Radar Summary
TEXT = 1.3/22/1/hw
CLEAR = yes
DEVICE = xw
RCMFIL = rcm.dat
RADTIM = current
RADDUR = 60
DITHER = 0
MESO = 0
TVS = 0
CNTR = 1/1/1
MAXTOP = 5
RADINFO = 2
ECHO = 1
CLRBAR = 1
LUTFIL =
IMCBAR =
OUTPUT = T
```
2. Overlay the IR satellite imagery with RCM data for the US west coast.

Use a DITHER value of 2 to for moderate transparency. Plot the radar

CLRBAR horizontally above title. Satellite CLRBAR will be vertical

along the left side of the image (default location from IMCBAR).

```
GAREA = ca-
PROJ = sat
```

```
SATFIL = $SAT/GOES-10/4km/IR/IR_991108_1600
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1/Satellite and Radar Composite
TEXT = 1/22/1/hw
CLEAR = yes
DEVICE = xw
RCMFIL = rcm.dat
RADTIM = current
RADDUR = 60
DITHER = 2
MESO = 0
TVS = 0
CNTR = 0
MAXTOP = 0
RADINFO = 0
ECHO = 1
CLRBAR = 1/h/cc/.46;.06
LUTFIL = default
IMCBAR = 1
OUTPUT = T
```
3. Display the current RCM data using the RCMDAT file template and

allow searching back 90 minutes. Do not filter TVS plotting, use a filter

of .3 for MESO plotting, a filter of .5 for CNTR, and 1.0 (aka YES) for

MAXTOP plotting. Send summary output to the terminal as well as to a

file named rcm.summary.

#MAP = 8

```
GAREA = us
PROJ = str/90;-105;0
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1/Radar Summary
TEXT = .9/22/1/hw
CLEAR = YES
DEVICE = xw
RCMFIL = rcmdat
RADTIM = current
RADDUR = 90
DITHER = 0
MESO = 2/1/1/1/hw;.3
TVS = 2/2/1/2/hw;n
CNTR = 1/.7/1;.5
MAXTOP = 6;y
RADINFO = 5
ECHO = 1
CLRBAR = 0
LUTFIL =
IMCBAR =
```

```
OUTPUT = tf/rcm.summary
```
### Error Messages

```
[GPRCM-1] Fatal error initializing TAE.

[GPRCM-2] Fatal error reading TAE parameters.

[GPRCM-3] Fatal error initializing GEMPLT.
```

## 4.45 GPTCWW

GPTCWW generates the track error watch/warn plot for the TPC, using breakpoint in-

formation contained in a VGF file.



```
STRMID Storm id / Advisory num / Device / Num fcst days / Text flag
```


GPTCWW will plot the track error watch/warn plot for each tropical system specified.

The system(s) to be plotted may be specified in one of two ways:

(1) A storm identifier and advisory number may be explicitly

```
specified using the STRMID parameter.
```
(2) Storm identifiers and advisory numbers may be gotten by

```
checking the storm history file against the contents of
the storm advisory directory. Only the latest advisory
for each storm in the directory (which is not found in the
history file) will be processed. This method of running is
specified by leaving the storm id and/or advisory number
blank in the STRMID parameter, and is the default mode.
See the help for STRMID for further details.
```
The track error watch/warn plot displays the current location and the forecasted track

for the tropical system. It also shows the track error envelope. Active hurricane watch-

es and warnings and tropical storm watches and warnings, if any, will be displayed.

The program can generate XW, GIF, or PostScript files, depending on the value of the

third field of the STRMID parameter. The default is GIF files. See the help for STR-

MID for further details.

The program will display either a 3 day or a 5 day forecast, depending on the value of

the fourth field of the STRMID parameter. See the help for STRMID for further details.

(GPTCWW does not use the fifth field of the STRMID parameter.)

## Examples

1. Use the history file to determine which new advisories are to be plotted.

GIF files will be created, preceded by an XW display of the track error

watch/warn file. A 5 day forecast is displayed.

#STRMID =


2. Plot the graphic for advisory number 8 of storm al892001. Display the

graphic in XW only. A 5 day forecast is displayed.

```
STRMID = al892001/8/xw
```
3. Plot the graphic for advisory number 1 of storm al882001 in an XW

window. Processing is automatic unless breakpoints are to be entered. A

3 day forecast is displayed.

```
STRMID = al882001/1/xw/3
```
4. Plot the graphic for advisory number 20 of storm ep1900 (which could

also have been specified as ep192000). A GIF file will be created,

preceded by an XW display of the track error watch/warn file. A 5 day

forecast is displayed.

```
STRMID = ep1900/20/gf
```
### Error Messages

```
[GPTCWW4] Error in opening breakpoint VGF file.

[GPTCWW3] Too few points in track error table.

[GPTCWW2] Cannot change ... in color table.

[GPTCWW1] No new storms to process.

[GPTCWW-1] Fatal error initializing TAE.

[GPTCWW-2] Fatal error reading TAE parameters.

[GPTCWW-3] Error in reading advisory.

[GPTCWW-5] Error in opening history_tc file.

[GPTCWW-6] No advisories found in directory.

[GPTCWW-7] Error in opening breakpoint file.

[GPTCWW-8] Error in opening average track error table.

[GPTCWW-9] Error in opening breakpoint station plotting table.

[GPTCWW-10] Invalid storm identifier in advisory message.

[GPTCWW-12] Error in opening file name table.
```

## 4.46 GPTEXT

GPTEXT draws the contents of a text file to the output device.



```
PANEL Panel loc/color/dash/width/regn
COLORS Color list
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
TXTFIL Text filename or LOGO|size|mode
TXTLOC Text location
COLUMN Number of columns
```


GPTEXT draws text read from an ASCII text file to the output device driver. If the in-

put file is LOGO, the NOAA logo will be plotted. The logo can be plotted in either full

color or monochrome. The NOAA logo is also plotted if "NOAA" is entered for the

file name. The NWS logo is plotted if "NWS" is entered.

The text location TXTLOC is given as the X and Y values in the Normalized coordinate

system or Map coordinate system if a '#' is prefixed before the X and Y values. To use

map coordinates, the map projection must be defined by another GEMPAK application

such as GPMAP.

COLUMN is the number of columns the text will be divided into within a panel.

## Examples

1. Draw the contents of the text file mytext.txt to the XW device. Display

the text at the location .1;.8. Display in a single column.

#PANEL = 0

#COLORS = 1

#TEXT = 1

```
CLEAR = yes
DEVICE = xw
TXTFIL = mytext.txt
TXTLOC = .1;.8
COLUMN = 1
```

### Error Messages

```
[GPTEXT-1] Fatal error initializing TAE.

[GPTEXT-2] Fatal error reading TAE parameters.

[GPTEXT-3] Fatal error initializing GEMPLT.

[GPTEXT-4] File ... does not exist.

[GPTEXT-5] Map projection is not set.
```

## 4.47 GPTPC

GPTPC generates four hurricane graphics for the TPC. They are:

```
(1) Wind swath plot
(2) Strike probability plot (Atlantic storms only)
(3) Wind intensity graph
(4) Wind speed table
```


```
STRMID Storm id / Advisory num / Device / Num fcst days / Text flag
```


GPTPC will plot up to four graphics for each tropical system specified. The system(s)

to be plotted may be specified in one of two ways:

1) A storm identifier and advisory number may be explicitly

```
specified using the STRMID parameter.
```
2) Storm identifiers and advisory numbers may be gotten by

```
checking the storm history file against the contents of
the storm advisory directory. Only the latest advisory
for each storm in the directory (which is not found in the
history file) will be processed. This method of running is
specified by leaving the storm id and/or advisory number
blank in the STRMID parameter, and is the default mode.
See the help for STRMID for further details.
```
The wind swath plot shows the cumulative radius of the 34 knot (tropical storm force)

and the 64 knot (hurricane force) winds over the life of the storm.

The strike probability plot displays contours of the strike probability percentage using

ranges of 10 to 19 percent, 20 to 49 percent, 50 to 99 percent, and 100 percent.

The wind intensity graph shows maximum one-minute wind speed forecast and proba-

bilities in miles per hour vs. forecast time, out to 72 hours. Category information is

also displayed.

The wind speed table shows the wind speed forecast expressed as a percentage proba-

bility of reaching a given strength and category, out to 72 hours.

The program can generate XW, GIF, or PostScript files, depending on the value of the

third field of the STRMID parameter. The default is GIF files. See the help for STR-

MID for further details. (GPTPC does not use the fourth or fifth fields of the STRMID

parameter.)


## Examples

1. Use the history file to determine which new advisories are to be plotted.

GIF files will be created.

#STRMID =

2. Plot the graphics for advisory number 8 of storm al892001. Display the

graphics in XW only, prompting the user to select which plots will be

displayed and when to move on to the next plot.

```
STRMID = al892001/8/test
```
3. Plot the graphics for advisory number 1 of storm al882001. Each of the

four graphics will be displayed in its own XW window (the windows are

tiled and may be raised as desired). Processing is automatic.

```
STRMID = al882001/1/xw
```
4. Plot the graphics for advisory number 20 of storm ep1900 (which could

also have been specified as ep192000). GIF files will be created.

```
STRMID = ep1900/20/gf
```
### Error Messages

```
[GPTPC2] Cannot change ... in color table.

[GPTPC1] No new storms to process.

[GPTPC-1] Fatal error initializing TAE.

[GPTPC-2] Fatal error reading TAE parameters.

[GPTPC-3] Error in reading advisory.

[GPTPC-4] Error in reading strike probability grid.

[GPTPC-5] Error in opening history file.

[GPTPC-6] No advisories found in directory.

[GPTPC-9] Error in contour fill.

[GPTPC-10] Error in coordinates for wind intensity plot.

[GPTPC-11] Error in opening intensity probability table.

[GPTPC-12] Error in opening file name table.

[GPTPC-13] Could not retrieve local date/time.
```

## 4.48 GPTRAJ

GPTRAJ computes trajectories for gridded data files.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
LUTFIL Enhancement lookup table filename
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
GDFILE Grid file
GVECT Vector grid
GPOINT Grid point
GLEVEL Grid level
GVCORD Grid vertical coordinate
GDATTIM Grid date/time
TSTEP Time step (minutes)
MARKER Marker color/type/size/width/hw
LINE Color/type/width/label/smth/fltr
RTRAJ Back/Reverse trajectory flag
```


GPTRAJ computes horizontal trajectories from user supplied vector functions on con-

stant grid level/coordinates.

Trajectories are computed using a user specified GPOINT as the starting or ending lo-

cation (see RTRAJ). The advected GPOINT location is determined by interpolating the

gridded vector components to the trajectory location using the nearest data time. The

trajectory location is updated iteratively through the forecast times using TSTEP over

the time range specified in GDATTIM.

TSTEP is the time step of integration (in minutes).

GPOINT may be specified using the traditional methods for denoting a single point, or

may be intered as a semicolon separated lat/lon pair of min/max/increment.

MARKER is used to define the marker characteristics used to locate the trajectory point

at times matching the available grid fields.

LINE is used to define the line type which connects the computed trajectory locations.


RTRAJ is a logical flag to specify whether a reverse/back trajectory will be computed.

If RTRAJ is NO, a forward trajectory is computed. If RTRAJ is YES, a backwards tra-

jectory will be computed from GPOINT.

The direction of the last data point is marked with a directional arrow.

## Examples

1. Draw a Lambert Conformal map of the US. Using the most recent ETA

model output, display a trajectory starting at DEN using the 10m wind for

the time range F024-F060.

#MAP = 1

```
GAREA = uslcc
PROJ = lcc
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1/Trajectory from DEN
TEXT = 1/22/1/hw
CLEAR = yes
DEVICE = xw
GDFILE = eta
GVECT = wnd
GPOINT = den
GLEVEL = 10
GVCORD = hght
GDATTIM = f024-f060
MARKER = 6/15/6/1
LINE = 7
RTRAJ = no
TSTEP = 1
```
2. Repeat the map above using all forecast times in the ETA model run for

a back trajectory.

#MAP = 1

```
GAREA = uslcc
PROJ = lcc
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1/Backward Trajectory for DEN
TEXT = 1/22/1/hw
CLEAR = yes
DEVICE = xw
GDFILE = eta
```

```
GVECT = wnd
GPOINT = den
GLEVEL = 10
GVCORD = hght
GDATTIM = fall
MARKER = 6/15/6/1
LINE = 7
RTRAJ = yes
TSTEP = 1
```
3. Display all 700mb backward trajectories for points ending at lat/lon pairs

every 10 degrees across the domain using all forecast times in the ETA

model run. Set the time step to 30 minutes.

#MAP = 1

```
GAREA = uslcc
PROJ = lcc
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 1/-1
TEXT = 1/22/1/hw
CLEAR = yes
DEVICE = xw
GDFILE = eta
GVECT = wnd
GPOINT = 30/50/10;-120/-70/10
GLEVEL = 700
GVCORD = pres
GDATTIM = fall
MARKER = 6/15/6/1
LINE = 7
RTRAJ = yes
TSTEP = 30
```
### Error Messages

```
[GPTRAJ-1] Fatal error initializing TAE.

[GPTRAJ-2] Fatal error reading TAE parameters.

[GPTRAJ-3] Fatal error initializing GEMPLT.
```

## 4.49 GPVAD

GPVAD plots the NEXRAD Level III VAD wind profile.



```
RADFIL Radar image filename(s)
RADTIM Radar composite current/dattim
WIND Wind symbol/siz/wdth/typ/hdsz
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
CLEAR Clear screen flag
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
```


GPVAD plots the NEXRAD Level III wind profile product.

RADFIL is the NEXRAD Level III input file. A template may be specified, with an

optional site name and product type (eg NEXRIII|DDC|NWV). If a site name is pro-

vided, it will be used to replace the %SITE% alias in the template name. If a product

type is provided, it will be used to replace the %PROD% alias in the template string if

present. NVW is the only valid VAD product identifier at present. The NEXRIII tem-

plate is provided for NEXRAD Level III files.

RADTIM is a valid GEMPAK date/time string or abbreviation. A time range may be

specified.

CLRBAR specifies the attributes used to plot the RMS ranges of the VAD winds.

WIND can be used to select the type and plot size of barbs or arrows for the vectors in

the VAD display.

TEXT can be used to control the attributes used to plot the text symbols contained in

the VAD product file.

Colors for the axes and wind vectors drawn by GPVAD are specified in the gpvad.con-

fig file. If the file does not exist, a default set of colors will be used. The config file

will be searched for in the standard locations, including the current working directory

in order to allow the user to tailor colors for the individual application. $GEMTBL/

unidata/gpvad.config is provided.


## Examples

1. Plot the VAD profile for radar location FFC using the NEXRIII template

for the most recent time. Use wind barbs, and plot the RMS bar along

the altitude axis.

#RADFIL = NEXRIII|FFC|NVW

```
RADTIM = last
WIND = bk1
TITLE = 1/-2/VAD DISPLAY ~
PANEL = 0
DEVICE = XW
CLEAR = y
TEXT = 1/1/1/hw
CLRBAR = 1/v/cl/.05;.5/.3;.01
```
2. Plot an animated VAD profile series for radar location FFC using the

NEXRIII template and specifying the time range from 21Z to 23Z today.

#RADFIL = NEXRIII|FFC|NVW

#RADTIM = 2200-2300

```
WIND = bk1
TITLE = 1/-2/VAD DISPLAY ~
PANEL = 0
DEVICE = XW
CLEAR = y
TEXT = 1/1/1/hw
CLRBAR = 1/v/cl/.05;.5/.3;.01
```
### Error Messages

```
[GPVAD+2] NEXRIII template not found using ...

[GPVAD-1] Fatal error initializing TAE.

[GPVAD-2] Fatal error reading TAE parameters.

[GPVAD-3] GEMPLT initialization error.
```

## 4.50 GPWARN

GPWARN is a version of GPMAP that plots filled county/zone regions from reports

which use the univeral generic county/zone identifier lines.



```
MAP Map color/dash/width/filter flag
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
LATLON Line color/dash/width/freq/inc/label/format
PANEL Panel loc/color/dash/width/regn
TITLE Title color/line/title
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
SVRL End time|SVRL clrs|Tm|Lb|Outline|Clr
LUTFIL Enhancement lookup table filename
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
OUTPUT Output device/filename
```


GPWARN is derived from the standard GEMPAK program GPMAP. The behaviour

is similar to gpmap, with the addition of plotting filled county/zones defined in a UGC

specifier with the map area specified. Satellite and radar data may be used as back-

grounds.

NWSTG Products which use the UGC identifier line include:

#PIL

```
Tornado Warnings TOR
Severe Thunderstorm Warnings SVR
Flood Watches FLA|FFA
Flood Warnings FLW|FFW
Winter Weather Bulletins WSW
Non-precipitation Bulletins NPW
Special Weather Statements SPS
```
GPWARN finds the UGC line following the pil line in the bulletin which looks like:

ALC129-131-MSC041-046>052-281030-

The program determines the counties/zone which are included in the bulletin, along

with the expiration time. The map information for the county or zone to be plotted is

read from the $GEMTBL/bounds files. Each county is filled and outlined with a color

code based on bulletin type and expiration time.

The following gempak color codes are used:


```
Type Current Expired
fill/outline fill/outline
Tornado 2/5 13/10
Tstorm 14/5 13/10
Flood Watch 21/5 19/18
Flood Warn 23/5 19/18
Winter 4/5 26/10
Non-precip 7/5 29/10
Special 6/5 24/26
```
The bulletins are read from the text bulletins which are also used with NWX. These

bulletins are stored under the $TEXT_DATA tree under watch_warn/torn_warn,

watch_warn/tstrm_warn, watch_warn/winter, watch_warn/noprcp, watch_warn/spe-

cial, fflood/watch, fflood/warn.

The program allows the user to specify an input file list using the WWFIL parameter.

Multiple file names or template aliases may be entered by separating each entry with a

semicolor (;). File templates should be defined in $GEMTBL/config/datatype.tbl.

GPWARN will search for the files by checking for a template, and then using the file

name given, as well as relative to $TEXT_DATA, eg.:

WWFIL = tstrmwarn WWFIL = $TEXT_DATA/watch_warn/tstrm_warn/

1997012800.warn WWFIL = watch_warn/tstrm_warn/1997012800.warn WWFIL = /

fullpath/nwx/watch_warn/tstrm_warn/1997012800.warn

each of the above entries are valid.

The time for the plot may be spacified as current or a valid GEMPAK format YYM-

MDD/HHMM time using the parameter WWS, eg:

WWS = cur WWS = 970128/0000

GPWARN will use the time in WWS to determine if a bulletin has expired by compar-

ing against the time on the UGC line.

The time range for plotted bulletins is set using the WWATT parameter. This param-

eter allows the user to specify how long after the bulletin has expired that it will still be

plotted, and how far in the future bulletins will be viewed. The format is WWATT =

min_prior/min_future, where min_prior and min_future are integers in minutes. No

display will occure for bulletins which have expired greater than min_prior minutes be-

fore the time in WWS.

For bulletins which have not yet expired, min_future will determine if the bulletin is to

be plotted. This feature is useful when plotting historical data, where products exist at

later times than were available at the time which the plot represents, or in the case of


advisories such as winter storm bulletins which may be issued well in advance of the

expected conditions.

If min_future is negative (eg -60), then no display will occur for bulletins which expire

greater than 60 minutes in the future. If min_future is positive (eg 60), then bulletins

that expire greater than 60 minutes in the future will be displayed in white, so that they

are not confused with those regions which are more immenent.

For product generation scripts, you may which to cat each of the desired file types to be

plotted into a single input file, otherwise, you may run the program a sperate time for

each input file desired.

The default it WWATT is blank is 1440/1440. If only one value is specified, it is used

for both.

In addition to plotting warnings, GPWARN also provides the option of plotting coun-

ties included within watched using the SVRL parameter as used in GPMAP. The files

for SVRL are generated by the dcsvrl decoder.

Color and line types for the areas drawn by GPWARN are specified in the gpwarn.con-

fig file. If the file does not exist, a default set of colors will be used. The config file

will be searched for in the standard locations, including the current working directory

in order to allow the user to tailor colors for the individual application.

## Examples

1. Draw a Lambert Conformal map of the US. Display current tornado,

severe thunderstorm, flood, winter weather and non-precipitation warnings.

Show counties/zones for up to 120 minutes after tha warning has expired.

Begin to show warnings 1440 minutes before they take affect.

```
GAREA = uslcc
PROJ = lcc
SATFIL =
RADFIL =
LATLON = 0
PANEL = 0
TITLE = 2/-2/Current Warnings
TEXT = 1.3/22/1/hw
CLEAR = yes
DEVICE = xw
SVRL =
WWFIL = nonpcpwarn;winterwarn;floodwarn;tstormwarn;tornadowarn
WWS = current
WWATT = 120/1400
OUTPUT = t
```

### Error Messages

```
[GPWARN-1] Fatal error initializing TAE.

[GPWARN-2] Fatal error reading TAE parameters.

[GPWARN-3] Fatal error initializing GEMPLT.
```

## 4.51 GRPHGD

GRPHGD generates a grid from contours drawn via NMAP product generation.



```
GDOUTF Output grid file
PROJ Map projection/angles/margins|drop flag
GRDAREA Area covered by grid
KXKY Number of grid points in x;y
MAXGRD Maximum number of grids
CPYFIL Grid file whose navigation is to be used in new grid file |
ANLYSS Grid analysis block
CNTRFL Contour file
GDATTIM Grid date/time
GFUNC Scalar grid
GLEVEL Grid level
GVCORD Grid vertical coordinate
KEYCOL Key color
GGLIMS Grid value limitations and control
HISTGRD Toggle writing history grid (y/n)
BOUNDS BOUNDS name|<key>key_name|mask_flag
```


GRPHGD generates a grid from contours drawn via NMAP product generation. A two-

step process is involved:

1) Contours must be drawn via NMAP and the coordinates saved, 2) The saved coordi-

nates are passed into GRPHGD for

```
conversion into gridded data.
```
NMAP Procedure

To generate contours, invoke NMAP and enter product generation. Contours are de-

fined as a simple solid line grouped with a numeric text value using 'LABEL' grouping.

The label may be located anywhere. Any number of equal-valued labels may be

grouped with any number of lines. When drawing, labeling and grouping have been

completed, a text file containing the geographical contour coordinates must be created.

Click 'PROD' then 'GRAPH GRID'. A popup will appear with a listing of all properly

grouped contours and their associated value and coordinates. Click 'SAVE' to save this

text to the ASCII file CNTRFL. This is the file which will be passed into GRPHGD.

Note that NMAP now invokes the GRPHGD program directly when the user selects

"MAKE GRID".

GRPHGD Procedure


To generate a gridded field from contour data, use the program GRPHGD. The follow-

ing parameters are used:

```
GDOUTF = Output grid file
PROJ = Map projection/angles/margins|dr
GRDAREA = Area covered by grid
KXKY = Number of grid points in x;y
MAXGRD = Maximum number of grids
CPYFIL = File to be copied
ANLYSS = Grid analysis block
CNTRFL = Contour file (ASCII data generated via NMAP)
GDATTIM = Grid date/time
GFUNC = GEMPAK parameter name
GLEVEL = GEMPAK vertical level value
GVCORD = GEMPAK vertical coordinate
KEYCOL = Key color
BOUNDS = Bound area(s) to exclude or include
```
Several parameters (PROJ, GRDAREA, KXKY, MAXGRD, CPYFIL, ANLYSS) are

intended to be used identically as in the programs GDCFIL and NAGRIB. These define

the grid (and gridfile) to create the data. The grid is written to file GDOUTF with pa-

rameter name GFUNC valid at GDATTIM. GLEVEL and GVCORD may be assigned

accordingly. KEYCOL indicates which lines actually get processed; =0 processes all

lines, =n processes only lines with color n. BOUNDS determines whether to assign

missing values to gridpoints inside (BOUNDS=...|...|false) or outside

(BOUNDS=...|...|true) a bounded area. Up to ten bounds areas may be specified by sep-

arating with '+'.

GRPHGD loosely follows an algorithm originally described in TDL OFFICE NOTE

92-14 "The Systematic Interpolative Radial Search (SIRS) -- A Method to Compute

Gridpoint Values from Contours". Some changes were incorporated to improve data

consistency, to increase execution efficiency and for added capability:

The program performs several methodologies to assign values at gridpoints: 1) bounds

check w/ appropriate missing value assignments, 2) exact value assignment when a

contour lies directly on a gridpoint, 3) radial search/assignment for grid points between

different valued contours, 4) grid points within closed contours, with and without rela-

tive minima/maxima, 5) grid points surrounded by like-valued contours, without rela-

tive minima/maxima, 6) a weighted search and assignment where neither 1) thru 5)

could be applied. A 9-point smoother is applied as a final step.

## Examples

1. Create a grid from the NMAP ASCII file qpf.info. Use the same grid as

found in the RUC2 model. Identify the grid as 'prcp' valid at 980903/

1200.

```
GDOUTF = qpf.98090312
```

#PROJ =

#GRDAREA =

#KXKY =

#MAXGRD = 10

```
CPYFIL = $MODEL/ruc2/ruc2_98090312
ANLYSS =
CNTRFL = qpf.info
GDATTIM = 980903/1200
GFUNC = prcp
```
2. Same as example 1, except assign missing values outside the SSA

bounded area HPC050, i.e., assign values only to those grid points inside

the bounds area.

```
GDOUTF = qpf.98090312
PROJ =
GRDAREA =
KXKY =
MAXGRD = 10
CPYFIL = $MODEL/ruc2/ruc2_98090312
ANLYSS =
CNTRFL = qpf.info
GDATTIM = 980903/1200
GFUNC = prcp
BOUNDS = SSA|<AREA>HPC050|true
```

### Error Messages

```
[GRPHGD+4] WARNING. Maximum number of lines exceeded.

[GRPHGD+3] WARNING. Maximum number of intersections exceeded.

[GRPHGD+2] WARNING. This grid dimension is too large.

[GRPHGD+1] WARNING. This grid is too large.

[GRPHGD-1] Fatal error initializing TAE.

[GRPHGD-2] Fatal error reading TAE parameters.

[GRPHGD-3] Fatal error initializing GEMPLT.

[GRPHGD-4] Navigation information is invalid.

[GRPHGD-5] Grid area ... is invalid.

[GRPHGD-6] Grid size is invalid.

[GRPHGD-7] The grid file name may not be blank.

[GRPHGD-8] Cannot create file ....

[GRPHGD-9] Grid name ... cannot be found in grid table.

[GRPHGD-10] Incompatible grid navigation in ....

[GRPHGD-11] Cannot open file ....
```

## 4.52 NAGRIB

NAGRIB converts gridded data in GRIB files to GEMPAK gridded data.



```
GBFILE GRIB data file name
INDXFL GRIB index file name
GDOUTF Output grid file
PROJ Map projection/angles/margins|drop flag
GRDAREA Area covered by grid
KXKY Number of grid points in x;y
MAXGRD Maximum number of grids
CPYFIL Grid file whose navigation is to be used in new grid file |
GAREA Graphics area
OUTPUT Output device/filename
GBTBLS Input GRIB decoding tables
GBDIAG GRIB diagnostic elements
PDSEXT PDSEXT Y or N, add PDS extension if found
```


NAGRIB will convert gridded data which is in GRIB files to gridded data in a GEM-

PAK file.

The GRIB input file is specified in GBFILE, and the index file associated with the

GRIB file is in INDXFL.

The GEMPAK output file is given in GDOUTF. This file will be opened if it exists and

the projection information matches the GRIB message projection. If the file does not

exist, it will be created using information given by the user. If the output filename is

"LIST", the GRIB file will be scanned and the decoded GEMPAK header information

will be printed out.

The navigation information given by the user may take several forms. The first possi-

bility is for the user to enter CPYFIL=#NNN, where NNN is a grid projection number

listed in GRDNAV.TBL. Second, the user may enter CPYFIL=GDS. This will read

the navigation from the Grid Definition Section of the first GRIB message. Third, the

user could enter CPYFIL=OLDFILE, where OLDFILE is the name of an existing

GEMPAK file from which to copy the navigation information. Last, the user may

choose to use the PROJ, GRDAREA, and KXKY parameters to specify the navigation.

GAREA is used to subset the grid. It may be specified as any valid input for GAREA

which are equivalent to lat/lon bounds or as a single @ followed by the lower-left and

upper-right grid point coordinates separated by semicolons. In the former case, the lo-

cations are rounded to the nearest grid points. The projection type and angles entered

as described above along with the lat/lon coordinates of the corners of the subset define

a new grid navigation different from that of the full grid. The navigation entered as de-


scribed in the preceding paragraph is always that of the full grid. To get the full grid,

set GAREA to blank, GRID or DSET.

OUTPUT defines the direction and destination of printed output.

GBTBLS allows the user to override the default GRIB decoding tables by listing the

filenames of the WMO and NCEP parameter tables, the vertical coordinate table and

the originating center table.

GBDIAG allows for detailed GRIB message section information, byte-by-byte, to be

printed out for selected GRIB messages. Simply list those GRIB sections to be exam-

ined (IDS, PDS, GDS, BDS, BMS, END or ALL for all sections), e.g., pds;gds. Select-

ed GRIB messages for examination may also be identified by number in list and/or

range format, e.g., 2;4;5-9.

PDSEXT is a logical flag which only becomes applicable when a PDS extension exists

in the GRIB message. If PDSEXT is YES, then a sequence of characters specifying the

extension information will be appended onto the standard GEMPAK parameter name.

If PDSEXT is NO, then the append will not be performed. For instance, if the param-

eter is HGHT and the PDS extension is an ensemble extension indicating that this par-

ticular height field is an ensemble mean average, then the PDS extension suffix will be

"ENMA". The final GEMPAK parameter name will be "HGHTENMA" and must be

referenced as such in any GEMPAK program.

## Examples

1. Convert the data in the NGM GRIB file for 2 Sep 1993. Create the

GEMPAK file using projection #105 from the grid navigation table.

Define a subset using lat/lon.

```
GBFILE = ngm_930902_00_000.grib
INDXFL =
GDOUTF = ngm_930902_00.grd
PROJ =
GRDAREA =
KXKY =
MAXGRD = 3000
CPYFIL = #105
GAREA = ks-
OUTPUT = t
```
2. Convert the data in the MRF GRIB and INDEX files for 2 Sep 1993.

Create the GEMPAK file using the navigation information given the Grid

Definition Section of the GRIB message. Define a subset using grid

coordinates.

```
GBFILE = mrf_930902_00_000.grib
```

```
INDXFL = mrf_930902_00_000.indx
GDOUTF = mrf_930902_00.grd
PROJ =
GRDAREA =
KXKY =
MAXGRD = 3000
CPYFIL = gds
GAREA = @230;110;290;140
OUTPUT = t
```
3. Convert the data in the GRIB file for North America. Create the

GEMPAK file using the user input for PROJ, GRDAREA and KXKY.

Do not do a subset, use the full grid.

```
GBFILE = north_amer.grib
INDXFL =
GDOUTF = north_amer.grd
PROJ = ced
GRDAREA = 0;-180;90;0
KXKY = 181;91
MAXGRD = 3000
CPYFIL =
GAREA = grid
OUTPUT = t
```
4. Convert the data in the GRIB file for the MESO model. Override the

default decoding tables.

```
GBFILE = /tmp/model/meso.mdl
INDXFL =
GDOUTF = /tmp/model/meso.mdl.gem
PROJ =
GRDAREA =
KXKY =
MAXGRD = 3000
CPYFIL = gds
GAREA = grid
OUTPUT = t
GBTBLS = /tmp/wmogrib2.tbl;/tmp/nmcgrib2.tbl;/tmp/vcrdgrib1.tbl
GBDIAG =
```
5. Scan a GRIB file and print out the PDS and GDS sections for GRIB

messages 1, 2, 6 through 12, and 88.

```
GBFILE = ensemble.grib
INDXFL =
GDOUTF = LIST
PROJ = ced
GRDAREA = 0;-180;90;0
KXKY = 181;91
MAXGRD = 3999
CPYFIL =
```

GAREA = grid
OUTPUT = t
GBTBLS =
GBDIAG = pds;gds|1;2;6-12;88


### Error Messages

```
[NAGRIB+5] GRIB version 0 cannot be decoded.

[NAGRIB+4] Invalid parameter code table version.

[NAGRIB+3] No valid parameter found for ....

[NAGRIB+2] No valid vertical coordinate found for ....

[NAGRIB+1] WARNING: This grid is too large for GEMPAK programs.

[NAGRIB-1] Fatal error initializing TAE.

[NAGRIB-2] Fatal error reading TAE parameters.

[NAGRIB-3] Fatal error initializing GEMPLT.

[NAGRIB-4] Navigation information is invalid.

[NAGRIB-5] Grid area ... is invalid.

[NAGRIB-6] Grid size is invalid.

[NAGRIB-7] The grid file name may not be blank.

[NAGRIB-8] Navigation table cannot be read.

[NAGRIB-9] Grid name ... cannot be found in grid table.

[NAGRIB-10] Input for GEMPAK output file is blank.

[NAGRIB-11] Error opening GEMPAK grid file.

[NAGRIB-12] Error creating GEMPAK grid file.

[NAGRIB-13] Cannot open GRIB vertical coordinate table.

[NAGRIB-14] Cannot open GRIB parameter table.

[NAGRIB-15] Error opening GRIB file.

[NAGRIB-16] Error getting next message.

[NAGRIB-17] Error setting date/time.

[NAGRIB-18] Invalid bitmap specification.

[NAGRIB-19] Error reading GRIB file.

[NAGRIB-20] Invalid input for GAREA.

[NAGRIB-21] Grid is too large.
```

[NAGRIB-22] Length of BDS section less than or equal to 0


## 4.53 NAMSND

NAMSND transfers model profile output in BUFR to GEMPAK sounding and surface

data files.



```
SNBUFR BUFR model sounding file
SNOUTF Output sounding file
SFOUTF Output surface file
SNPRMF Sounding parameter packing file
SFPRMF Surface parameter packing file
TIMSTN Times/additional stations
```


NAMSND transfers model profile output in BUFR to GEMPAK sounding and surface

data files. The input BUFR file must be a Jack Woollen type BUFR file whose first

message is a table of information about the file contents. Note that both a GEMPAK

sounding file and a surface file are created by this program.

NAMSND reads the table from the first BUFR message to get information on the pa-

rameters in the file. A print out of this information to a local file called bufr_table.dump

is done if SNPRMF and SFPRMF are both set to blank. No GEMPAK files or other

output files are created in this case.

If the surface file name SFOUTF is followed by +, then a second surface file is opened.

Its name is that of the first with the suffix _aux. The packing table for this auxiliary file

is expected to have the name of that specified in SFPRMF with the suffix _aux. The

auxiliary file contains any primary parameters for which there is no space in the prima-

ry surface data file, followed by diagnosed parameters. The parameter table entries for

the auxiliary file must reflect this requirement. So, the parameter file for the auxiliary

file starts with a continuation of the list of primary parameters in the order expected

from the BUFR data file followed by any diagnosed parameters. If there is no overflow

of primary parameters, then only diagnosed parameters are found in the auxiliary file.

When an auxiliary file is used, any diagnosed parameters that are desired in the primary

file must be listed in the parameter table file for both the primary and the auxiliary file.

A list of diagnosed parameters is given below.

If the BUFR data file name is followed by |sss=#####, where sss is a 3-character station

ID and ##is the corresponding station number, then an ASCII file name prof.sss is

generated. This file will contain the output for only station #####. If no GEMPAK files

are needed, set SNOUTF and SFOUTF to blank. The parameter files are required for

both the GEMPAK files and the ASCII file.

NOTE ABOUT THE PARAMETER TABLE ENTRIES:


Diagnosed or extra computed parameters are added at the end of the parameter list for

surface data. They may be added anywhere for profile data, but the last two columns

must contain the GEMPAK names of the actual BUFR parameters in correct order (as

shown in the file bufr_table.dump). The penultimate column is the scaling factor to be

applied to the parameter named in the last column. Note that the names in the last col-

umn need not agree with the names in the first column for sounding parameter files.

Thus, in the sounding parameter file, the last two columns are essentially independent

of the first four columns. Surface parameter files do not have the extra last column of

names.

None of the parameter names in the GEMPAK parameter file have to be the same as

the names found in the file bufr_table.dump. The file bufr_table.dump gives the order

in which the parameters must be present in the GEMPAK parameter files. The replica-

tion of surface parameters is not indicated in bufr_table.dump. Currently, the only rep-

licated surface parameters are the soil layer parameters in the sequence SLYR (class 1

Eta file only).

--------------------------------------------------------------

```
MODEL Sounding/Surface GEMPAK Parameter Names
```
Name TYP C Definition Units

PRES# SND 2 Pressure mb
TMPC# SND 2 Temperature C
DWPC# SND 2 Dewpoint temperature C
SPED# SND 2 Speed m/s
DRCT# SND 2 Direction degrees
OMEG# SND 2 Omega pa/s
HGHT# SND 2 Height m

DTCP SND 1 Temp tndncy from conv phase change K / day
DTGP SND 1 Temp tndncy from grd-scl phase change K / day
DTSW# SND 1 Temp tndncy from short-wave rad K / day
DTLW# SND 1 Temp tndncy from long-wave rad K / day
DTAR#* SND 1 Temp tndncy from all rad K / day

CFRL# SND 1 Cloud fraction in layer %
CWTR# SND 1 Cloud water Kg / Kg
TKEL# SND 1 Turbulent kinetic energy J / Kg

RADH SND - Net radiative heating rate K/day

#-----------


PMSL# SFC 2 Mean sea-level pressure mb
PRES# SFC 2 Surface pressure mb
TMPC#@ SFC 2 First level temperature C
DWPC#@ SFC 2 First level dewpoint temperature C
SPED#@ SFC 2 First level wind speed m/s
DRCT#@ SFC 2 First level wind direction degrees
SKTC# SFC 2 Earth surface temperature C
SBTC# SFC 1 Bottom soil temperature C
SLTC# SFC 0 Layer 1 soil temperature C
TMIN SFC 1 Min lowest layer air temp over 1 hr C
TMAX SFC 1 Max lowest layer air temp over 1 hr C

FXLH SFC 1 Flux of latent heat (evaporation) W / m**2
FXLP SFC 1 Potential flux of latent heat W / m**2
FXSH SFC 1 Flux of sensible heat W / m**2
FXSS SFC 1 Flux of sub-surface heat W / m**2
FXSN SFC 1 Flux of snow phase change heat W / m**2
SWRD SFC 1 Short-wave radiation downward flux W / m**2
SWRU SFC 1 Short-wave radiation upward flux W / m**2
LWRD SFC 1 Long-wave radiation downward flux W / m**2
LWRU SFC 1 Long-wave radiation upward flux W / m**2
NDRF* SFC 1 Net downward rad flux at the surface W / m**2
RNET* SFC 1 Net radiative flux at the surface W / m**2
FXTT* SFC 1 Total surface energy budget residual W / m**2

P01M SFC 2 Total precip over 1 hr mm (kg/m**2)
P06M SFC - Total precip over 6 hr mm (kg/m**2)
P12M SFC - Total precip over 12 hr mm (kg/m**2)
C01M SFC 2 Convective precip over 1 hr mm (kg/m**2)
S01M* SFC 2 Stable precip over 1 hr mm (kg/m**2)
WTNS# SFC 2 Sfc moisture availability (fraction) unitless
EVAP SFC 0 Evaporation over 1 hr mm (kg/m**2)
SNFL SFC 1 Accumulated snowfall over 1 hr mm (kg/m**2)
SNRA# SFC 1 Snow ratio %
SLMM# SFC 1 Soil moisture mm (kg/m**2)
SWEM# SFC 2 Snow water equivalent mm (kg/m**2)
N01M SFC 1 Total snow melt over 1 hr mm (kg/m**2)
R01M SFC 2 Storm sfc runoff over 1 hr mm (kg/m**2)
BFGR SFC 2 Baseflow-groundwater runoff over 1 hr mm (kg/m**2)
SLLH* SFC 1 Surface evaporation over 1 hr mm (kg/m**2)
SLLP* SFC 1 Potential sfc evaporation over 1 hr mm (kg/m**2)
SWBL* SFC 1 Total water budget residual mm (kg/m**2)

LWRT SFC 1 Long-wave radiation at top W / m**2
SWRT SFC 1 Short-wave radiation at top W / m**2

WXTS# SFC 2 Categorical weather type snow 0,1
WXTP# SFC 2 Categorical weather type ice pellets 0,1
WXTZ# SFC 2 Categorical weather type frzng rain 0,1
WXTR# SFC 2 Categorical weather type rain 0,1
WSYM#* SFC 2 Weather type symbol number -


LCLD# SFC 2 Low-cloud amount %
MCLD# SFC 2 Middle-cloud amount %
HCLD# SFC 2 High-cloud amount %

CLPL SFC - Low-level cloud pressure level mb
CLPM SFC - Mid-level cloud pressure level mb
CLPH SFC - Hgh-level cloud pressure level mb

UWND# SFC 2 10 m u component m/s
VWND# SFC 2 10 m v component m/s
TH10# SFC 1 10 m potential temperature K
Q10M# SFC 1 10 m specific humidity g/kg

HLCY# SFC 2 Estimated storm-relative helicity m*m/s*s
USTM# SFC 2 Estimated storm motion u component m/s
VSTM# SFC 2 Estimated storm motion v component m/s

T2MS# SFC 2 2 m temperature C
TD2M#* SFC 2 Estimated 2 m dewpoint temperature C
Q2MS# SFC 2 2 m specific humidity g/kg

SRLM SFC 1 Surface roughness length m
SEXC# SFC 1 Surface exchange coefficient m/s
VEGF SFC 1 Green vegetation cover %
CNPW SFC 1 Canopy water mm (kg/m**2)

SMCx# SFC 1 Layer x=1,4 volumetric soil moisture -
STCx# SFC 1 Layer x=1,4 soil temperature K

SMSK SFC 2 Land/water mask (0=land, 1=water) -
SELV SFC 2 Surface elevation (model terrain) m
SLAT SFC 2 Station latitude degrees
SLON SFC 2 Station longitude degrees
STNM SFC 2 Station number -

* Parameter is derived during data reformatting. # Parameter is an instantaneous value.

? Parameter is currently undefined (meaningless or missing value). @ Parameter de-

rived from first level profile data.

Column C has the following meaning:

```
0 = Parameter is in class 0 Eta output only
1 = Parameter is in class 1 Eta output only
2 = Parameter is in both class 0 and class 1 Eta output
```
- = Parameter is not in the Eta output

Note that any of these parameters may be found in the output from other NCEP models.

```
Derived Parameter Definitions
```

#DTAR = DTLW + DTSW

#NDRF = SWRD + SWRU + LWRD

#RNET = SWRD + SWRU + LWRD + LWRU

#FXTT = FXLH + FXSH + FXSS + FXSN + RNET

#S01M = P01M - C01M

```
SLLH = dt * FXLH / HEATVP

SLLP = dt * FXLP / HEATVP

SWBL = - ( SLMM (t) - SLMM (t-dt) )
```
- ( SWEM (t) - SWEM (t-dt) )
+ P01M - R01M + SLLH

```
WSYM = weather symbol number from standard symbol table

TD2M = saturation temperature for mixing ratio Q2MS at
pressure PRES
```
HEATVP = 2.834E06 if SWEM (t) > 0. or HEATVP = 2.5E06 if SWEM (t) = 0.

NOTE:

If UWND and VWND (the 10 m wind) are in the BUFR file, then do not request SPED

and DRCT in the parameter file for the primary surface data set. These may be request-

ed in the auxiliary surface data parameter file. Only one piece of wind data can be ac-

cessed from a surface data file. --------------------------------------------------------------

## Examples

1. Create sounding and surface data files from the BUFR output file name

950612.bufr containing hourly profiles and surface data from a 48-h eta

model forecast. This is a standard class 0 BUFR output file.

```
SNBUFR = 950612.bufr
SNOUTF = 950612.snd
SFOUTF = 950612.sfc
SNPRMF = snclass0.prm
SFPRMF = sfclass0.prm
TIMSTN = 49/500
```
2. Create only an ASCII text file from the BUFR data.


```
SNBUFR = 950612.bufr|pit=72520
SNOUTF =
SFOUTF =
SNPRMF = snclass0.prm
SFPRMF = sfclass0.prm
TIMSTN = 49/500
```
3. Create only the ASCII parameter list file bufr_table.dump.

```
SNBUFR = 950612.bufr
SNOUTF =
SFOUTF =
SNPRMF =
SFPRMF =
TIMSTN = 49/500
```

### Error Messages

```
[NAMSND+3] Done -- just wrote out bufr_table.dump.

[NAMSND+2] Creating GEMPAK file ....

[NAMSND+1] End of input file -- all done.

[NAMSND-1] Fatal error initializing TAE.

[NAMSND-2] Fatal error reading TAE parameters.

[NAMSND-3] Error opening model sounding file ....

[NAMSND-4] Error opening sounding parameter file.

[NAMSND-5] Wrong number of sounding parms in output file.

[NAMSND-6] Wrong sounding parameters in output file.

[NAMSND-7] Cannot create a new sounding data file.

[NAMSND-8] Error opening surface parameter file.

[NAMSND-9] Wrong number of surface parms in output file.

[NAMSND-10] Wrong surface parameters in output file.

[NAMSND-11] Cannot create a new surface data file.

[NAMSND-12] Error reading input file.

[NAMSND-13] Packing file not opened.

[NAMSND-14] Different SND parm count in input file.

[NAMSND-15] Different SFC parm count in input file.

[NAMSND-16] Cannot add SND time.

[NAMSND-17] Cannot add SFC time.

[NAMSND-18] Cannot add SND station.

[NAMSND-19] Cannot add SFC station.

[NAMSND-20] Sequence name not found in input file.

[NAMSND-21] No input parameter names were requested.

[NAMSND-22] Forecast time is missing.

[NAMSND-23] Station number is missing.
```

[NAMSND-24] Station latitude is missing.

[NAMSND-25] Station longitude is missing.

[NAMSND-26] Station elevation is missing.

[NAMSND-27] No surface data for a station.

[NAMSND-28] No data in output array.

[NAMSND-29] Sounding has only one level.

[NAMSND-30] Error opening auxiliary parameter file.

[NAMSND-31] Wrong number of auxiliary parms in output file.

[NAMSND-32] Wrong auxiliary parameters in output file.

[NAMSND-33] Cannot create a new auxiliary surface data file.

[NAMSND-34] Error assigning unit number to BUFR file.

[NAMSND-35] Must use all snd parms if not using one profile.

[NAMSND-36] Maximum number of profiles exceeded.

[NAMSND-41] Missing packing information---defective BUFR file.

[NAMSND-42] Too many TABLE B entries found in BUFR file.

[NAMSND-43] A sequence has no member parameters.

[NAMSND-44] Table is too long---MXTBLN is too small.

[NAMSND-45] No TABLE B entries found.

[NAMSND-46] Too many TABLE D entries found.

[NAMSND-47] Too many parameters in a sequence.

[NAMSND-48] No TABLE D entries found---defective BUFR file.

[NAMSND-49] The sequence ... was not found.

[NAMSND-50] The parameter ... was not found in table.

[NAMSND-51] Cannot open BUFR file ....

[NAMSND-52] BUFR file name is blank.

[NAMSND-53] Cannot open bufr_table.dump file.

[NAMSND-54] 6th column entry in SND packing table is missing.


[NAMSND-55] Error opening sounding output file.

[NAMSND-56] Error opening surface output file.

[NAMSND-57] Error opening surface auxiliary output file.


## 4.54 NDFDG2

NDFDG2 converts NDFD gridded data in GRIB2 files to GEMPAK gridded data.



```
GBFILE GRIB data file name
GDOUTF Output grid file
MAXGRD Maximum number of grids
GAREA Graphics area
GSKIP Skip factor for GRIB2 files
OUTPUT Output device/filename
```


NDFDG2 will convert NDFD gridded data which is in GRIB2 files to gridded data in

a GEMPAK file.

The GRIB2 input file is specified in GBFILE.

The GEMPAK output file is given in GDOUTF. This file will be opened if it exists and

the projection information matches the GRIB2 message projection. If the output file-

name is "LIST", the GRIB file will be scanned and the decoded GEMPAK header in-

formation will be printed out.

GAREA is used to subset the grid. It may be specified as any valid input for GAREA

which are equivalent to lat/lon bounds or as a single @ followed by the lower-left and

upper-right grid point coordinates separated by semicolons. In the former case, the lo-

cations are rounded to the nearest grid points. The projection type and angles entered

as described above along with the lat/lon coordinates of the corners of the subset define

a new grid navigation different from that of the full grid. The navigation entered as de-

scribed in the preceding paragraph is always that of the full grid. To get the full grid,

set GAREA to blank, GRID or DSET.

GSKIP is used to reduce the resolution of a grid. This may be required if the GRIB2

full resolution grid exceeds the GEMPAK maximum grid size.

OUTPUT defines the direction and destination of printed output.

## Examples

1. Convert the data in the GRIB2 file for the MESO model.

```
GBFILE = /tmp/model/meso.mdl
GDOUTF = /tmp/model/meso.mdl.gem
MAXGRD = 3000
GAREA = KY
GSKIP =
```

```
OUTPUT = t
```
2. Scan a GRIB2 file and print out the message info.

```
GBFILE = ensemble.grib
GDOUTF = LIST
MAXGRD = 3999
GAREA = KY
GSKIP =
OUTPUT = t
```
3. Skip every two grid points in X and Y for whole grid.

```
GBFILE = /tmp/model/meso.mdl
GDOUTF = /tmp/model/meso.mdl.gem
MAXGRD = 3000
GAREA =
GSKIP = 2
OUTPUT = t
```
4. Skip every other grid point in X and Y for GAREA.

```
GBFILE = ensemble.grib
GDOUTF = ensemble.grib.gem
MAXGRD = 3000
GAREA = MD
GSKIP = 1
OUTPUT = t
```

### Error Messages

```
[NDFDG2+5] GRIB version 0 cannot be decoded.

[NDFDG2+4] Parameter not found in table: ...

[NDFDG2+3] GRIB2 unpacking warning ... returned from unpk_grib2.

[NDFDG2+2] Vertical coordinate ... not supported.

[NDFDG2+1] WARNING: This grid is too large for GEMPAK programs.

[NDFDG2-1] Fatal error initializing TAE.

[NDFDG2-2] Fatal error reading TAE parameters.

[NDFDG2-3] Fatal error initializing GEMPLT.

[NDFDG2-4] Grid Definition Template number ... not supported.

[NDFDG2-5] Grid area ... is invalid.

[NDFDG2-6] Grid size is invalid.

[NDFDG2-7] The grid file name may not be blank.

[NDFDG2-8] Source of Grid Definition ... not supported.

[NDFDG2-9] Fatal GRIB2 unpacking error ... returned from unpk_grib2.

[NDFDG2-10] Input for GEMPAK output file is blank.

[NDFDG2-11] Error opening GEMPAK grid file.

[NDFDG2-12] Error creating GEMPAK grid file.

[NDFDG2-13] Discipline number ... not supported.

[NDFDG2-14] Data type ... not supported.

[NDFDG2-15] Error opening GRIB file.

[NDFDG2-16] Error getting next message.

[NDFDG2-17] Error setting date/time.

[NDFDG2-18] Invalid bitmap specification.

[NDFDG2-19] Error reading GRIB file.

[NDFDG2-20] Invalid input for GAREA.

[NDFDG2-21] Grid is too large.
```

[NDFDG2-22] Local section present, but not supported.

[NDFDG2-23] No data present in grib message.

[NDFDG2-24] GSKIP too large for size of grid.


## 4.55 NEX2GINI

NEX2GINI creates a GINI format image composite of NEXRAD level III products.



```
GRDAREA Area covered by grid
PROJ Map projection/angles/margins|drop flag
KXKY Number of grid points in x;y
CPYFIL Grid file whose navigation is to be used in new grid file |
GFUNC Scalar grid
RADTIM Radar composite current/dattim
RADDUR Radar time window (minutes prior to RADTIM)
RADFRQ Update Frequency
STNFIL Station information file
RADMODE Radar operational mode
SATFIL Satellite image filename(s)
COMPRESS Write output in compressed format
```


NEX2GINI samples NEXRAD Level III (NIDS) products to a common grid projection.

Then creates a GINI format image with appropriate header information using the grid

as the image raster. NEX2GINI is not limited by the traditional LLMXGD limitation

for grid files.

NEX2GINI uses a suplemental table $GEMTBL/unidata/nex2gini.tbl to specify con-

figurations for product identifiers so that the generated images are correctly identified

for enhancement tables in $GEMTBL/sat/imgtyp.tbl.

SATFIL specifies the output GINI file. If the file already exists, it is overwritten.

CPYFIL may provide either an existing grid file to read the projection information

from, or a grid number (#nnn) defined in grdnav.tbl.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if CPYFIL is

blank. The GINI format has additional restrictions which limit which GEMPAK pro-

jections may be used. These are, LCC tangent projection (eg la1 and la2 are the same),

STR, and CED.

STNFIL is the station table which supplies radar IDs to be searched for the composite.

If STNFIL is blank, then 'nexrad.tbl' is used by default.

GFUNC is the data parameter which the composite is created for. The NEXRAD file

naming is assumed to be such that the site identifier and the product type are both

present in the directory/file naming structure. The datatype.tbl template NEXRIII is

used to provide the file naming convention used. If NEXRIII is not found in the tem-

plate database, a default directory structure for NEXRAD data is assumed where the

root directory $RAD/NIDS contains a tree structure supporting %SITE%/%PROD%/


%PROD%_YYYYMMDD_HHNN file names. The %SITE% template will be re-

placed by the site IDs in the STNFIL table. The %PROD% will be replaced by the

GFUNC product name. The GEMPAK data/time template will be used with RADTIM

and RADDUR to determine which NEXRAD products are in the valid time range.

RADTIM determined the output grid time for the radar composite. The value of

RADTIM may either be 'current', or a GEMPAK dattim. If 'current' is selected for

RADTIM, then the current system clock time is used. No data files later than RADTIM

will be included in the composite. RADDUR provides the time window previous to

RADTIM in order to include data for each site. The time closest to RADTIM will be

used. A default RADDUR of 30 minutes is used if RADDUR is blank.

RADFRQ is the frequency in minutes at which the program will run. When RADFRQ

is defined, NEX2GINI will wait for the specified time before rerunning. This option is

most useful when RADTIM is set to 'current'. When the program is sleeping, ctrl-c can

be used to exit the loop and return to the dynamic tutor. If RADFRQ is not set, the dy-

namic tutor will be re-entered at the end of processing the radar mosaic.

RADMODE allows the user to select whether to include radar data from sites operating

in (P) precipitation/storm mode, (C) clear air mode, and/or (M) maintainence mode.

The default, if none are specified is data from all 3 modes (PCM). Multiple modes may

be specified.

COMPRESS allows the user to optionally write the GINI file using PNG compression

for the image raster. The GINI header will identify the image as a compressed product.

## Examples

1. Create a 1km National composite of NEXRAD base reflectivity (N0R).

Use the current time with a 30 minute window for data. Rerun the

mosaic creation continuously with a 5 minute wait period between each

update. Use the nexrad.tbl station table. Create the GINI output file

using the rad_YYYYMMDD_HHNN file name template. Accept data

from radars operating in precipitation and clear air mode. Do not write

the GINI output using compression.

#GRDAREA = 23;-120;47.2634;-63.5664

```
PROJ = lcc/40;-100;40
KXKY = 4736;3000
CPYFIL =
GFUNC = n0r
RADTIM = current
RADDUR = 30
RADFRQ = 5
STNFIL = nexrad.tbl
RADMODE = PC
SATFIL = rad_YYYYMMDD_HHNN
```

```
COMPRESS = no
```
### Error Messages

```
[NEX2GINI+5] Mode rejected: ...

[NEX2GINI+4] Write image ...

[NEX2GINI+3] Using default station file ...

[NEX2GINI+2] NEXRIII template not found using ...

[NEX2GINI+1] Too old: ...

[NEX2GINI0] Using: ...

[NEX2GINI-1] Fatal error initializing TAE.

[NEX2GINI-2] Fatal error reading TAE parameters.

[NEX2GINI-3] Fatal error initializing GEMPLT.

[NEX2GINI-4] Failed to read grid projection ...

[NEX2GINI-8] Station table ... not found
```

## 4.56 NEX2IMG

NEX2IMG creates a GIF format image composite of NEXRAD level III products.



```
GRDAREA Area covered by grid
PROJ Map projection/angles/margins|drop flag
KXKY Number of grid points in x;y
CPYFIL Grid file whose navigation is to be used in new grid file |
GFUNC Scalar grid
RADTIM Radar composite current/dattim
RADDUR Radar time window (minutes prior to RADTIM)
RADFRQ Update Frequency
STNFIL Station information file
RADMODE Radar operational mode
RADFIL Radar image filename(s)
LUTFIL Enhancement lookup table filename
```


NEX2IMG samples NEXRAD Level III (NIDS) products to a common grid projection,

and then creates a GIF format image raster. NEX2IMG is not limited by the traditional

LLMXGD limitation for grid files.

NEX2IMG uses a suplemental table $GEMTBL/unidata/nex2gini.tbl to specify config-

urations for data to pixel mappings.

RADFIL specifies the output file. If the file already exists, it is overwritten.

CPYFIL may provide either an existing grid file to read the projection information

from, or a grid number (#nnn) defined in grdnav.tbl.

PROJ, GRDAREA, and KXKY define a grid navigation as in GDCFIL if CPYFIL is

blank.

STNFIL is the station table which supplies radar IDs to be searched for the composite.

If STNFIL is blank, then 'nexrad.tbl' is used by default.

GFUNC is the data parameter which the composite is created for. The NEXRAD file

naming is assumed to be such that the site identifier and the product type are both

present in the directory/file naming structure. The datatype.tbl template NEXRIII is

used to provide the file naming convention used. If NEXRIII is not found in the tem-

plate database, a default directory structure for NEXRAD data is assumed where the

root directory $RAD/NIDS contains a tree structure supporting %SITE%/%PROD%/

%PROD%_YYYYMMDD_HHNN file names. The %SITE% template will be re-

placed by the site IDs in the STNFIL table. The %PROD% will be replaced by the

GFUNC product name. The GEMPAK data/time template will be used with RADTIM

and RADDUR to determine which NEXRAD products are in the valid time range.


RADTIM determined the output grid time for the radar composite. The value of

RADTIM may either be 'current', or a GEMPAK dattim. If 'current' is selected for

RADTIM, then the current system clock time is used. No data files later than RADTIM

will be included in the composite. RADDUR provides the time window previous to

RADTIM in order to include data for each site. The time closest to RADTIM will be

used. A default RADDUR of 30 minutes is used if RADDUR is blank.

RADFRQ is the frequency in minutes at which the program will run. When RADFRQ

is defined, NEX2IMG will wait for the specified time before rerunning. This option is

most useful when RADTIM is set to 'current'. When the program is sleeping, ctrl-c can

be used to exit the loop and return to the dynamic tutor. If RADFRQ is not set, the dy-

namic tutor will be re-entered at the end of processing the radar mosaic.

RADMODE allows the user to select whether to include radar data from sites operating

in (P) precipitation/storm mode, (C) clear air mode, and/or (M) maintainence mode.

The default, if none are specified is data from all 3 modes (PCM). Multiple modes may

be specified.

LUTFIL specifies the color table to be used for the output GIF file.

## Examples

1. Create a National composite of NEXRAD base reflectivity (N0R). Use

the current time with a 30 minute window for data. Rerun the mosaic

creation continuously with a 5 minute wait period between each update.

Use the nexrad.tbl station table. Create the GIF output file using the

YYYYMMDD_HHNN.gif file name template. Accept data from radars

operating in precipitation and clear air mode.

#GRDAREA = 25;-125;50;-65

#PROJ = CED

#KXKY = 6000;2500

#CPYFIL =

```
GFUNC = n0r
RADTIM = current
RADDUR = 30
RADFRQ = 5
STNFIL = nexrad.tbl
RADMODE = PC
SATFIL = YYYYMMDD_HHNN.gif
LUTFIL = upc_rad24.tbl
```

### Error Messages

```
[NEX2IMG+5] Mode rejected: ...

[NEX2IMG+4] Write image ...

[NEX2IMG+3] Using default station file ...

[NEX2IMG+2] NEXRIII template not found using ...

[NEX2IMG+1] Too old: ...

[NEX2IMG0] Using: ...

[NEX2IMG-1] Fatal error initializing TAE.

[NEX2IMG-2] Fatal error reading TAE parameters.

[NEX2IMG-3] Fatal error initializing GEMPLT.

[NEX2IMG-4] Failed to read grid projection ...

[NEX2IMG-5] Error writing to ...

[NEX2IMG-8] Station table ... not found
```

## 4.57 NEXR2RHI

NEXR2RHI displays NEXRAD level II vertical cross sections.



```
CXSTNS Cross-section station line
GVCORD Grid vertical coordinate
PTYPE Plot type/h:w ratio/margins
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
SKIP Skip_cntr/skip_plt_x;skip_plt_y
CINT Contour interval/min/max
SCALE Scalar scale / vector scale
LINE Color/type/width/label/smth/fltr
BORDER Background color/type/width
TITLE Title color/line/title
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
PANEL Panel loc/color/dash/width/regn
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CONTUR Subbox/smooth
FINT Fill interval/min/max
FLINE Fill colors/fill types
CTYPE Contour type: C/F
RADFIL Radar image filename(s)
RADTIM Radar composite current/dattim
RADPARM Radar parameter (dz, vr, sw)
INTERP Interpolation flag
```


NEXR2RHI displays NEXRAD Level II vertical cross sections for data in ARCHIVE2

format. This program is adopted from GDCROSS to create a volume rendering of

NEXRAD data along a specified axis.

RADFIL specifies the input Level II file. A template may be specified, with an optional

site name (eg NEXRII|KDDC). If a site name is provided, it will be used to replace the

%SITE% alias in the template name. The NEXRII template is provided for

ARCHIVE2 format files.

RADPARM is the Radar parameter to be displayed. Valid values are dz (reflectivity),

vr (radial velocity), sw (spectrum width).

RADTIM is a valid GEMPAK date/time string or abbreviation. A time range may be

specified.

INTERP is a logical variable which determines whether interpolation between sweeps

will occur.


CXSTNS is the cross section axis (as in GDCROSS). A grid coordinate can be used

with row and colums 1 to 460.

GVCORD is the vertical coordinate of the radar data (always HGHT).

PTYPE is the plot type. A value of "LIN" is generally the only useful setting.

Other parameters as in GDCROSS.

## Examples

1. Display an East-West RHI which passes through the radar location for

reflectivity data from KLVX. Display the RHI from 0 to 20KM using

color filled contours. Use a contour interval of 4 dBZ with a minimum

value of -12 dBZ. Grid point 230;230 is the radar center. Interpolate

contours between beam scans.

#CXSTNS = @1;230>@460;230

```
GVCORD = hght
PTYPE = lin
YAXIS = 0/20000
CINT = 0
SCALE = 0
LINE = 1
BORDER = 1
SKIP = 0
TITLE = 1/-2/RHI Base Reflectivity Level II ^
CLEAR = YES
DEVICE = xw
TEXT = .8/1/1/111/hw
PANEL = 0
CLRBAR = 1
CONTUR = 3/2
FINT = 4
FLINE = 0;30-7
CTYPE = f
RADFIL = NEXRII|KLVX
RADPARM = dz
RADTIM = last
INTERP = y
```
2. Display a cross section of radial velocity from 38.6N;86.8W to

37.4N;84.8W. Plot filled contours at 4 m/s intervals. Display data from

0 to 20km. Allow interpolation between beam scans. Use line countours,

with negative values contoured by a dotted line, positive values contoured

with a solid line.

#CXSTNS = 38.6;-86.8>37.4;-84.8

```
GVCORD = hght
```

```
PTYPE = lin
YAXIS = 0/20000
CINT = 4
SCALE = 0
LINE = 5/-10
BORDER = 1
SKIP = 0
TITLE = 1/-2/RHI Radial Velocity Level II ^
CLEAR = YES
DEVICE = xw
TEXT = .8/1/1/111/hw
PANEL = 0
CLRBAR =
CONTUR = 3/2
FINT =
FLINE =
CTYPE = c
RADFIL = NEXRII|KLVX
RADPARM = vr
RADTIM = last
INTERP = y
```
### Error Messages

```
[NEXR2RHI-1] Fatal error initializing TAE.

[NEXR2RHI-2] Fatal error reading TAE parameters.

[NEXR2RHI-3] Fatal error initializing GEMPLT.
```

## 4.58 OABOX

OABOX draws a box around an objective analysis region.



```
LINE Color/type/width/label/smth/fltr
DEVICE Device|name|x size;y size|color type
REGION Region type
GAREA Graphics area
PROJ Map projection/angles/margins|drop flag
GDFILE Grid file
```


This program draws a box around a region defined for the objective analysis programs.

REGION can be specified as GRID, DATA, or EXTEND. The default is GRID.

The GRID region is the area used for the objective analysis. The DATA region is the

area over which data for the analysis is extracted. The EXTEND region is the grid area

extended in each direction for the first pass analysis.

## Examples

1. Draw a solid line around the grid area in color 1 using a solid line of

width 1.

#LINE = 1

```
REGION = grid
GDFILE = sample.grd
DEVICE = xw
GAREA = us
PROJ = mer
```
2. Draw a line around the data region using color 2, line type 3, and line

width 5.

#LINE = 2/3/5

```
REGION = data
```

### Error Messages

```
[OABOX-1] Fatal error initializing TAE.

[OABOX-2] Fatal error reading TAE parameters.

[OABOX-3] Fatal error initializing GEMPLT.

[OABOX-4] No box will be drawn since the color is 0.

[OABOX-5] Invalid input for REGION.
```

## 4.59 OABSFC

OABSFC performs a Barnes objective analysis on surface data.



```
SFFILE Surface data file
GDFILE Grid file
SFPARM Surface parameter list
DATTIM Date/time
DTAAREA Data area for OA
GUESS Guess file*time
GAMMA Convergence parameter
SEARCH Search radius/Extrapolation
NPASS Number of passes
QCNTL Quality control threshold
```


OABSFC performs a Barnes objective analysis on surface data. Multiple parameters

may be analyzed at the same time. If more than one time is entered, the analyses will

be performed sequentially.

The input surface file is specified in SFFILE. Up to three files may be entered by sep-

arating the names with a plus, +. If the time is set to FIRST or LAST, the first or last

time from the first file will be used.

The projection and area for the grid are read from the navigation block in the output

grid file. The extend grid area, the station spacing and the data subset area are read from

the analysis block. The extend area is used to define a larger grid for the first pass, al-

lowing data to be interpolated back to stations outside the final grid area. The average

station spacing is used to compute the weighting functions. The data subset area spec-

ifies which data will be used in the analysis. If DTAAREA is blank or set to DATA,

the data area defined in the analysis block will be used. This is the recommended pro-

cedure. However, DTAAREA may be specified in the same way as the AREA param-

eter used in other programs. This is especially useful to eliminate stations which are

known to have bad data.

A first-guess field can be used in the analysis. The name of the grid file containing the

guess field must be entered in GUESS. In addition, the time to be used to extract the

guess field from the file must be entered as the second value in GUESS following a *.

If a guess field is entered, the navigation information in the guess grid file must be iden-

tical to that in the output grid file. When a guess field is used, it is inserted into the grid

as the zeroth pass.

Each pass of the analysis interpolates data from stations to grid points using the weight-

ing function:


#WTFUNC = EXP [ -DIST **2/WEIGHT ]

where:

DIST ** 2 = [ ( lat (grid) - lat (stn) ) ** 2 +

```
( lon (grid) - lon (stn) ) ** 2 * coslsq (grid) ]
```
COSLSQ = COS ( lat (grid) ) ** 2 WEIGHT = 5.051457 * ( DELTAN * 2 / PI ) ** 2

DELTAN = Station spacing read from grid file analysis block

Near the poles, an approximate calculation of the distance along a great circle arc is

used.

GAMMA, the convergence parameter, multiplies the weights for passes after the first

pass. GAMMA must be within the range 0 - 1. Any value outside this range will de-

fault to a value of .3. If GAMMA is 0, the number of passes will be set to 1. The rec-

ommended value for GAMMA is .3.

SEARCH is used to control the search radius, which is the maximum distance that a

station may be from a grid point to be used in the analysis for that point. The search

radius will be set so that stations whose weighting function would be less than EXP (-

SEARCH) will not be used. SEARCH must be in the range 1 - 50. If it is not, a default

value of 20 will be used. If a very small value is used, many grid points will not have

3 stations within the search area and will be set to the missing data value. Entering /EX

after the SEARCH value allows data extrapolation to assign values to grid points on the

periphery of the data region. This will reduce the number of grid points with missing

values in data void regions.

NPASS controls the number of passes. Valid values are in the range 1 - 5. Note that

two passes are STRONGLY RECOMMENDED.

QCNTL is the quality control threshold values. It is used only when the first-guess field

exists.

## Examples

1. Analyze 15Z temperature, dewpoint, wind components and mean sea level

pressure in two surface files for the area of us-. The first-guess field is

from the 3hr forecast of NGM. The output grid is sfc.grd. If the

difference between the original temperature and pressure data and the

interpolated first-guess values is greater than 5 C and 10 mb, the original

data will be discarded.

```
SFFILE = $OBS/hrly/990219.hrly + $OBS/ship/99021915.ship
GDFILE = sfc.grd
SFPARM = tmpc;dwpc;uwnd;vwnd;pmsl
```

#DATTIM = 15

```
DTAAREA = us-
GUESS = $MODEL/ngm/ngm_99021912*990219/1200F03
GAMMA = 0.3
SEARCH = 20
NPASS = 2
QCNTL = 5;;;;10
```
2. Repeat the analysis eliminating the data at stations DCA and BUF.

```
DTAAREA = data/-@dca;buf
```

### Error Messages

```
[OABSFC+11] Surface file ... cannot be opened.

[OABSFC+10] There are no times in file ....

[OABSFC+9] No data from ... will be used.

[OABSFC+8] Parameter ... is a character parameter.

[OABSFC+7] Parameter ... cannot be computed.

[OABSFC+6] WARNING: Area is not DATA area in file.

[OABSFC+5] WARNING: The recommended number of passes is 2.

[OABSFC+4] ... is invalid for NPASS. It is set to 2.

[OABSFC+3] ... is invalid for search. It is set to 20.

[OABSFC+2] WARNING: GAMMA is 0. There will be only 1 pass.

[OABSFC+1] ... is invalid for GAMMA. It is set to .3.

[OABSFC-1] Fatal error initializing TAE.

[OABSFC-2] Fatal error reading TAE parameters.

[OABSFC-3] Fatal error initializing GEMPLT.

[OABSFC-4] Grid size is too large.

[OABSFC-5] Not enough buffer space; reduce number of grids.

[OABSFC-6] There are too many stations in data subset area.

[OABSFC-7] There are too few stations in data subset area.

[OABSFC-8] There are no times in the surface files.

[OABSFC-9] No valid parameters have been entered.

[OABSFC-10] No surface file could be opened.

[OABSFC-11] Time cannot be found in ....
```

## 4.60 OABSND

OABSND performs a Barnes objective analysis on upper air data.



```
SNFILE Sounding data file
GDFILE Grid file
SNPARM Sounding parameter list
STNDEX Stability indices
LEVELS Vertical levels
VCOORD Vertical coordinate type
DATTIM Date/time
DTAAREA Data area for OA
GUESS Guess file*time
GAMMA Convergence parameter
SEARCH Search radius/Extrapolation
NPASS Number of passes
QCNTL Quality control threshold
```


OABSND performs a Barnes objective analysis on upper air data. Multiple parameters

and levels may be analyzed at the same time. If more than one time is entered, the anal-

yses will be performed sequentially.

The input sounding file is specified in SNFILE. Up to three files may be entered by

separating the names with a plus, +. If the time is set to FIRST or LAST, the first or

last time from the first file will be used.

The projection and area for the grid are read from the navigation block in the output

grid file. The extend grid area, the station spacing and the data subset area are read from

the analysis block. The extend area is used to define a larger grid for the first pass, al-

lowing data to be interpolated back to stations outside the final grid area. The average

station spacing is used to compute the weighting functions. The data subset area spec-

ifies which data will be used in the analysis. If DTAAREA is blank or set to DATA,

the data area defined in the analysis block will be used. This is the recommended pro-

cedure. However, DTAAREA may be specified in the same way as the AREA param-

eter used in other programs. This is especially useful to eliminate stations which are

known to have bad data.

A first-guess field can be used in the analyses. The name of the grid file containing the

guess field must be entered in GUESS. In addition, the time to be used to extract the

guess field from the file must be entered as the second value in GUESS following a *.

If a guess field is entered, the navigation information in the guess grid file must be iden-

tical to the that in the output grid file. When a guess field is used, it is inserted into the

grid as the Zeroth pass.


Each pass of the analysis interpolates data from stations to grid points using the weight-

ing function:

WTFUNC = EXP [ -DIST ** 2 / WEIGHT ]

where:

DIST ** 2 = [ ( lat (grid) - lat (stn) ) ** 2 +

```
( lon (grid) - lon (stn) ) ** 2 * coslsq (grid) ]
```
COSLSQ = COS ( lat (grid) ) ** 2 WEIGHT = 5.051457 * ( DELTAN * 2 / PI ) ** 2

DELTAN = Station spacing read from grid file analysis block

Near the poles, an approximate calculation of the distance along a great circle arc is

used.

GAMMA, the convergence parameter, is a multiplies the weights for passes after the

first pass. GAMMA must be within the range 0 - 1. Any value outside this range will

default to a value of .3. If GAMMA is 0, the number of passes will be set to 1. The

recommended value for GAMMA is .3.

SEARCH is used to control the search radius, which is the maximum distance that a

station may be from a grid point to be used in the analysis for that point. The search

radius will be set so that stations whose weighting function would be less than EXP (-

SEARCH) will not be used. SEARCH must be in the range 1 - 50. If it is not, a default

value of 20 will be used. If a very small value is used, many grid points will not have

3 stations within the search area and will be set to the missing data value. Entering /EX

after the SEARCH value allows data extrapolation to assign values to grid points on the

periphery of the data region. This will reduce the number of grid points with missing

values in data void regions.

NPASS controls the number of passes. Valid values are in the range 1 - 5. Note that

two passes are STRONGLY RECOMMENDED.

QCNTL is the quality control threshold values. It is used only for level parameters

when the first-guess field exists.

## Examples

1. Analyze temperature, dewpoint, height and wind components at the

mandatory levels at all time periods. Also analyze the lifted and

Showalter indices. If the difference between the original temperature and

height data and the interpolated first-guess values is greater than 5 C and

50 meters, the original data will be discarded.

```
SNFILE = $GEMDATA/hrcbob.snd
GDFILE = hbobsnd.grd
```

SNPARM = tmpc;dwpc;hght;uwnd;vwnd
STNDEX = lift;show
LEVELS = man
VCOORD = pres
DATTIM = all
DTAAREA = us-
GUESS =
GAMMA = 0.3
SEARCH = 20
NPASS = 2
QCNTL = 5;;50


### Error Messages

```
[OABSND+10] No data from ... will be used.

[OABSND+9] Station parameter ... cannot be computed.

[OABSND+8] Parameter ... is a character parameter.

[OABSND+7] Parameter ... cannot be computed.

[OABSND+6] WARNING: Area is not DATA area in file.

[OABSND+5] WARNING: The recommended number of passes is 2.

[OABSND+4] ... is invalid for NPASS. It is set to 2.

[OABSND+3] ... is invalid for search. It is set to 20.

[OABSND+2] WARNING: GAMMA is 0. There will be only 1 pass.

[OABSND+1] ... is invalid for GAMMA. It is set to .3.

[OABSND-1] Fatal error initializing TAE.

[OABSND-2] Fatal error reading TAE parameters.

[OABSND-3] Fatal error initializing GEMPLT.

[OABSND-4] Grid size is too large.

[OABSND-5] Not enough buffer space; reduce number of grids.

[OABSND-6] There are too many stations in data subset area.

[OABSND-7] There are too few stations in data subset area.

[OABSND-8] There are no times in the sounding file.

[OABSND-9] No valid parameters have been entered.

[OABSND-10] LEVELS cannot contain a range.

[OABSND-11] Sounding file could not be opened.

[OABSND-12] Time cannot be found in ....
```

## 4.61 OAGRID

OAGRID creates a GEMPAK grid file which can be used in a Barnes objective analysis

program.



```
GDFILE Grid file
DELTAN Station spacing
DELTAX X spacing
DELTAY Y spacing
GRDAREA Area covered by grid
EXTEND Points to extend grid
DTAAREA Data area for OA
SOURCE Data source (SN or SF)
SNFILE Sounding data file
SFFILE Surface data file
SNPARM Sounding parameter list
SFPARM Surface parameter list
DATTIM Date/time
LEVELS Vertical levels
MAXGRD Maximum number of grids
```


This program allows the user to create a GEMPAK grid file which contains the infor-

mation required to perform a Barnes objective analysis.

The output grids will be evenly spaced latitude/longitude (CED) grids. Three areas

used by the objective analysis programs are defined in this program. GRDAREA de-

fines a region for the output grid. The upper right corner specified will be moved to-

ward the lower left in order to align it on a grid point.

The second area, defined by EXTEND, is used to extend the grid area outward by some

number of grid points. This area is used as a first-pass grid area in the objective anal-

ysis. The default EXTEND values are 2,2,2,2.

DTAAREA defines the area over which station data will be input to the analysis. Only

data within the EXTEND area are used for the second pass. If a value for DTAAREA

is not specified, the EXTEND area will be used.

If values for the station spacing and grid spacings, DELTAN, DELTAX, and

DELTAY, are all specified by the user, they are stored in the grid file for use by the

analysis programs. If any of these numbers is 0, a suggested station spacing is comput-

ed using the station data from the file specified. This station spacing will be used to

compute values for DELTAX and DELTAY. The computed station spacing is the av-

erage of the average minimum station spacing and the uniform station spacing. These

station spacing values are computed using the stations reporting data for the first DAT-

TIM. The average minimum station spacing is the average of the distances from each


station to its closest station. The uniform spacing is the spacing that would be found

between stations if they were evenly spaced over the data area.

GDCFIL can also be used to create grid files. OAGRID creates only CED grids, but

values for the grid spacing and station spacing are estimated from the input surface or

upper air file. GDCFIL creates grid files for grids in any projection, but the grid spacing

and station spacing must be input directly.

## Examples

1. Create a surface grid file called hbobsfc.grd. Use the temperature and

dewpoint in the surface file hrcbob.sfc to compute the station spacing.

The grid area is US; use defaults for the remaining parameters. Create a

file which can contain up to 200 grids.

```
GDFILE = hbobsfc.grd
DELTAN = 0
DELTAX = 0
DELTAY = 0
GRDAREA = us
EXTEND = 2;2;2;2
DTAAREA =
SOURCE = sf
SNFILE = $GEMDATA/hrcbob.snd
SFFILE = $GEMDATA/hrcbob.sfc
SNPARM =
SFPARM = tmpc;dwpc
DATTIM = last
LEVELS = 850
MAXGRD = 200
```
2. Create a upper-air grid file called hbobsnd.grd. Use the temperature and

dewpoint at 500 mb from the upper-air file hrcbob.snd to compute the

station spacing. Create a file which can contain up to 2000 grids.

```
GDFILE = hbobsnd.grd
DELTAN = 0
DELTAX = 0
DELTAY = 0
GRDAREA = us
EXTEND = 2;2;2;2
DTAAREA = us
SOURCE = sn
SNFILE = $GEMDATA/hrcbob.snd
SFFILE = $GEMDATA/hrcbob.sfc
SNPARM = tmpc;dwpc
SFPARM = tmpc;dwpc
DATTIM = last
LEVELS = 500
MAXGRD = 2000
```

### Error Messages

```
[OAGRID+1] WARNING : This grid is too large for GEMPAK programs.

[OAGRID-1] Fatal error initializing TAE.

[OAGRID-2] Fatal error reading TAE parameters.

[OAGRID-3] ... is invalid for GRDAREA.

[OAGRID-4] ... is invalid for DTAAREA.

[OAGRID-5] No data file name specified.

[OAGRID-6] Parameter input is invalid.

[OAGRID-7] Parameter ... cannot be calculated.

[OAGRID-8] Invalid value for DELTAX or DELTAY.

[OAGRID-9] Too few stations to calculate DELTAN.

[OAGRID-10] Source must be set to SN or SF.

[OAGRID-11] Station data file is invalid.

[OAGRID-12] Invalid time requested.

[OAGRID-13] Invalid level requested.
```

## 4.62 SECTOR

SECTOR subsets a GINI satellite image in area and pixel resolution.



```
SATFIL Satellite image filename(s)
OUTFIL Output satellite image filename
GAREA Graphics area
PIXRES Resolution of output image
```


SECTOR subsets a GINI satellite image in area and pixel resolution. The input satellite

file MUST be an AWIPS/GINI file. The new file is created with an updated header

based on the subset area or the new number of pixels and lines.

If GAREA is DSET the entire image is used, otherwise a subset specified by GAREA

is obtained from the image. PIXRES is the input for how many pixels and lines to in-

clude in the new image. If PIXRES is 4, then every 4th pixel and line will be used in

the new image.

## Examples

1. Create a subset image over Kansas, that uses all pixels.

#SATFIL = IR_960723_1200

```
OUTFIL = IR_960723_1200_ks
GAREA = ks
PIXRES = 1
```
2. Create a new image that is for the entire area but uses every 4th pixel

and line.

#SATFIL = IR_960723_1200

#OUTFIL = IR_960723_1200_4

```
GAREA = dset
PIXRES = 4
```

### Error Messages

```
[SECTOR-1] Fatal error initializing TAE.

[SECTOR-2] Fatal error reading TAE parameters.

[SECTOR-3] Error initializing GEMPLT.

[SECTOR-4] Input file is not a GINI image.

[SECTOR-5] File ... does not exist.

[SECTOR-6] Too few points in the requested image.

[SECTOR-7] Requested image has no points from the original.

[SECTOR-8] Bad bounds for the requested image.

[SECTOR-9] Pixel resolution, ..., is invalid
```

## 4.63 SFCFIL

SFCFIL creates a new GEMPAK surface file.



```
SFOUTF Output surface file
SFPRMF Surface parameter packing file
STNFIL Station information file
SHIPFL Ship data file flag
TIMSTN Times/additional stations
SFFSRC Surface file source
```


SFCFIL creates a GEMPAK surface file. The file may be a standard file or a ship for-

mat file. The file may have two data types -- decoded data and/or text data.

SFFSRC is the surface file source and may be set to either decoded data (including

AIRW, METR, SHIP, BUOY, and SYNP) or TEXT.

If both types are requested, they are separated by a '|'. SFFSRC may be entered as:

```
SFFSRC = ==> create a file for
unknown (UNKN) decoded
data

SFFSRC = AIRW ==> create a file for
decoded airways data

SFFSRC = TEXT ==> create a file for
text data

SFFSRC = AIRW|TEXT ==> create a file for both
```
The decoded file type MUST be specified first, otherwise a default to text only will oc-

cur.

If SHIPFL is set to YES, a ship format file is created. In this case, both station location

and time are stored with each data entry. This capability is useful when the station lo-

cation varies in time, such as for moving ships, aircraft or free-floating buoys.

If a standard file is to be created, the maximum number of times to be included in the

file must be entered as the first value in TIMSTN. If a ship format file is being created,

the maximum number of entries in the file is given by the first value in TIMSTN.

If STNFIL is not blank, information about all the stations in STNFIL will be added to

the data set. Space will be left in the file for the additional number of stations specified


as the second parameter in TIMSTN. Note that an error will result if STNFIL is blank

and TIMSTN does not request more stations.

SFPRMF contains information about the parameters to be included in the file. SF-

PRMF may be either a list of parameters or the name of a packing file. If a list is en-

tered, the parameters must be separated with semicolons; packing information may also

be included after a slash with the minimum and maximum values and the resolution

separated by dashes. For example, to include temperature and dewpoint in a file with-

out packing, SFPRMF may be entered as :

#SFPRMF = TMPC;DWPC

To pack the data, use:

#SFPRMF = TMPC/-127-63-.1;DWPC/-63-63-.1

SFPRMF may also contain the name of a packing file. A packing file for data which is

not to be packed contains a list of parameters with one parameter per line. In a file for

packed data, each line must include the parameter name, the minimum and maximum

data values and the resolution, all separated with spaces. The default packing file for

surface data is SF51.PACK.

## Examples

1. Create a surface file which can contain both decoded and text data called

SURF.DAT with a maximum of 15 times using the default station and

parameter files. Leave room in the file for 100 stations in addition to the

stations in STNFIL. Use the GEMPAK standard packing file and table

file.

```
SFOUTF = surf.dat
SFPRMF = sf51.tbl
STNFIL = stations.tbl
SHIPFL = no
TIMSTN = 15/100
SFFSRC = AIRW|TEXT
```
2. Create a ship file which can contain decoded data only and a maximum

of 1000 ship reports. Use a locally developed parameter file named

SHIP.PRM for packing.

```
SFOUTF = ship.dat
SFPRMF = ship.prm
STNFIL =
SHIPFL = yes
TIMSTN = 1000
SFFSRC = SHIP
```

3. Create a surface file which can contain text data only called SURF.DAT.

Use a locally developed parameter file named SURF.PRM for packing.

```
SFOUTF = ship.dat
SFPRMF = surf.prm
STNFIL =
SHIPFL = no
TIMSTN =
SFFSRC = TEXT
```
### Error Messages

```
[SFCFIL+2] WARNING! ADDSTN was negative -- set to 0.

[SFCFIL+1] Cannot add stn to a ship file; STNFIL ignored.

[SFCFIL-1] Fatal error initializing TAE.

[SFCFIL-2] Fatal error reading TAE parameters.

[SFCFIL-3] Error opening station file ....

[SFCFIL-4] File does not include room for any stations.

[SFCFIL-5] File does not include room for any times.

[SFCFIL-6] SFPRMF is incorrectly specified.

[SFCFIL-7] The output file name is blank.
```

## 4.64 SFCHCK

SFCHCK reads a GEMPAK surface data file and produces a table of stations and an

indicator showing whether each station reported data at each time in the file.



```
SFFILE Surface data file
AREA Data area
DATTIM Date/time
OUTPUT Output device/filename
IDNTYP STNM or STID
STNTYP STN reporting type
```


SFCHCK can be used to quickly determine which stations report data in an individual

GEMPAK surface data file. It is useful for determining which stations do not report

data as well as new stations that are not in the data file.

The header of the table prints out each hour in the file.

Each station's listing appears as a separate row in the table. For each time, a "+" indi-

cates that the station reported data, while a "-" indicates that the station did not report

data. The total number of times that the station reported data appears at the end of the

row.

If the station did not report at any time in the file, its listing will be prepended with an

"N". If the station is not in the station table, its listing will be prepended by a "#".

The DATTIM and AREA parameters can be modified to specify a range of times and /

or regions. Note that AREA must be set to DSET for unlisted stations to appear.

The STNTYP parameter can be modified to specify the reporting characteristics of the

stations. For more information, please consult the STNTYP documentation.

## Examples

1. Generate a table for the most recent decoded surface METAR file. List

all stations in the file. Output the table to the file "sfchck.fil".

```
SFFILE = hrly
AREA = dset
DATTIM = all
OUTPUT = f
IDNTYP = stid
STNTYP = A
```

2. Do the same as above, but only list those stations that ARE in the

station table and do NOT report any data.

```
SFFILE = hrly
AREA = dset
DATTIM = all
OUTPUT = f
IDNTYP = stid
STNTYP = ML
```
3. Do the same as above, but only list those stations that are NOT in the

station table and DO report data.

```
SFFILE = hrly
AREA = dset
DATTIM = all
OUTPUT = f
IDNTYP = stid
STNTYP = RU
```
4. Do the same as (3), but use flash flood guidance data from 4/3/97.

```
SFFILE = $OBS/ffg/970403.ffg
AREA = dset
DATTIM = all
OUTPUT = f
IDNTYP = stid
STNTYP = RU
```
5. List only stations in Vermont for the hours 0300 - 0900 UTC. Output

the table to the screen. Use the current METAR file.

```
SFFILE = hrly
AREA = @VT
DATTIM = /03-09
OUTPUT = T
IDNTYP = stid
STNTYP = A
```
### Error Messages

```
[SFCHCK-1] Fatal error initializing GEMPAK.

[SFCHCK-2] Fatal error reading GEMPAK parameters.

[SFCHCK-3] No valid stations could be found.
```

## 4.65 SFCNTR

SFCNTR plots surface station data on a map and optionally contours one of the fields

being plotted.



```
AREA Data area
GAREA Graphics area
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
SFPARM Surface parameter list
DATTIM Date/time
SFFILE Surface data file
COLORS Color list
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
TITLE Title color/line/title
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
FILTER Filter data factor
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CNTRPRM Parameter to contour
GAMMA Convergence parameter
WEIGHT Barnes Weighting parameter
LINE Color/type/width/label/smth/fltr
CONTUR Subbox/smooth
NPASS Number of passes
CINT Contour interval/min/max
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
```


SFCNTR is a special version of SFMAP which optionally allows the user to select one

of the parameters specified in SFPARM to be contoured using Barnes objective analy-

sis.

SFCNTR uses the mean station spacing as provided in OAGRID in order to define a

suitable number of gridpoints for the analysis. Objective analysis is accomplished us-

ing the Barnes method as given in OABSFC. Finally, contours are drawn as in the GD-

CNTR program.

SFMAP plots data at station locations on a map. Any parameter that can be computed

from the parameters in the data set can be displayed. Conditions can be specified for

the parameters. The conditions are documented in the SFPARM documentation. Data

may be plotted in any valid GEMPAK projection and may be overlaid on images.


A list of times may be given in DATTIM allowing animation of surface plots.

The order of the ### Input Parameters determines their location on the plot. Parameters are

separated by semicolons. A position may be skipped by entering two consecutive semi-

colons or entering the name SPAC or BLNK. A parameter will be plotted centered at

the station if it is the first parameter in the list, for example, if SFPARM = SKYC, the

sky cover symbol will be plotted centered on the station location. If no parameter is to

be displayed centered on the station location, a semicolon must appear before the first

parameter listed. SPAC or BLNK may also be entered. The following chart shows the

location on the model of each position.

#8

#2104

#315

#6117

#9

Note that wind symbols and markers are always plotted at the center of the station mod-

el.

If FILTER is set to YES, the stations will be filtered so that overlapping stations will

not be plotted. FILTER may also be entered as a number greater than or equal to zero.

FILTER = NO has the same effect as FILTER = 0. FILTER = 1 has the same effect as

FILTER = YES. Values less than 1 allow more crowding of stations, values exceeding

1 less crowding. If a position is skipped using two semicolons or if the parameter is

BLNK, the filter will not allocate the space. The parameter SPAC may be used to re-

serve the space with the filter option. Then later plots will plot the same stations after

filtering, provided that the same parameter locations are specified.

If certain stations are not to be removed by the filter, these stations are listed first fol-

lowing an @. The area over which filtering is to occur is specified after a slash. For

example,

```
AREA = @bwi;iad;dca/md
FILTER = YES
```
will display a filtered array of stations over the area corresponding to MD, but BWI,

IAD and DCA will be shown regardless of the filtering.

Weather symbols can also be plotted. The size and width of the symbols can be speci-

fied by appending the numbers to the parameter name using a colon as a separator. For

example, WSYM:2:5 will plot weather symbols with size of 2. and line width of 5.

Other symbols can be specified in a similar way. The names for the pressure tendency,

sky cover, cloud type, weather and wind symbols and how their characteristics are set

are given in the SFPARM documentation.


Parameters can be color-coded based on their own value or on the value of any other

computable parameter. Refer to the COLORS documentation for details. If one or

more parameters are color-coded, a color bar will be displayed for the first color-coded

parameter.

CNTRPRM is the name of one of the parameters specified in SFPARM which will be

contoured from objective analysis of the data. The parameter must be one listed in SF-

PARM. If the used does not want the data values displayed, set COLOR=0 for that SF-

PARM value.

GAMMA is the Barnes objective analysis convergence parameter.

WEIGHT is the Barnes weighting parameter. Typical values range from 20 to 50. A

value of 20 is the default.

LINE provides the settings for contour lines used in the display.

CONTUR is the grid sub-box and number of smoothing passes used by the contouring

algorithm.

NPASS is the number of passes in the Barnes scheme.

CINT is the contour interval for the display.

## Examples

1. Plot the 12Z observerd surface temperatures (tmpf) on a US map and

contour the data using a contour interval of 5 degrees.

```
AREA = dset
GAREA = nj
SATFIL =
RADFIL =
SFPARM = tmpf
DATTIM = 1200
SFFILE = metar
COLORS = 6
MAP = 3/1/1
LATLON = 0
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = lcc
FILTER = n
TEXT = 1/22//hw
LUTFIL =
```

#STNPLT =

#CLRBAR =

```
CNTRPRM = tmpf
GAMMA = .3
WEIGHT = 50
LINE = 5/1/1
CONTUR = 3/3
NPASS = 2
CINT = 5
```
### Error Messages

```
[SFCNTR-1] Fatal error initializing TAE.

[SFCNTR-2] Fatal error reading TAE parameters.

[SFCNTR-3] Fatal error initializing GEMPLT.

[SFCNTR-4] Parameter ... is not calculable.

[SFCNTR-5] Winds are not calculable.
```

## 4.66 SFDELT

SFDELT deletes data from a surface data file.



```
SFFILE Surface data file
DATTIM Date/time
AREA Data area
```


SFDELT deletes data from a surface data file. The stations to be deleted may be spec-

ified in AREA; the times to be deleted are given in DATTIM.

If AREA is set to DSET or ALL, all the data present at the times in DATTIM will be

deleted, along with the headers for those times.

## Examples

1. Delete all the data at time 920722/1200.

```
SFFILE = 92jul.sfc
DATTIM = 920722/1200
AREA = dset
```
2. Delete data at BWI for the most recent time in the file.

```
SFFILE = realtime.sfc
DATTIM = last
AREA = @bwi
```
### Error Messages

```
[SFDELT-1] Fatal error initializing TAE.

[SFDELT-2] Fatal error reading TAE parameters.

[SFDELT-3] Error deleting data at time ....
```

## 4.67 SFEDIT

SFEDIT adds or changes data in a surface file using a sequential text file.



```
SFEFIL Surface edit file
SFFILE Surface data file
```


SFEDIT adds information from a sequential text file to an existing GEMPAK surface

file. The program may be used to add new data to a file or to change existing values.

The output file must already exist.

The data to be added must reside in a text file, SFEFIL. This file can be created by spec-

ifying F as the output device in GEMPAK program SFLIST or by generating the file

with a text editor. A text editor may be used to change the file.

The parameters to be edited must be specified at the beginning

of the edit file, for example: PARM=TMPF;DWPF. Only

parameters stored in the output file can be edited. Parameters that have character val-

ues, such as WTHR, are actually stored as real numbers. Thus, the numeric weather

code must be used. Station information, such as the latitude, SLAT, cannot be changed

using this program. Rather, these changes can be made using the GEMPAK program,

SFSTNS.

The data follow the parameter list. Each data listing must include the time, followed

by the station character or numeric identifier followed by the data. If data are missing,

the current missing data value, -9999., must be used. Blank fields will not be recog-

nized.

If a station or time does not exist in the output file it will be added to the file. If stations

are added, the GEMPAK program SFSTNS can be used to add station latitudes, longi-

tudes and elevations.

## Examples

1. Change values of TMPF and DWPF in the file SFCDATA.DAT.

```
SFEFIL = sflist.fil
SFFILE = sfcdata.dat
```

The file SFLIST.FIL follows. Note that the title line is ignored. PARM =

TMPF;DWPF

#STN YYMMDD/HHMM TMPF DWPF

#BWI 850426/0400 55.40 42.80

#SBY 850426/0400 62.62 55.40

#BGJ 850426/0400 55.40 42.80

The program replaces the temperature and dewpoint for stations BWI, SBY

and NHK at the given time values in output file SFCDATA.DAT.

### Error Messages

```
[SFEDIT+2] Station ... cannot be added to the file.

[SFEDIT-1] Fatal error initializing TAE.

[SFEDIT-2] Fatal error reading TAE parameters.

[SFEDIT-3] Edit file ... cannot be opened.

[SFEDIT-5] PARM keyword not found.

[SFEDIT-6] Too many parameters to edit.

[SFEDIT-7] Time ... cannot be added to data set.

[SFEDIT-8] Parameter ... is not in the data set.

[SFEDIT-9] SFEDIT cannot continue with invalid parameters.

[SFEDIT-10] Missing lat/lon pair for ship file
```

## 4.68 SFGRAM

SFGRAM draws a meteogram for surface data.



```
SFFILE Surface data file
DATTIM Date/time
STATION Stations
TRACE1 Parms/colors/range/witness
TRACE2 Parms/colors/range/witness
TRACE3 Parms/colors/range/witness
TRACE4 Parms/colors/range/witness
TRACE5 Parms/colors/range/witness
NTRACE Number of traces
TAXIS Time1-time2-tinc;lbl;gln;tck
BORDER Background color/type/width
MARKER Marker color/type/size/width/hw
TITLE Title color/line/title
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
```


SFGRAM draws a time series plot for surface data. Up to five traces may be drawn in

a single plot. NTRACE determines the number of traces to draw. If NTRACE is 5, the

plot area will be divided into fifths; if NTRACE is 4, the plot area will be divided into

fourths, etc. If STATION is a single station, all the traces will be for that station. If

STATION is a list of stations, TRACE1 will plot data from the first station, TRACE2

will plot data from the second station, etc.

Each TRACE parameter contains specifications for the parameters to be plotted in the

corresponding trace. The format for each trace is:

parms/colors/range/witness!parms/colors/range/witness

The parameters before the! will be labeled on the left of the plot; those after the! will

be labeled on the right. Any GEMPAK surface parameter may be entered. Real valued

parameters will be drawn as a graph. Character valued parameters will be rotated 90

degrees and written on the plot. Symbols are plotted for the symbol parameters. For

GUST and GUMS, the character, G, will be plotted.

Up to four parameters may be plotted along each axis. The parameters must be sepa-

rated using semicolons. Character and symbols may only be plotted in positions 1, 2 or

3, where position 1 is at the bottom of the trace, position 2 is in the middle and position

3 is at the top.


Each parameter name may be followed by a colon, the size or line dash type, a second

colon, and the width. For example, WSYM:.5:5 will draw weather symbols half the

default size with a line width of 5. TMPF:3 will plot a temperature line using dash pat-

tern 3.

The colors for the parameters must also be separated using semicolons. If a single num-

ber is entered, all parameters are drawn in that color. If a zero is entered, the current

default color is used.

The range specifies the scaling of the y-axis. The format is:

```
start;stop;increment
```
Note that, in this program, the parts of range must be separated using semicolons. If no

range is given, it is selected using the data values. If start = stop, then the y-axis range

is from the mean of the data minus this value to the mean plus the start = stop value. If

the start or the stop value (or both) is preceded by a + sign, then the y-axis range is de-

termined by that value unless the data actually goes past it, in which case, the y-axis is

extended to plot all of the data.

Witness lines may be specified for each TRACE. These are horizontal dotted lines. A

list of y values, separated by semicolons, may be entered. Alternatively, if WITNESS

= YES, a witness line will be centered on the plot.

The time axis is specified in TAXIS as a minimum time, a maximum time, and a time

increment separated with dashes. If any or all of the parts of TAXIS is blank, reason-

able values will be selected by the program.

## Examples

1. Plot the meteogram for PWM for all the times available in the data set.

Plot the time axis from 19/03 to 20/09. TRACE1 plots TMPF and

DWPF in colors 2 and 3. TRACE2 plots PMSL in color 4. TRACE3

plots the wind barbs and gusts in knots. TRACE4 plots the visibility in

miles in color 7. TRACE5 plots the cloud cover codes at the bottom and

the weather symbols at the top in color 6.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
DATTIM = all
STATION = pwm
TRACE1 = tmpf;dwpf:3/2;3
TRACE2 = pmsl/4
TRACE3 = gust;brbk
TRACE4 = vsby/7
TRACE5 = clds;;wsym/6
NTRACE = 5
TAXIS = 19/03-20/09
BORDER = 1
```

#MARKER = 3/12

#TITLE = 1

```
CLEAR = y
DEVICE = xw
PANEL = 0
TEXT = 1///sw
```
2. Plot TMPF and DWPF for stations PWM, BWI and HAT. TMPF is

plotted with a solid line using color 2 and DWPF is plotted with a

dashed line using color 3.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
DATTIM = all
STATION = pwm;bwi;hat
TRACE1 = tmpf;dwpf:3/2;3
TRACE2 = tmpf;dwpf:3/2;3
TRACE3 = tmpf;dwpf:3/2;3
TRACE4 =
TRACE5 =
NTRACE = 3
TAXIS = 19/03-20/09
BORDER = 1
MARKER = 3/12
TITLE = 1
CLEAR = y
DEVICE = xw
PANEL = 0
TEXT = 1///sw
```

### Error Messages

```
[SFGRAM+3] Hardware text cannot generally be rotated.

[SFGRAM-1] Fatal error initializing TAE.

[SFGRAM-2] Fatal error reading TAE parameters.

[SFGRAM-3] Fatal error initializing GEMPLT.

[SFGRAM-4] No stations have been entered.

[SFGRAM-5] ... cannot be plotted in position 4.

[SFGRAM-6] The time range along the x axis is 0.

[SFGRAM-7] The parameter ... cannot be computed.

[SFGRAM-8] The graph coordinates are invalid.

[SFGRAM-9] There is no data at station ....

[SFGRAM-10] Station ... is invalid.

[SFGRAM-11] There are no times in the file.

[SFGRAM-12] The time ... is invalid.

[SFGRAM-14] The file ... cannot be opened.

[SFGRAM-15] There are no parameters specified.

[SFGRAM-16] Error in specifying TAXIS.

[SFGRAM-17] No valid time or file found.
```

## 4.69 SFL604

SFL604 lists data from a GEMPAK surface file in a fixed format.



```
SFFILE Surface data file
AREA Data area
DATTIM Date/time
OUTPUT Output device/filename
SKPMIS Skip missing data flag
IDNTYP STNM or STID
SFPARM Surface parameter list
```


SFL604 lists data from a surface file in a format designed to display airways data.

Data will be listed for the stations and times requested in AREA and DATTIM. If SKP-

MIS is YES, stations that are within the area requested, but have not reported data, will

not be listed. If SKPMIS is set to NO, then only the date and station identifier will be

listed for nonreporting stations.

Stations may be identified in the output listing by character or numeric identifier. This

is accomplished by setting IDNTYP to STID or STNM.

This program will list predefined parameters only. However, stations may be selected

using conditions in SFPARM. For example, if SFPARM = WTHR=S+, then only those

stations reporting heavy snow will be listed. The conditions specified in SFPARM can

be for any computable parameter, not just those to be listed.

## Examples

1. List the data for all stations in Pennsylvania at the most recent time.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
AREA = @pa
DATTIM = last
OUTPUT = t
SKPMIS = yes
IDNTYP = stid
SFPARM =
```
2. List the data for all times at PHL.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
AREA = @phl
DATTIM = all
OUTPUT = t
```

```
SKPMIS = yes
IDNTYP = stid
SFPARM =
```
3. List all stations in Pennsylvania, including nonreporting stations for the

last time.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
AREA = @pa
DATTIM = last
OUTPUT = t
SKPMIS = no
IDNTYP = STID
SFPARM =
```
4. List all stations in Pennsylvania where there are reported thunderstorms.

#SFFILE = $GEMDATA/HRCBOB.SFC

```
AREA = @pa
DATTIM = LAST
OUTPUT = t
SKPMIS = yes
IDNTYP = STID
SFPARM = wthr=t
```
### Error Messages

```
[SFL604+1] Parameter ... cannot be computed.

[SFL604-1] Fatal error initializing TAE.

[SFL604-2] Fatal error reading TAE parameters.

[SFL604-3] No valid stations could be found.

[SFL604-4] No parameters can be computed.
```

## 4.70 SFLIST

SFLIST lists surface data from a GEMPAK surface data file.



```
SFFILE Surface data file
AREA Data area
DATTIM Date/time
SFPARM Surface parameter list
OUTPUT Output device/filename
IDNTYP STNM or STID
```


SFLIST lists any parameters which can be derived from the data in a surface data file.

The stations and times to be included are specified in AREA and DATTIM. If no data

are reported for a station, that station will not be listed. The listings will be grouped by

time.

If the surface file contains both decoded and undecoded text data, both types can be list-

ed by specifying GEMPAK parameters along with parameter 'TEXT'. The special re-

ports may also be listed in an undecoded format using the paramter 'SPCL'. TEXT and

SPCL may appear anywhere in the list of parameters.

Conditions can be specified for the parameters. The conditions are documented in the

SFPARM documentation.

## Examples

1. List the decoded and undecoded text air and dewpoint temperatures in

Fahrenheit and the mean sea-level pressure and weather of stations in

Pennsylvania for the latest time. Use a locally created surface file that

contains both types of data called SAOTEXT.SFC.

#SFFILE = SAOTEXT.SFC

```
AREA = @pa
DATTIM = last
SFPARM = tmpf;dwpf;pmsl;wthr;text
OUTPUT = t
IDNTYP = stid
```
2. List the same parameters for PIT and BWI at 0500 and 0600 GMT.

#SFFILE = SAOTEXT.SFC

```
AREA = @pit;bwi
DATTIM = /05-06
SFPARM = tmpf;dwpf;pmsl;wthr;text
OUTPUT = t
```

```
IDNTYP = stid
```
3. List the temperature, dewpoint, pressure, and weather for stations in

Pennsylvania which are reporting thunderstorms and dewpoint temperatures

greater than 65 degrees Fahrenheit.

#SFFILE = $GEMDATA/HRCBOB.SFC

#AREA = @PA

#DATTIM = /05-06

```
SFPARM = tmpf;dwpf>65;pmsl;wthr=t
OUTPUT = T
IDNTYP = STID
```
4. List the latest undecoded SAO text data for stations in Pennsylvania.

Use a locally created surface file with SAO decoded and text data called

SAOTEXT.SFC.

#SFFILE = SAOTEXT.SFC

#AREA = @PA

```
DATTIM = last
SFPARM = text;spcl
OUTPUT = T
IDNTYP = STID
```
### Error Messages

```
[SFLIST+1] Parameter ... is not computable.

[SFLIST-1] Fatal error initializing TAE.

[SFLIST-2] Fatal error reading TAE parameters.

[SFLIST-3] No stations reporting data.

[SFLIST-4] No valid computable parameters.
```

## 4.71 SFMAP

SFMAP plots surface station data on a map.



```
AREA Data area
GAREA Graphics area
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
SFPARM Surface parameter list
DATTIM Date/time
SFFILE Surface data file
COLORS Color list
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
TITLE Title color/line/title
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
FILTER Filter data factor
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
```


SFMAP plots data at station locations on a map. Any parameter that can be computed

from the parameters in the data set can be displayed. Conditions can be specified for

the parameters. The conditions are documented in the SFPARM documentation. Data

may be plotted in any valid GEMPAK projection and may be overlaid on images.

A list of times may be given in DATTIM allowing animation of surface plots.

The order of the ### Input Parameters determines their location on the plot. Parameters are

separated by semicolons. A position may be skipped by entering two consecutive semi-

colons or entering the name SPAC or BLNK. A parameter will be plotted centered at

the station if it is the first parameter in the list, for example, if SFPARM = SKYC, the

sky cover symbol will be plotted centered on the station location. If no parameter is to

be displayed centered on the station location, a semicolon must appear before the first

parameter listed. SPAC or BLNK may also be entered. The following chart shows the

location on the model of each position.

#8

#2104

#315

#6117

#9


Note that wind symbols and markers are always plotted at the center of the station mod-

el.

If FILTER is set to YES, the stations will be filtered so that overlapping stations will

not be plotted. FILTER may also be entered as a number greater than or equal to zero.

FILTER = NO has the same effect as FILTER = 0. FILTER = 1 has the same effect as

FILTER = YES. Values less than 1 allow more crowding of stations, values exceeding

1 less crowding. If a position is skipped using two semicolons or if the parameter is

BLNK, the filter will not allocate the space. The parameter SPAC may be used to re-

serve the space with the filter option. Then later plots will plot the same stations after

filtering, provided that the same parameter locations are specified.

If certain stations are not to be removed by the filter, these stations are listed first fol-

lowing an @. The area over which filtering is to occur is specified after a slash. For

example,

```
AREA = @bwi;iad;dca/md
FILTER = YES
```
will display a filtered array of stations over the area corresponding to MD, but BWI,

IAD and DCA will be shown regardless of the filtering.

Weather symbols can also be plotted. The size and width of the symbols can be speci-

fied by appending the numbers to the parameter name using a colon as a separator. For

example, WSYM:2:5 will plot weather symbols with size of 2. and line width of 5.

Other symbols can be specified in a similar way. The names for the pressure tendency,

sky cover, cloud type, weather and wind symbols and how their characteristics are set

are given in the SFPARM documentation.

Parameters can be color-coded based on their own value or on the value of any other

computable parameter. Refer to the COLORS documentation for details. If one or

more parameters are color-coded, a color bar will be displayed for the first color-coded

parameter.

## Examples

1. Display the visible satellite image from 18Z and overlay the surface data

for the area covered by the image. Plot a standard station model for

each location in the data set. The surface data to plot include: 1) sky

cover symbol; 2) temperature in Fahrenheit; 3) weather symbol; 4) coded

mean sea level pressure; 5) pressure tendency with symbol; 6) dewpoint in

Fahrenheit; 6) station ID; 7) visibility; and 8) wind barbs in knots. The

data are plotted using the specified color list.


```
AREA = us
GAREA = dset
SATFIL = $GEMDATA/VIS_910819_1801
RADFIL =
IMCBAR = 1/V/LL/0;.05/.90
SFPARM = skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;m
DATTIM = 910819/1800
SFFILE = $GEMDATA/hrcbob.sfc
COLORS = 26;2;7;25;20;22;18;24;6
MAP = 1
LATLON = 2/10/1/1/5;5
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = sat
FILTER = 1
TEXT = 1/22//hw
LUTFIL =
STNPLT =
CLRBAR =
```
2. Using the above specifications, change the area to New Jersey. The

satellite image will be subset and replotted with the data for that area.

Change the filter factor to allow more stations to be plotted.

```
AREA = nj-
GAREA = nj
SATFIL = $GEMDATA/VIS_910819_1801
RADFIL =
IMCBAR = 1/V/LL/0;.05/.90
SFPARM = skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;m
DATTIM = 910819/1800
SFFILE = $GEMDATA/hrcbob.sfc
COLORS = 26;2;7;25;20;22;18;24;6
MAP = 1
LATLON = 2/10/1/1/5;5
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = sat
FILTER = .45
TEXT = 1/22//hw
LUTFIL =
STNPLT =
CLRBAR =
```
3. Using the previous specifications, vary the color of the weather symbols

by color coding them according to their precipitation type. To make them

more discernible, omit using the satellite image and change the map

projection.


```
AREA = nj-
GAREA = nj
SATFIL =
RADFIL =
IMCBAR =
SFPARM = skyc:.75;tmpf;wsym:.75;smsl;ptnd;dwpf;stid;;vsby*10;brbk:1:2;m
DATTIM = 910819/1800
SFFILE = $GEMDATA/hrcbob.sfc
COLORS = 26;2;(19-90-10/17;6;8;17;3;3;1;22;2);25;20;22;18;24;6
MAP = 1
LATLON = 2/10/1/1/5;5
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = mer
FILTER = .45
TEXT = 1/22//hw
LUTFIL =
STNPLT =
CLABAR =
```
### Error Messages

```
[SFMAP-1] Fatal error initializing TAE.

[SFMAP-2] Fatal error reading TAE parameters.

[SFMAP-3] Fatal error initializing GEMPLT.

[SFMAP-4] Parameter ... is not calculable.

[SFMAP-5] Winds are not calculable.
```

## 4.72 SFMOD

SFMOD moves selected surface data from an input surface file to an output surface file.



```
SFFILE Surface data file
SFOUTF Output surface file
DATTIM Date/time
DATOUT Output date/time
AREA Data area
SFPARM Surface parameter list
```


SFMOD takes data from an existing GEMPAK surface file, SFFILE, and writes the

data into another existing surface file, SFOUTF. This program can be used to subset

the original dataset by time and/or stations and/or parameters.

This program will not create a new surface file. The GEMPAK program, SFCFIL, can

be used to create a surface file. If the parameters in the output file are not the same as

the parameters in the input file, the required parameter conversions will be done.

If a requested station or time is not in the output file, it will be added to the file if there

is room. The number of times and stations that can be included in a file is specified by

TIMSTN in SFCFIL.

The parameter DATOUT can be used to set different times in the output file from those

times listed with the parameter DATTIM.

The parameter SFPARM can be used to specify which parameters in the output file are

to be written.

## Examples

1. Put the data for all stations at the latest time in file TEST.SFC into a file

called LAST.SFC.

```
SFFILE = test.sfc
SFOUTF = last.sfc
DATTIM = last
AREA = dset
```
2. Put the data for stations in Maryland and Virginia for all times into the

output data set.

```
SFFILE = test.sfc
SFOUTF = last.sfc
```

```
DATTIM = all
AREA = @md/@va
```
3. Put the data for stations in Maryland for 1100 - 1400 UTC into the

output file with the new times from 1500 - 1800 UTC. Write only the

temperature and dewpoint data to the output file.

```
SFFILE = test.sfc
SFOUTF = last.sfc
DATTIM = 940513/1100;940513/1200;940513/1300;940513/1400
DATOUT = 0513/1500;0513/1600;0513/1700;0513/1800
AREA = @md
SFPARM = tmpf;dwpf
```
### Error Messages

```
[SFMOD+4] Parameter ... is not in output file.

[SFMOD+3] Some stations were not added to file.

[SFMOD+2] Character parameter cannot be used: ...

[SFMOD+1] Parameter ... cannot be calculated.

[SFMOD-1] Fatal error initializing TAE.

[SFMOD-2] Fatal error reading TAE parameters.

[SFMOD-3] There are no parameters to be computed.

[SFMOD-4] Time ... cannot be added.

[SFMOD-5] Number of input and output date/time are not equal.
```

## 4.73 SFSTNS

SFSTNS modifies the station information in a surface file.



```
SFFILE Surface data file
STNFIL Station information file
ADDSTN Add station flag
IDNTYP STNM or STID
```


SFSTNS updates the station information in a GEMPAK surface file. The station infor-

mation includes the character station identifier, STID, the station number, STNM, the

latitude, SLAT, the longitude, SLON, the elevation, SELV, the state, STAT, and the

country, COUN.

This information must be stored in a fixed format in the table file specified in STNFIL.

See the default table for an example of the required format. Station names may be in-

cluded, but are not used. The current default GEMPAK surface station table for US,

Canadian and Mexican stations is SFSTNS.TBL.

ADDSTN is a logical parameter that indicates whether stations which are in the table

file but not already in the surface file will also be added to the surface file, provided

there is room for them. To create a surface data file with enough room to allow for sta-

tions to be added, run the program SFCFIL. Set the second parameter for TIMSTN to

be large enough to hold the number of stations to be added to the surface file.

IDNTYP governs whether station numbers, STNM, or character identifiers, STID, will

be used to identify stations in the table.

## Examples

1. Update surface file called SURF.DAT with station information from

MYSTN.DAT, adding stations that are not already in the surface file.

```
SFFILE = surf.dat
STNFIL = mystn.dat
ADDSTN = yes
IDNTYP = stid
```

### Error Messages

```
[SFSTNS+1] WARNING! No stations were updated.

[SFSTNS-1] Fatal error initializing TAE.

[SFSTNS-2] Fatal error reading TAE parameters.

[SFSTNS-3] Invalid input for IDNTYP; must be STID or STNM.

[SFSTNS-4] STNFIL ... cannot be opened.
```

## 4.74 SFVGSF

SFVGSF adds or changes data in a surface file using the elements found in a Vector

Graphics file.



```
VGFILE Vgfile | scale file | attribute file
SFOUTF Output surface file
DATTIM Date/time
SFPARM Surface parameter list
COLORS Color list
```


SFVGSF adds information from a Vector Graphics file to an existing GEMPAK sur-

face file. The program may be used to add new data to a file or to change existing val-

ues. The output file must already exist.

Each parameter in the Vector Graphics file must have a unique color because the search

method uses the color to distinguish between parameters. The colors listed in COLORS

must correspond to the colors used to create the Vector Graphics file, so that the param-

eter names can be matched to the data values.

The Vector Graphics file can be created by specifying VG as the device driver in the

GEMPAK program SFMAP or by generating the file in the product generation of

NMAP.

The parameters to be added to the surface file are specified in SFPARM and the asso-

ciated colors are listed in COLORS. The STID or STNM must be present in the VG

file and the parameter list so that the data can be written to the proper station.

The date/time must also be provided by the user. Any missing parts of the date/time

will be supplied by the system time.

If a station or time does not exist in the output file, it will be added to the file. If stations

are added, the GEMPAK program SFSTNS can be used to add station latitudes, longi-

tudes and elevations.

## Examples

1. Read the Vector Graphics file day3max.vgf and write the data for the

requested parameters to the surface file 990307.mrfmos. In this example,

TDYE is the edited maximum temperature value.

```
VGFILE = day3max.vgf
SFOUTF = 990307.mrfmos
```

#DATTIM = 990307/1200

#SFPARM = TDYE;STID

#COLORS = 2;17

### Error Messages

```
[SFVGSF+3] Parameter ... is not in the data set.

[SFVGSF+2] Station ... cannot be added to the file.

[SFVGSF-1] Fatal error initializing TAE.

[SFVGSF-2] Fatal error reading TAE parameters.

[SFVGSF-3] VG file ... cannot be opened.

[SFVGSF-4] The output file must be a standard SF file.

[SFVGSF-5] One of the parameters must be STID or STNM.

[SFVGSF-6] Time ... cannot be added to data set.
```

## 4.75 SNCFIL

SNCFIL creates a new GEMPAK sounding file.



```
SNOUTF Output sounding file
SNPRMF Sounding parameter packing file
STNFIL Station information file
MRGDAT Merge data file flag/part type
TIMSTN Times/additional stations
```


SNCFIL creates a GEMPAK sounding file. The file may be created to store merged or

unmerged data. Merged data files may contain any meteorological parameters with all

parameters included at each level. Unmerged files store the mandatory data and signif-

icant-level temperature and wind data as separate parts.

MRGDAT must be set to YES or NO. If YES, a merged file will be created. The pa-

rameters to be included in the file must be specified in SNPRMF. If MRGDAT is NO,

an unmerged file will be created. In this case, the type of unmerged file can be included

following a slash. Type 1 creates a file with only mandatory data below 100 mb. Type

2 specifies a file with mandatory and significant-level data below 100 mb, and type 3,

which is the default, specifies a file with mandatory and significant-level data below

and above 100 mb.

SNPRMF contains information about the parameters to be included in a merged file.

Either a list of parameters or the name of a packing file may be entered. If a list is en-

tered, the parameters must be separated with semicolons; packing information may also

be included after a slash with the minimum and maximum values and the resolution

separated using dashes. For example, to include temperature and dewpoint in a file

without packing, SNPRMF may be entered as:

#SNPRMF = TMPC;DWPC

To pack the data, use:

#SNPRMF = TMPC/-127-63-.1;DWPC/-63-63-.1

SNPRMF may also contain the name of a packing file. A packing file for data which

is not to be packed contains a list of parameters with one parameter per line. In a file

for packed data, each line must include the parameter name, the minimum and maxi-

mum data values and the resolution, all separated with spaces. The default packing file

for merged sounding data is SNPACK.TBL.

If STNFIL is not blank, information about all the stations in STNFIL will be added to

the data set. Space will be left in the file for the additional number of stations specified


as the second parameter in TIMSTN. Note that an error will result if STNFIL is blank

and TIMSTN does not request additional stations.

## Examples

1. Create a sounding file called SOUND.DAT containing merged data. Use

the standard station and parameter files. Leave room in the file for 100

stations in addition to the stations in STNFIL and allow 15 times to be

added to the file.

```
SNOUTF = sound.dat
SNPRMF = snpack.tbl
STNFIL = snstns.tbl
MRGDAT = yes
TIMSTN = 15/100
```
2. Create an unmerged sounding file called WEEKLY.SND which will store

mandatory and significant-level data below and above 100 mb. Do not

add any stations now. Leave room for 14 times and 300 stations.

```
SNOUTF = weekly.snd
SNPRMF =
STNFIL =
MRGDAT = no/3
TIMSTN = 14/300
```
### Error Messages

```
[SNCFIL+2] Negative number of stations entered -- 0 used.

[SNCFIL-1] Fatal error initializing TAE.

[SNCFIL-2] Fatal error reading TAE parameters.

[SNCFIL-3] Error opening station file ....

[SNCFIL-4] File does not include room for any stations.

[SNCFIL-5] File does not include room for any times.

[SNCFIL-6] SNPRMF is incorrectly specified.

[SNCFIL-7] The file name for the new file is blank.

[SNCFIL-8] Vertical coordinate = ... -- specify more parameters.
```

## 4.76 SNCROSS

SNCROSS draws cross sections through sounding data.



```
CXSTNS Cross-section station line
SNPARM Sounding parameter list
SNFILE Sounding data file
DATTIM Date/time
VCOORD Vertical coordinate type
PTYPE Plot type/h:w ratio/margins
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
TAXIS Time1-time2-tinc;lbl;gln;tck
LINE Color/type/width/label/smth/fltr
BORDER Background color/type/width
CINT Contour interval/min/max
WIND Wind symbol/siz/wdth/typ/hdsz
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
CLEAR Clear screen flag
FILTER Filter data factor
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CURVE Curve fit type
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CONTUR Subbox/smooth
FINT Fill interval/min/max
FLINE Fill colors/fill types
CTYPE Contour type: C/F
```


SNCROSS draws cross sections using upper-air sounding data. The cross-section line,

CXSTNS, must be specified as a list of stations separated with semicolons. Alterna-

tively, a single station may be entered in CXSTNS and a list of times in DATTIM. In

this case, a time section will be drawn.

Any parameter that can be computed from the data set parameters can be displayed.

The parameter to be displayed is specified in SNPARM. If the value of SNPARM is

ISEN, isentropes will be drawn. Note that SNPARM may also be THTA, in which

case, potential temperature will be gridded and contoured. ISEN will fit splines to the

station data in plot space and then check for tangled lines and untangle them if neces-

sary.

Contours may be displayed as lines or as a color fill. If CTYPE is C, contour lines are

drawn using input from CINT and LINE. If CTYPE is F, filled contours are drawn us-

ing specifications from FINT and FLINE. Both contour lines and filled contours are

drawn if CTYPE is F/C.


The attributes of the contour lines, including the color, line type, line width, and label

frequency are specified in LINE. The four attributes are separated with slashes; semi-

colons separate the values for each attribute. If the line type is set to a single negative

number, negative contour values will have the absolute value of the line type and pos-

itive values will be solid. If the label type is set to a single number, n, then every nth

value will be labeled.

The contour fill intervals are specified in FINT; the attributes for the fill are specified

in FLINE. The first color specified in FLINE fills values less than the first level; while

the last color fills values greater than the last level. Therefore, n levels require n+1 col-

ors. CLRBAR allows a color bar to be added for color fill contours.

A range of colors may be specified in either FLINE or LINE by specifying starting, end-

ing and increment values in that order separated by dashes. If the increment is missing,

a default of 1 is used.

The fill type may be set to 1 (solid), 2 (slanted dash) or 3 (slanted line). If fill type is

set to 0, solid fill is used. If the fill type is set to a single negative number, negative

values will use the absolute value of the fill type, and positive values will be solid.

The plot background consists of a pressure axis, a horizontal axis with the station

names, a filled region indicating the part of the plot below the earth surface, and vertical

lines at each station. The station lines are the specified line type up to the level at which

wind data stop, and are dotted from there to the level at which temperature data stop.

The color and other attributes for the background axes and labels are given by the first

numbers separated by semicolons in the color number, line type and line width entry

sections for BORDER. The second set of numbers applies to the station lines, and the

third set to the underground color fill (for which the line type and width do not apply).

To eliminate, the background, the station lines, or the underground fill, just set the cor-

responding color number to zero. If one color number is entered, it is used for all three;

if two are entered, the second is used for both the station lines and the underground fill.

So, BORDER has the following entries:

background color; station line color; underground fill color / background line type; sta-

tion line type / background line width; station line width

The horizontal axis represents a straight line between the first and last stations. The po-

sitions of intervening stations are proportional to the perpendicular projections of the

actual positions onto the section line. All of these calculations are done in lat/lon coor-

dinates. If the plot is a time section, the times will be displayed on the x axis with the

earliest time at the left. If the first character in TAXIS is an R, the earliest time will

appear on the right.

The vertical coordinate may be specified as LIN, LOG, or STUVE; SKEWT is not valid

in this program. The bottom and top limits for the y axis are specified in YAXIS, but

the axis labeling specifications are ignored.


## Examples

1. Draw isentropes in color 3 for the last time in the data set. Plot the

background in color 1. Use a logarithmic scale for the y axis and a 5

degree interval for the isentropes. Draw wind barbs in color number 6

with line width 2.

```
CXSTNS = chh;acy;wal;hat;chs;ays
SNPARM = thta
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = last
VCOORD = pres
PTYPE = log
YAXIS =
TAXIS =
LINE = 3
BORDER = 1
CINT = 5
WIND = bm6//2
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR =
CONTUR = 0
FINT = 0
FLINE = 10-20
CTYPE = C
```
2. Draw filled contours of relative humidity for the same cross section. Fill

in contours greater than 70%. Draw a horizontal color bar at the bottom

of the plot. Draw the cross section from 1050 mb to 300 mb.

```
CXSTNS = chh;acy;wal;hat;chs;ays
SNPARM = relh
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = last
VCOORD = pres
PTYPE = log/.5
YAXIS = 1050/300
TAXIS =
LINE = 3
BORDER = 1
CINT = 5
WIND = bm6//2
TITLE = 1
PANEL = 0
```

```
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR = 1/h
CONTUR = 0
FINT = 70;80;90
FLINE = 0;21;22;23
CTYPE = f
```
3. Draw a time section of relative humidity for ACY. Reverse the time

axis to plot latest time on the left.

```
CXSTNS = acy
SNPARM = relh
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = all
VCOORD = pres
PTYPE = log/.5
YAXIS = 1050/300
TAXIS = r
LINE = 3
BORDER = 1
CINT = 5
WIND = bm6//2
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR = 1/h
CONTUR = 0
FINT = 70;80;90
FLINE = 0;21;22;23
CTYPE = f
```

### Error Messages

```
[SNCROSS-1] Fatal error initializing TAE.

[SNCROSS-2] Fatal error reading TAE parameters.

[SNCROSS-3] GEMPLT initialization error.

[SNCROSS-4] Vertical coordinate for isentropes must be PRES.

[SNCROSS-5] There are no times in the file.

[SNCROSS-6] Fewer than four stations/times were selected.

[SNCROSS-7] Data buffer is too small.

[SNCROSS-8] Temperature or pressure data not available.

[SNCROSS-9] The station ... cannot be found in the data set.

[SNCROSS-10] Error setting up graph; check invalid LOG axis.

[SNCROSS-11] Input ... for PTYPE is invalid.

[SNCROSS-12] Either input ... for YAXIS or input for VCOORD is invalid.

[SNCROSS-13] Parameter ... is not computable.

[SNCROSS-14] Parameter ... is a character.

[SNCROSS-15] The grid coordinates cannot be defined.

[SNCROSS-16] Multiple station entry is invalid for time sections.

[SNCROSS-17] Data at time ... is not in the file.

[SNCROSS-18] Wind data cannot be computed.
```

## 4.77 SNCROSS2

SNCROSS2 draws cross sections through sounding data.



```
CXSTNS Cross-section station line
SNPARM Sounding parameter list
SNFILE Sounding data file
DATTIM Date/time
VCOORD Vertical coordinate type
PTYPE Plot type/h:w ratio/margins
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
TAXIS Time1-time2-tinc;lbl;gln;tck
LINE Color/type/width/label/smth/fltr
BORDER Background color/type/width
CINT Contour interval/min/max
WIND Wind symbol/siz/wdth/typ/hdsz
TITLE Title color/line/title
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
CLEAR Clear screen flag
FILTER Filter data factor
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
CURVE Curve fit type
CLRBAR Color/ornt/anch/x;y/ln;wd/freq|text_info
CONTUR Subbox/smooth
FINT Fill interval/min/max
FLINE Fill colors/fill types
CTYPE Contour type: C/F
```


SNCROSS2 is a modification of SNCROSS to allow plotting of variable color wind

staffs. A range of colors may be entered in the WIND parameter definition, eg: WIND

= bk30-7.

SNCROSS2 draws cross sections using upper-air sounding data. The cross-section

line, CXSTNS, must be specified as a list of stations separated with semicolons. Alter-

natively, a single station may be entered in CXSTNS and a list of times in DATTIM.

In this case, a time section will be drawn.

Any parameter that can be computed from the data set parameters can be displayed.

The parameter to be displayed is specified in SNPARM. If the value of SNPARM is

ISEN, isentropes will be drawn. Note that SNPARM may also be THTA, in which

case, potential temperature will be gridded and contoured. ISEN will fit splines to the

station data in plot space and then check for tangled lines and untangle them if neces-

sary.


Contours may be displayed as lines or as a color fill. If CTYPE is C, contour lines are

drawn using input from CINT and LINE. If CTYPE is F, filled contours are drawn us-

ing specifications from FINT and FLINE. Both contour lines and filled contours are

drawn if CTYPE is F/C.

The attributes of the contour lines, including the color, line type, line width, and label

frequency are specified in LINE. The four attributes are separated with slashes; semi-

colons separate the values for each attribute. If the line type is set to a single negative

number, negative contour values will have the absolute value of the line type and pos-

itive values will be solid. If the label type is set to a single number, n, then every nth

value will be labeled.

The contour fill intervals are specified in FINT; the attributes for the fill are specified

in FLINE. The first color specified in FLINE fills values less than the first level; while

the last color fills values greater than the last level. Therefore, n levels require n+1 col-

ors. CLRBAR allows a color bar to be added for color fill contours.

A range of colors may be specified in either FLINE or LINE by specifying starting, end-

ing and increment values in that order separated by dashes. If the increment is missing,

a default of 1 is used.

The plot background consists of a pressure axis, a horizontal axis with the station

names, a filled region indicating the part of the plot below the earth surface, and vertical

lines at each station. The station lines are the specified line type up to the level at which

wind data stop, and are dotted from there to the level at which temperature data stop.

The color and other attributes for the background axes and labels are given by the first

numbers separated by semicolons in the color number, line type and line width entry

sections for BORDER. The second set of numbers applies to the station lines, and the

third set to the underground color fill (for which the line type and width do not apply).

To eliminate, the background, the station lines, or the underground fill, just set the cor-

responding color number to zero. If one color number is entered, it is used for all three;

if two are entered, the second is used for both the station lines and the underground fill.

So, BORDER has the following entries:

background color; station line color; underground fill color / background line type; sta-

tion line type / background line width; station line width

The horizontal axis represents a straight line between the first and last stations. The po-

sitions of intervening stations are proportional to the perpendicular projections of the

actual positions onto the section line. All of these calculations are done in lat/lon coor-

dinates. If the plot is a time section, the times will be displayed on the x axis with the

earliest time at the left. If the first character in TAXIS is an R, the earliest time will

appear on the right.


The vertical coordinate may be specified as LIN, LOG, or STUVE; SKEWT is not valid

in this program. The bottom and top limits for the y axis are specified in YAXIS, but

the axis labeling specifications are ignored.

## Examples

1. Draw isentropes in color 3 for the last time in the data set. Plot the

background in color 1. Use a logarithmic scale for the y axis and a 5

degree interval for the isentropes. Draw wind barbs in color number 6

with line width 2.

```
CXSTNS = chh;acy;wal;hat;chs;ays
SNPARM = thta
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = last
VCOORD = pres
PTYPE = log
YAXIS =
TAXIS =
LINE = 3
BORDER = 1
CINT = 5
WIND = bm6//2
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR =
CONTUR = 0
FINT = 0
FLINE = 10-20
CTYPE = C
```
2. Draw filled contours of relative humidity for the same cross section. Fill

in contours greater than 70%. Draw a horizontal color bar at the bottom

of the plot. Draw the cross section from 1050 mb to 300 mb.

```
CXSTNS = chh;acy;wal;hat;chs;ays
SNPARM = relh
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = last
VCOORD = pres
PTYPE = log/.5
YAXIS = 1050/300
TAXIS =
LINE = 3
BORDER = 1
CINT = 5
```

```
WIND = bm6//2
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR = 1/h
CONTUR = 0
FINT = 70;80;90
FLINE = 0;21;22;23
CTYPE = f
```
3. Draw a time section of relative humidity for ACY. Reverse the time

axis to plot latest time on the left.

```
CXSTNS = acy
SNPARM = relh
SNFILE = $GEMDATA/HRCBOB.SND
DATTIM = all
VCOORD = pres
PTYPE = log/.5
YAXIS = 1050/300
TAXIS = r
LINE = 3
BORDER = 1
CINT = 5
WIND = bm6//2
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .8
TEXT = 1
CURVE = 2
CLRBAR = 1/h
CONTUR = 0
FINT = 70;80;90
FLINE = 0;21;22;23
CTYPE = f
```
4. Plot 6 minute profiler data at VCIO2, using color coded windbarbs. Use

the BORDER color option to omit station axes in plot.

```
CXSTNS = vcio2
SNPARM =
SNFILE = pro6
DATTIM = all
VCOORD = hght
PTYPE = lin
YAXIS = 0/17000
TAXIS = r
```

#LINE = 3

#BORDER = 1;0;1

#CINT = 5

WIND = bk30-7
TITLE = 1
PANEL = 0
DEVICE = xw
CLEAR = y
FILTER = .4
TEXT = 1
CURVE = 2
CLRBAR =
CONTUR = 3/3
FINT =
FLINE =
CTYPE = c


### Error Messages

```
[SNCROSS2-1] Fatal error initializing TAE.

[SNCROSS2-2] Fatal error reading TAE parameters.

[SNCROSS2-3] GEMPLT initialization error.

[SNCROSS2-4] Vertical coordinate for isentropes must be PRES.

[SNCROSS2-5] There are no times in the file.

[SNCROSS2-6] Fewer than four stations/times were selected.

[SNCROSS2-7] Data buffer is too small.

[SNCROSS2-8] Temperature or pressure data not available.

[SNCROSS2-9] The station ... cannot be found in the data set.

[SNCROSS2-10] Error setting up graph; check invalid LOG axis.

[SNCROSS2-11] Input ... for PTYPE is invalid.

[SNCROSS2-12] Either input ... for YAXIS or input for VCOORD is invalid.

[SNCROSS2-13] Parameter ... is not computable.

[SNCROSS2-14] Parameter ... is a character.

[SNCROSS2-15] The grid coordinates cannot be defined.

[SNCROSS2-16] Multiple station entry is invalid for time sections.

[SNCROSS2-17] Data at time ... is not in the file.

[SNCROSS2-18] Wind data cannot be computed.
```

## 4.78 SNDELT

SNDELT deletes data from a sounding data file.



```
SNFILE Sounding data file
DATTIM Date/time
AREA Data area
```


SNDELT deletes data from a sounding data file. The stations to be deleted are specified

in AREA; the times to be deleted are given in DATTIM.

If AREA is set to DSET or ALL, all the data present at the times in DATTIM will be

deleted, along with the headers for those times.

## Examples

1. Delete all the data at time 920722/1200.

```
SNFILE = 92jul.snd
DATTIM = 920722/1200
AREA = dset
```
2. Delete data at IAD for the most recent time in the file.

```
SNFILE = realtime.snd
DATTIM = last
AREA = @iad
```
### Error Messages

```
[SNDELT-1] Fatal error initializing TAE.

[SNDELT-2] Fatal error reading TAE parameters.

[SNDELT-3] Error deleting data at time ....
```

## 4.79 SNEDIT

SNEDIT adds data in a sequential edit file to a sounding file.



```
SNEFIL Sounding edit file
SNFILE Sounding data file
TIMSTN Times/additional stations
```


SNEDIT adds information from a sequential file to a GEMPAK sounding file. It can

be used to add or replace data at a station. The program can be used to add data to either

merged or unmerged data sets.

The data to be added must be in a text file, SNEFIL. This file can be created using F

as the output device in SNLIST. MRGDAT, which can be set in SNLIST, is a flag in-

dicating whether the data to be written will be merged or written as separate parts. If

the edit file contains unmerged data, the part name must be included and the parameters

must be in the order expected by GEMPAK. Undecoded text (raw reports) should not

be included in SNEFIL.

If SNFILE exists, the data will be added to the file. If it does not already exist, a new

file will be created. A new file will be a merged or unmerged data set, depending on

the type of data in SNEFIL. The maximum number of stations and times allocated in

a new file will be read from TIMSTN.

For merged edit data files, the parameters to be edited must be specified at the begin-

ning of the edit file. For example:

#PARM=PRES;TMPC;DWPC;DRCT;SPED;HGHT

The parameter line must contain the string, PARM, and =. Note that the output of SN-

LIST is SNPARM = xxxx;yyyy;..., which is valid. If the parameter list must be contin-

ued on the next line, the last character on the current line must be a semicolon. If

SNFILE is an existing file, the parameters listed in the edit file must be exactly those

in the data set, although SNFILE may include undecoded text. For unmerged data, in-

formation about the parameters is not necessary.

The information to be added to the sounding file consists of a series of station entries

including information for the station, followed by station data,Station information must

be listed as KEYWORD = value, where the valid keywords are STID, STNM, SLAT,

SLON, SELV, and TIME. The TIME keyword must be found for each station. Either

STID or STNM is also required. If the station is not already in the file, the station iden-

tifier, station number, latitude, longitude, and elevation found will be added to the file.


If the station is already in the file, the station location and elevation will not be changed.

Note that program SNSTNS can be used to modify station header values.

## Examples

1. Add the data in the edit file, SNLIST.FIL, to the file, SOUND.DAT,

which does not exist.

```
SNEFIL = snlist.fil
SNOUTF = sound.dat
TIMSTN = 1/ 10
```
The file SNLIST.FIL follows:

#SNPARM = PRES;TMPC;DWPT;DRCT;SPED;HGHT

#STNPRM = SHOW;LIFT;KINX

#STID = IAD STNM = 72403 TIME = 841227/1200

#SLAT = 38.98 SLON = -77.46 SELV = 85.0

#SHOW = 14.11 LIFT = 25.60 KINX = 11.50

#PRES TMPC DWPT DRCT SPED HGHT

#1024.00 2.40 -1.40 0.00 0.00 85.00

#1000.00 2.20 -3.80 -9999.00 -9999.00 277.00

#850.00 5.60 -9.40 -9999.00 -9999.00 1592.00

#700.00 -1.90 -2.30 -9999.00 -9999.00 3167.00

#500.00 -15.70 -22.70 -9999.00 -9999.00 5790.00

#400.00 -28.90 -33.40 -9999.00 -9999.00 7430.00

#300.00 -44.30 -93.30 -9999.00 -9999.00 9420.00

#250.00 -55.30 -104.30 -9999.00 -9999.00 10620.00

#200.00 -66.50 -115.50 -9999.00 -9999.00 12000.00

#150.00 -65.90 -114.90 -9999.00 -9999.00 13750.00

#100.00 -66.90 -115.90 -9999.00 -9999.00 16220.00

The merged file SOUND.DAT will be created. It will contain the parameters

PRES;TMPC;DWPT;DRCT;SPED;HGHT. Space for a maximum of 1 time

and 10 stations will be allocated. The time 841227/1200 and station IAD

will be added to the file and then the data for that time and station will

be added. Note that the stability indices will be ignored. 2. Add the

data in the edit file, IAD.FIL, to the unmerged file, REALTIME.SND,

which already exists.

```
SNEFIL = realtime.snd
SNOUTF = iad.fil
```

#TIMSTN = 0

The file IAD.FIL follows:

#STID = IAD STNM = 72403 TIME = 900823/0000

#SLAT = 38.98 SLON = -77.46 SELV = 85.0

#TTAA 0

#PRES TMPC DWPC DRCT SPED HGHT

#1008.00 19.20 18.90 0.00 3.00 -9999.00

#1000.00 19.00 18.20 -9999.00 -9999.00 150.00

#850.00 14.40 13.30 160.00 8.00 1538.00

#700.00 5.40 4.30 240.00 6.00 3157.00

#500.00 -8.10 -9.80 215.00 7.00 5850.00

#400.00 -19.90 -49.90 225.00 11.00 7540.00

#300.00 -34.10 -64.10 235.00 18.00 9620.00

#250.00 -44.30 -9999.00 245.00 16.00 10870.00

#200.00 -54.30 -9999.00 250.00 22.00 12330.00

#150.00 -60.50 -9999.00 265.00 14.00 14140.00

#100.00 -64.50 -9999.00 235.00 2.00 16620.00

#TTBB 0

#PRES TMPC DWPC

#1008.00 19.20 18.90

#850.00 14.40 13.30

#684.00 3.60 1.20

#602.00 0.00 -1.30

#481.00 -9.90 -12.50

#PPBB 0

#HGHT DRCT SPED

#0.00 0.00 3.00

#914.00 120.00 12.00

#1219.00 130.00 10.00

#1829.00 180.00 7.00

#2134.00 195.00 6.00

#2438.00 210.00 6.00


### Error Messages

```
[SNEDIT+1] Data for ... has been added to the file.

[SNEDIT-1] Fatal error initializing TAE.

[SNEDIT-2] Fatal error reading the TAE parameters.

[SNEDIT-3] The edit file cannot be opened.

[SNEDIT-4] List of parameters not found in edit file.

[SNEDIT-5] Too many parameters in edit file.

[SNEDIT-6] Edit file ... cannot be opened.

[SNEDIT-7] Error creating new sounding file ....

[SNEDIT-8] Edit file parms don't match those in data set.

[SNEDIT-9] First parameter not a valid vertical coordinate.

[SNEDIT-10] Error opening existing sounding file ....

[SNEDIT-11] Cannot write merged data to unmerged data set.

[SNEDIT-12] Sounding file can't be created with MAXTIM <= 0.

[SNEDIT-13] Sounding file can't be created with MAXSTN <= 0.

[SNEDIT-14] The time ... was not added to the file.

[SNEDIT-15] The station ... was not added to the file.

[SNEDIT-16] Error writing data to file.

[SNEDIT-17] The part ... has invalid parameters.

[SNEDIT-18] Edit file ... has an invalid format.

[SNEDIT-19] Station ... has an invalid format.

[SNEDIT-20] Cannot write unmerged data to merged data set.
```

## 4.80 SNHODO

SNHODO draws a hodograph of upper air data.



```
SNFILE Sounding data file
AREA Data area
LINE Color/type/width/label/smth/fltr
MARKER Marker color/type/size/width/hw
BORDER Background color/type/width
TITLE Title color/line/title
XAXIS Xstrt/xstop/xinc/lbl;gln;tck
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
LEVELS Vertical levels
VCOORD Vertical coordinate type
DATTIM Date/time
CLEAR Clear screen flag
DEVICE Device|name|x size;y size|color type
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
```


SNHODO draws a hodograph which represents the vertical distribution of the horizon-

tal wind at a station. The winds are plotted in meters/sec.

The line color, line type, and width are specified in LINE. The marker color, type, and

size are specified in MARKER. The levels at which the line will be labeled are speci-

fied in LEVELS; VCOORD specifies the vertical coordinate for LEVELS.

The x and y axis limits are specified in XAXIS and YAXIS. If these values are blank,

the axes will be scaled to the actual data.

## Examples

1. Plot the hodograph for station CHH for the latest time in the data set.

Label the hodograph every 100 mb from 1000 mb to 100 mb. Use

hardware font 2 for the text and display the hodograph in a XW window

named snhodo.

#SNFILE = $GEMDATA/HRCBOB.SND

```
AREA = @chh
LINE = 3
MARKER = 1
BORDER = 1
TITLE = 1
XAXIS =
YAXIS =
LEVELS = 1000-100-100
```

```
VCOORD = pres
DATTIM = last
CLEAR = y
DEVICE = xw|snhodo
PANEL = 0
TEXT = 1/2/1/hw
```
2. Plot the CHH hodograph using height coordinates. Label every 2000

meters from 1000 meters to 24000 meters. Draw the x and y axes from

-10 to 40 meters/sec.

#SNFILE = $GEMDATA/HRCBOB.SND

```
AREA = @chh
LINE = 3
MARKER = 1
BORDER = 1
TITLE = 1
XAXIS = -10/40/10
YAXIS = -10/40/10
LEVELS = 1000-24000-2000
VCOORD = hght
DATTIM = last
CLEAR = y
DEVICE = xw|snhodo
PANEL = 0
TEXT = 1/2/1/hw
```
### Error Messages

```
[SNHODO-1] Fatal error initializing TAE.

[SNHODO-2] Fatal error reading TAE parameters.

[SNHODO-3] Fatal error initializing GEMPLT.

[SNHODO-4] The input for XAXIS is invalid.

[SNHODO-5] The input for YAXIS is invalid.

[SNHODO-6] Winds cannot be computed.

[SNHODO-7] The vertical coordinate ... cannot be computed.

[SNHODO-8] The range along the x or y axis is invalid.

[SNHODO-9] No winds can be plotted.

[SNHODO-10] No valid stations were found.
```

## 4.81 SNLIST

SNLIST lists upper air data from a sounding file for specified vertical levels and sta-

tions.



```
SNFILE Sounding data file
AREA Data area
DATTIM Date/time
SNPARM Sounding parameter list
STNDEX Stability indices
LEVELS Vertical levels
VCOORD Vertical coordinate type
OUTPUT Output device/filename
MRGDAT Merge data file flag/part type
```


SNLIST lists parameters derived from an upper air data set for the requested stations

and times.

Parameters which can be computed at various levels in the data set should be specified

in SNPARM. Parameters which have a single value at the station, such as stability in-

dices, should be specified in STNDEX.

Any parameters which can be computed from the data set parameters may be listed. If

the vertical levels requested are not present in the data set, the data will be interpolated

between existing levels. Data will also be interpolated to a new vertical coordinate sys-

tem, if requested.

Data from an unmerged data set can be listed without being merged if MRGDAT is set

to NO. If unmerged data are listed, SNPARM, VCOORD and LEVELS will be ig-

nored, except that parameter 'TEXT' is allowed for SNPARM.

If the sounding file contains both decoded data and undecoded text data, both types can

be listed by specifying parameter 'TEXT' in the parameter list for either SNPARM or

STNDEX. TEXT may appear anywhere in the list of parameters. Only text data for

parts TTAA, TTBB, PPBB and TTCC will be listed.

## Examples

1. List the parameters in the data set for the stations IAD and ACY at the

mandatory levels. Also list the Showalter, lifted and K indices.

#SNFILE = $GEMDATA/HRCBOB.SND

```
AREA = @iad;acy
DATTIM = last
```

```
SNPARM = dset
STNDEX = show;lift;kinx
LEVELS = man
VCOORD = pres
OUTPUT = t
MRGDAT = YES
```
2. List the unmerged data from IAD.

#SNFILE = $GEMDATA/HRCBOB.SND

```
AREA = @iad
DATTIM = last
SNPARM = dset
STNDEX = show;lift;kinx
LEVELS = man
VCOORD = pres
OUTPUT = t
MRGDAT = no
```
3. List the pressure, mixing ratio, u- and v-wind components and the

Montgomery stream function on the isentropic surfaces from 300 to 400

degrees in 10-degree increments. Change the station to ACY.

#SNFILE = $GEMDATA/HRCBOB.SND

```
AREA = @acy
DATTIM = last
SNPARM = pres;mixr;uwnd;vwnd;psym
STNDEX =
LEVELS = 300-400-10
VCOORD = thta
OUTPUT = t
MRGDAT = yes
```
4. List the unmerged data and the undecoded text for station BOI. Use a

locally created sounding file containing both types of data called

DATA.SND.

#SNFILE = DATA.SND

```
AREA = @boi
DATTIM = last
SNPARM = text
STNDEX =
LEVELS =
VCOORD =
OUTPUT = t
MRGDAT = no
```

### Error Messages

```
[SNLIST-1] Fatal error initializing TAE.

[SNLIST-2] Fatal error reading TAE parameters.

[SNLIST-3] There are no parameters to be listed.

[SNLIST-4] Level parameter ... cannot be computed.

[SNLIST-5] Parm ... is character; cannot be listed.

[SNLIST-6] Vertical coordinate ... cannot be computed.

[SNLIST-7] Station parameter ... cannot be computed.

[SNLIST-8] Parm ... is character; cannot be listed.

[SNLIST-9] No stations reported at these times.

[SNLIST-10] Too many parameters; ... cannot be added.

[SNLIST-11] The data set is merged.

[SNLIST-12] No inputs for levels.
```

## 4.82 SNMAP

SNMAP plots sounding data on a map.



```
AREA Data area
GAREA Graphics area
SATFIL Satellite image filename(s)
RADFIL Radar image filename(s)
IMCBAR Color/ornt/anch/x;y/ln;wd/freq
SNPARM Sounding parameter list
DATTIM Date/time
LEVELS Vertical levels
VCOORD Vertical coordinate type
SNFILE Sounding data file
COLORS Color list
MAP Map color/dash/width/filter flag
LATLON Line color/dash/width/freq/inc/label/format
TITLE Title color/line/title
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
DEVICE Device|name|x size;y size|color type
PROJ Map projection/angles/margins|drop flag
FILTER Filter data factor
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
LUTFIL Enhancement lookup table filename
STNPLT Txtc/txt attr|marker attr|stnfil#col
```


SNMAP plots sounding data parameters at station locations on a map. Any level or sta-

tion parameter that can be computed can be displayed. Data may be plotted in any valid

GEMPAK projection and may be overlaid on images.

A list of times may be given in DATTIM allowing animation of sounding plots.

The order of the ### Input Parameters determines their location on the plot. Parameters are

separated by semicolons. A position may be skipped by entering two consecutive semi-

colons or entering parameter SPAC or BLNK. A parameter will be plotted centered at

the station if it is the first parameter in the list. If no parameter is to be displayed cen-

tered on the station location, a semicolon must appear before the first parameter, or the

first parameter must be either SPAC or BLNK. The following chart shows the place-

ment of the data around the station. The number indicates the position of the parameter

in the SNPARM list:

#8

#2104

#315

#6117

#9


Note that wind symbols and markers are always plotted at the center.

Station data will be filtered; i.e., overlapping stations will not be plotted, if FILTER is

set to YES. FILTER may also be entered as a number greater than or equal to zero.

FILTER = NO has the same effect as FILTER = 0. FILTER = 1 has the same effect as

FILTER = YES. Values less than 1 allow more crowding of stations, values exceeding

1 less crowding. If a parameter is BLNK, the filter will not allocate any space for that

parameter. The parameter SPAC may be used to reserve the space with the FILTER

option so that later calls will plot the same stations after filtering, provided that the same

number of parameters is specified.

If certain stations are not to be removed by the filter, these stations are listed first fol-

lowing an @. The area over which filtering is to occur is specified after a slash. For

example,

```
AREA = @iad;hts;rap/us
FILTER = YES
```
will display a filtered array of stations over the area corresponding to US, but IAD, HTS

and RAP will be shown regardless of the filtering.

Either wind barbs or wind arrows can be plotted, by specifying a wind symbol param-

eter name in the list of parameters for SNPARM. The wind barb or arrow is plotted at

the station location according to the type specification, which is entered as described in

the SNPARM documentation.

Conditions can be specified for the parameters. The conditions are documented in the

SNPARM documentation. Note that individual parameters can be scaled using these

conditional functions. For example, TMPC*10 will plot temperature multiplied by 10.

## Examples

1. Plot a polar stereographic map of stations in the Eastern United States at

850 mb for all times in the data file. Plot a standard station model for

each location. The upper air data to plot include: 1) wind barbs in knots;

2) temperature in Celsius; 3) coded height; 4) dewpoint depression in

Celsius; and 5) station ID. The data are plotted using the specified color

list.

```
AREA = east-
GAREA = east
SATFIL =
RADFIL =
IMCBAR =
SNPARM = brbk:1:2:112;tmpc;;stdz;;dpdc;stid
DATTIM = all
```

#LEVELS = 850

```
VCOORD = pres
SNFILE = $GEMDATA/hrcbob.snd
COLORS = 6;2;5;3;18
MAP = 1
LATLON = 2/10/1/1/10;10
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = nps
FILTER = no
TEXT = 1
LUTFIL =
STNPLT =
```
2. Plot the temperature and height at 500 mb and the lifted index for all

times in the data file. The data are plotted at the stations where the

temperature is less than -8 degrees Celsius. The lifted index is multiplied

by 10 before being plotted.

```
AREA = us-
GAREA = us
SATFIL =
RADFIL =
IMCBAR =
SNPARM = mark:2;;tmpc<-8;;hght;;;;;lift*10
DATTIM = all
LEVELS = 500
VCOORD = pres
SNFILE = $GEMDATA/hrcbob.snd
COLORS = 2;5;3
MAP = 1
LATLON = 2/10/1/1/10;10
TITLE = 1
CLEAR = yes
PANEL = 0
DEVICE = xw
PROJ = mer
FILTER = 0
TEXT = .75
LUTFIL =
STNPLT =
```

### Error Messages

```
[SNMAP+1] Parameter ... cannot be computed.

[SNMAP-1] Fatal error initializing TAE.

[SNMAP-2] Fatal error reading TAE parameters.

[SNMAP-3] Fatal error initializing GEMPLT.

[SNMAP-4] Invalid levels or vertical coordinate have been input.

[SNMAP-5] A range of levels is invalid in SNMAP.

[SNMAP-6] Winds cannot be computed.
```

## 4.83 SNMOD

SNMOD moves selected sounding data from an input sounding file to an output sound-

ing file.



```
SNFILE Sounding data file
SNOUTF Output sounding file
SNPARM Sounding parameter list
AREA Data area
DATTIM Date/time
LEVELS Vertical levels
VCOORD Vertical coordinate type
TIMSTN Times/additional stations
MRGDAT Merge data file flag/part type
IDNTYP STNM or STID
```


SNMOD takes data from a GEMPAK sounding file, SNFILE, and writes it to an output

GEMPAK sounding file, SNOUTF. This program can be used to subset the original

dataset by time and/or stations and to change the levels and vertical coordinate.

The output dataset may be either a merged or unmerged data set. If the output file does

not exist, a new file will be created by this program. If a new file is created, MRGDAT

will determine whether the output file will be a merged or unmerged dataset. Note that

an error will result if the input dataset is merged and the output dataset is unmerged.

The value of SNPARM will be ignored if the output dataset is unmerged. If a merged

data file is to be created, SNPARM will specify the parameters to be included. In this

case, if SNPARM = DSET, the parameters in the input dataset will be used. If the out-

put file is an existing merged dataset, the parameters specified in SNPARM must be in

the dataset. In this case, if SNPARM = DSET, the parameters in the output file will be

used. Undecoded text (raw reports) may be included in the input file, but will not be

moved to the output file.

If the output file is to be created, the maximum number of times and stations which can

be stored in the file must be specified in TIMSTN.

IDNTYP has been added to specify whether station numbers or station identifiers will

be used in referring to stations. Generally, IDNTYP should be set to STNM to use sta-

tion numbers. However, in datasets which do not have station numbers, or which have

a single invalid number, character station identifiers must be used and IDNTYP must

be set to STID.


## Examples

1. Put the mandatory data for stations in an area centered on IL at time

910819/1200 into a new unmerged sounding file, NEW.SND, which may

contain 2 times and 150 stations.

```
SNFILE = gemdata:hrcbob.snd
SNOUTF = new.snd
SNPARM = dset
AREA = il
DATTIM = 19/12
LEVELS = man
VCOORD = pres
TIMSTN = 2/150
MRGDAT = no
IDNTYP = stnm
```
2. Add the stations at time 910819/00 to the file created in example 1.

#DATTIM = 19/00

3. Create an isentropic dataset containing the specified parameters with

levels every 5 degrees. Include all the US stations.

```
SNFILE = gemdata:hrcbob.snd
SNOUTF = thta.snd
SNPARM = thta;pres;mixr;uwnd;vwnd;psym
AREA = @us
DATTIM = all
LEVELS = 250-450-5
VCOORD = thta
TIMSTN = 4/150
MRGDAT = yes
```

### Error Messages

```
[SNMOD-1] Fatal error initializing TAE.

[SNMOD-2] Fatal error reading TAE parameters.

[SNMOD-3] Output file cannot be unmerged type.

[SNMOD-4] A new file cannot be created with no times.

[SNMOD-5] A new file cannot be created with no stations.

[SNMOD-6] Parameter ... cannot be computed.

[SNMOD-7] ... is character type; cannot be added to file.

[SNMOD-8] Output vertical coordinate ... cannot be used.

[SNMOD-9] No valid parameters were specified.

[SNMOD-10] Time ... cannot be added to the output file.

[SNMOD-11] Station ... cannot be added to output file.

[SNMOD-12] Parms do not match those in output data set.

[SNMOD-13] Output file name is blank.
```

## 4.84 SNPROF

SNPROF draws profiles of upper air data.



```
SNFILE Sounding data file
DATTIM Date/time
AREA Data area
SNPARM Sounding parameter list
LINE Color/type/width/label/smth/fltr
PTYPE Plot type/h:w ratio/margins
VCOORD Vertical coordinate type
STNDEX Stability indices
STNCOL Stability index color
WIND Wind symbol/siz/wdth/typ/hdsz
WINPOS Wind position
MARKER Marker color/type/size/width/hw
BORDER Background color/type/width
TITLE Title color/line/title
DEVICE Device|name|x size;y size|color type
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
XAXIS Xstrt/xstop/xinc/lbl;gln;tck
FILTER Filter data factor
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
THTALN THTA color/dash/width/mn/mx/inc
THTELN THTE color/dash/width/mn/mx/inc
MIXRLN MIXR color/dash/width/mn/mx/inc
```


SNPROF draws profiles of upper air data.

Any two parameters that can be computed from the data set can be plotted. These pa-

rameters are specified in SNPARM. The profiles' colors, line types, and widths are

specified in LINE. Markers identifying the data points can be plotted by setting a color

in MARKER.

Profiles are plotted in an animation sequence if more than one station and/or time is

specified in AREA and DATTIM, respectively.

The type of y axis is specified in PTYPE. LIN, LOG, STUVE, and SKEW are all valid.

Note that SKEW is only valid when plotting temperature vs. pressure. The bottom and

top of the axis, along with an increment for labels, are set in YAXIS. If the panel is

changed to be less than the full screen, the default margins will often be too large. The

margins can be set explicitly in PTYPE.


Winds are plotted in the right margin and can be specified in WIND. There are three

positions available for plotting winds. They are numbered 1 to 3, with 1 being the left-

most position. The location for the wind to be plotted is specified in WINPOS. The

positions also apply to the station identifier, date/time and stability indices which are

plotted above the diagram.

Dry adiabats, moist adiabats and mixing ratio background lines can be added to the di-

agram. The attributes of these lines are specified in THTALN, THTELN, and

MIXRLN.

If the color is set to 0, no lines will be plotted. If the start, stop and increment are not

set, defaults will be supplied. Note that these lines can only be drawn on plots of pres-

sure versus temperature.

FILTER in this program determines whether the wind barbs are filtered.

## Examples

1. Plot a Skew T chart for HAT for all of the times in the file. Display

the Showalter index, the bulk Richardson number, CAPE, the Lifted index

and the K index. Plot TMPC in color 2 using a solid line with width of

3. Plot DWPC in color 3 using a dashed line with width of 3. Include

theta, theta-e, and mixing ratio lines. Plot wind barbs in color 6 in

position 1.

```
SNFILE = $GEMDATA/hrcbob.snd
DATTIM = all
AREA = @hat
SNPARM = tmpc;dwpc
LINE = 2;3/1;3/3
PTYPE = skewt
VCOORD = pres
STNDEX = show;brch;cape;lift;kinx
STNCOL = 1
WIND = bm6//2
WINPOS = 1
MARKER =
BORDER = 1
TITLE = 1
DEVICE = xw
YAXIS = ///;1
XAXIS = -40/40/10/;1
FILTER = no
CLEAR = yes
PANEL = 0
TEXT = 1
THTALN = 8/3/1
THTELN = 23/1/1
MIXRLN = 16/10/2
```

2. Plot relative humidity with a logarithmic pressure axis. Plot the data

from the surface to 200 mb. The theta, theta-e, and mixing ratio lines

are turned off, and no stability indices are computed.

```
SNFILE = $GEMDATA/hrcbob.snd
DATTIM = all
AREA = @hat
SNPARM = relh
LINE = 3/1/3
PTYPE = log
VCOORD = pres
STNDEX =
STNCOL = 1
WIND = 0
WINPOS = 1
MARKER =
BORDER = 1
TITLE = 1
DEVICE = xw
YAXIS = 1050/200
XAXIS = 0/100/20
FILTER = no
CLEAR = yes
PANEL = 0
TEXT = 1
THTALN =
THTELN =
MIXRLN =
```
3. Plot equivalent potential temperature on the x axis vs. potential

temperature on the y axis. Display the data for the first time in the data

file for HAT, ALB and BNA. Set the X axis to range from 310 to 360

Kelvin in increments of 10. Also, set the Y axis to ranged from 280 to

350 Kelvin in increments of 10.

```
SNFILE = $GEMDATA/hrcbob.snd
DATTIM = first
AREA = @hat;alb;bna
SNPARM = thte
LINE = 3/1/3
PTYPE = lin
VCOORD = thta
STNDEX =
STNCOL = 1
WIND = 0
WINPOS = 1
MARKER =
BORDER = 1
TITLE = 1
DEVICE = xw
YAXIS = 280/350/10
XAXIS = 310/360/10
```

```
FILTER = no
CLEAR = yes
PANEL = 0
TEXT = 1
THTALN =
THTELN =
MIXRLN =
```
### Error Messages

```
[SNPROF+6] Background lines cannot be drawn.

[SNPROF+5] Stability indicies are specified with color = 0

[SNPROF+4] Parameter ... was requested with color set to 0.

[SNPROF+3] Winds cannot be computed.

[SNPROF+2] Parameter ... is a character type.

[SNPROF+1] Parameter ... cannot be computed.

[SNPROF-1] Fatal error initializing TAE.

[SNPROF-2] Fatal error reading TAE parameters.

[SNPROF-3] Fatal error initializing GEMPLT.

[SNPROF-4] The plot type ... is invalid.

[SNPROF-5] The the x-axis range must be specified in XAXIS.

[SNPROF-6] The two parms use different temperature units.

[SNPROF-7] The vertical coordinate ... cannot be computed.

[SNPROF-8] The range along the x or y axis is invalid.

[SNPROF-9] No valid stations were found.

[SNPROF-10] SNPARM has not been specified.
```

## 4.85 SNSTNS

SNSTNS modifies the station information in an upper air file.



```
SNFILE Sounding data file
STNFIL Station information file
ADDSTN Add station flag
IDNTYP STNM or STID
```


SNSTNS updates the station information in a GEMPAK upper air file. The station in-

formation generally consists of the character station identifier, STID, the station num-

ber, STNM, the latitude, SLAT, the longitude, SLON, the elevation, SELV, the state,

STAT and the country, COUN.

This information, along with the station name, which is not used, must be stored in a

fixed format in the table file specified in STNFIL. The current GEMPAK upper air sta-

tion table for US, Canadian and Mexican stations is SNSTNS.TBL. An upper air file

containing stations for the world is SNWORLD.TBL.

ADDSTN is a logical parameter which indicates whether stations which are in the table

file but not already in the upper air file will also be added to the upper air file, provided

there is room for them.

Either STID or STNM may be used to key on the desired station identifier by setting

the desired value in IDNTYP.

## Examples

1. Update upper air file called SOUNDINGS.DAT with station information

from MYSTN.DAT adding stations that are not already in the file.

```
SNFILE = soundings.dat
STNFIL = mystn.dat
ADDSTN = yes
IDNTYP = stid
```

### Error Messages

```
[SNSTNS+1] WARNING! No stations were updated.

[SNSTNS-1] Fatal error initializing TAE.

[SNSTNS-2] Fatal error reading TAE parameters.

[SNSTNS-3] Invalid input for IDNTYP; must be STID or STNM.

[SNSTNS-4] STNFIL ... cannot be opened.
```

## 4.86 SNTSER

SNTSER draws a time series at a sounding station.



```
SNFILE Sounding data file
DATTIM Date/time
TAXIS Time1-time2-tinc;lbl;gln;tck
LEVELS Vertical levels
VCOORD Vertical coordinate type
SNPARM Sounding parameter list
STNDEX Stability indices
AREA Data area
PTYPE Plot type/h:w ratio/margins
YAXIS Ystrt/ystop/yinc/lbl;gln;tck
BORDER Background color/type/width
LINE Color/type/width/label/smth/fltr
MARKER Marker color/type/size/width/hw
TITLE Title color/line/title
CLEAR Clear screen flag
PANEL Panel loc/color/dash/width/regn
TEXT Size/fnt/wdth/brdr/N-rot/just/hw flg
DEVICE Device|name|x size;y size|color type
```


SNTSER draws a time series plot for a sounding station.

Only one parameter may be plotted at a time. This parameter may be a level parameter

defined in SNPARM or a stability index defined in STNDEX. If both SNPARM and

STNDEX have values, SNPARM will be used. If a level parameter is defined in SN-

PARM, a vertical level must also be set in LEVELS.

The type of y axis can be set in PTYPE. Either LIN or LOG is valid. The limits on the

y axis can be set in YAXIS.

The times to plot are specified in DATTIM. Only those times specified will be plotted.

Lines will be drawn connecting the data points, provided that no more than two points

are missing between segments.

The time axis is specified in TAXIS using the usual GEMPAK date/time conventions,

including FIRST and LAST. If TAXIS has no increment, a reasonable value is chosen.

If TAXIS is blank, the limits are taken from the first and last valid times set in DAT-

TIM.

When the user desires multiple lines on the same graph, TAXIS and YAXIS should be

explicitly set. Then LINE, MARKER, and TITLE can be varied for successive combi-

nations of LEVELS, VCOORD, AREA, and SNPARM.


The height-to-width ratio of the plot may be specified in PTYPE following a slash. If

no value is entered, a value of 0.5 will be used.

## Examples

1. Plot the relative humidity at the 850 mb level for station CHH for all of

the times in the data set. Use hardware font 2 for the text and plot the

relative humidity using a solid green line.

#SNFILE = $GEMDATA/HRCBOB.SND

```
DATTIM = all
TAXIS =
LEVELS = 850
VCOORD = pres
SNPARM = relh
STNDEX =
AREA = @chh
PTYPE = lin
YAXIS =
BORDER = 1
LINE = 3
MARKER = 1
TITLE = 1
CLEAR = y
PANEL = 0
TEXT = 1/2/1/hw
DEVICE = xw
```
2. Plot the lifted index at CHH for the times between 18/00 and 20/00.

Plot the line with a thick short dashed pattern. Use asterisks for the

markers.

#SNFILE = $GEMDATA/HRCBOB.SND

```
DATTIM = all
TAXIS = 18/00-20/00
LEVELS = 850
VCOORD = pres
SNPARM =
STNDEX = lift
AREA = @chh
PTYPE = lin
YAXIS =
BORDER = 1
LINE = 2/2/3
MARKER = 3/12
TITLE = 1
CLEAR = y
PANEL = 0
TEXT = 1/2/1/hw
DEVICE = xw
```

### Error Messages

```
[SNTSER+2] WARNING, invalid PTYPE, LIN will be used.

[SNTSER+1] WARNING, more than one parameter specified.

[SNTSER-1] Fatal error initializing TAE.

[SNTSER-2] Fatal error reading TAE parameters.

[SNTSER-3] Fatal error initializing GEMPLT.

[SNTSER-4] Input for LEVEL is invalid.

[SNTSER-5] Error defining graph coordinates.

[SNTSER-6] No points found for plot.

[SNTSER-7] Session not interactive.

[SNTSER-8] No parameters entered.

[SNTSER-9] Parm ... not calculable.

[SNTSER-10] Parm ... is a character.

[SNTSER-11] PTYPE chosen is invalid.
```

## 4.87 VG2UKA

VG2UKA converts VG files to ASCII files, using the UKMET browsable ASCII for-

mat. Currently, only high level significant weather (SWH VG files may be converted.



```
DATTIM Date/time
FHOUR Forecast hour
FXYTBL FXY table file
CENTER Originating Center ID #/Sub-Center ID #
VGFILE Vgfile | scale file | attribute file
UKAFIL Intermediate input/output ASCII file
```


VG2UKA is a table-driven GEMPAK program with parameter input, which reads an

input VG file and converts it to the UKMET browsable ASCII format. (A description

of this format for SWH data may be found in the WAFC document "Representing

WAFS

Significant Weather (SIGWX) Data in BUFR.") The ### Input Parameters

are GEMPAK date/time, forecast hour, FXY table file name (chart type alias), originat-

ing center id, VG input file name and ASCII output file name.

Parameters DATTIM and FXYTBL must be specified. Default values for other param-

eters are FHOUR = 24, CENTER = 7, VGFILE = $SIGWX/vgfiles/

YYYYMMDDHH_FF_final.vgf (from $GEMTBL/config/datytype.tbl, with SIGWX

set by the site administrator) and UKAFIL = <FXYTBL>.txt.

An SWH VG file may consist of groups of clouds, fronts, jets, turbulence, storm sym-

bols and/or volcano symbols. Tropopause text information and radiation symbols ap-

pear in the the file as ungrouped elements. Ungrouped "JET" elements may also be

used.

See the Product Generation-BUFR help for information about creating the input VG

file.

## Examples

1. Convert a 24-hour forecast SWH VG file, test1.vgf, to an ASCII file,

SIGWXHI.txt. The originating center is NCEP.

#DATTIM = 040527/1200

#FHOUR = 24

#FXYTBL = SWH

#CENTER = 7

```
VGFILE = test1.vgf
UKAFIL = SIGWXHI.txt
```

2. Convert an 18-hour forecast SWH VG file, test2.vgf, to an ASCII file,

SIGWXHI_18.txt. The originating center is UKMO.

#DATTIM = 040526/0000

#FHOUR = 18

#FXYTBL = SWH

#CENTER = 74

```
VGFILE = test2.vgf
UKAFIL = SIGWXHI_18.txt
```
3. Using default values, convert a 24-hour forecast SWH VG file, $SIGWX/

vgfiles/2004060718_24_final.vgf, to an ASCII file, SWH.txt. The

originating center is NCEP.

#DATTIM = 040607/1800

#FHOUR =

#FXYTBL = SWH

#CENTER =

#VGFILE =

#UKAFIL =

### Error Messages

```
[VG2UKA-1] The GEMPAK date/time and/or valid hour should be input.

[VG2UKA-2] Group type ... cannot be found in the group type list.

[VG2UKA-3] Cannot create alias from datatyp.tbl. Check alias or inputs.

[VG2UKA-4] VG file ... not found.

[VG2UKA-6] Chart type/alias should be input.
```

