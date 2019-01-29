
# GEMPAK Variables

## ADDSTN

ADDSTN is a logical variable which indicates whether stations which are in STNFIL, but not already included in the data file, should be added to the data file. Enter YES to add stations. Enter NO to update current stations but not add new ones.

## AFOSFL

AFOSFL is the name of the AFOS graphics file to be displayed.

## AIRM

AIRM is the ending valid time for the airmet, the colors for the instrument flight rules, mountain obscuration, turbulence, icing and sustained winds, and flags for plotting symbols or airmet type, the end time, and the flight levels on the map.

    End time|IR;MO;TB;IC;SW clrs|TB color2|Flight lvl|low;upper filter lvl|
    Symbol flg|Time flg|Flight lvl flg

Airmets that are valid at the ending time will be plotted on the map. The ending time is given as a GEMPAK date/time string. Any missing items from the string will be filled in by the system time. The ending time may also be LAST or ALL. LAST will use the system time and plot all current airmets. ALL will plot all the airmets in the data files for the last ten days, whether they are active, cancelled, updated or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol will not be plotted. If any color is missing, a default will be used. Defaults are red for instrument flight rules, magenta for mountain obscuration, light blue for turbulence, green for icing and dark pink for sustained winds.

TB color2 is the second color for turbulence. The color from the first list will be used to plot Turbulence AIRMETs below the specified level. The color listed as the second color will be for Turbulence AIRMETs above the specified level.

Flight lvl is the dividing level for the turbulence display. The value is in hundreds of feet.


The lower and upper filter levels are flight level values used to filter the AIRM data display. If neither value is given, then all data is plotted. If only the lower level is present, then only data above that level is displayed. For the upper level only, data below this upper value are displayed. If both values are given, then the data is plotted for levels between the two boundary values.

The symbol flag is YES or NO and controls whether or not to plot a symbol or identifying text for the airmet on the map. The default is NO.

The time flag is YES or NO and controls whether or not to plot the end time of the airmet on the map. The default is NO.

The flight level flag is YES or NO and controls whether or not to plot the flight level(s) in hundreds of feet on the map. The default is NO.

## ANLYSS

ANLYSS is the average station spacing and the grid extend region separated by a slash:

    station spacing / grid extend region

This information is retrieved from the grid file and used in the objective analysis programs.

The station spacing is in degrees of latitude and is used to compute the weighting functions in the Barnes analysis programs. The default for the station spacing is twice the grid spacing.

The grid extend area is specified by four integers, separated by semicolons, which are the numbers of grid points to extend the grid left, down, right, and up. Only data within the extended grid area is used after the first Barnes pass. The default for the grid extend region is `2;2;2;2`.

## AREA

AREA is the data area. Only data within the area specified will be processed. Areas may be defined containing subareas. Subareas must be separated by slashes. Each subarea is additive, `+`, or subtractive, `-`, depending on the first character following the slash, with `+` being the default. Additive subareas add stations to the list of valid stations; subtractive subareas eliminate stations which were previously valid.

Subareas may be specified in the following ways:

1. `lat1;lon1;lat2;lon`

    This defines a latitude/longitude range where (`lat1, lon1`) is the lower left corner and (`lat2, lon2`) is the upper right corner. West longitude is negative.

    `#clat;clon;dlat;dlon`

    This defines a latitude/longitude range by the center latitude and longitude. The lower left corner is (`clat-dlat; clon-dlon`); the upper right corner is (`clat+dlat; clon+dlon`). No corrections are made for the poles or the International Date Line.

2. GEOG

    This is an abbreviation for a geographic area defined in the GEMPAK geographic table which includes abbreviations for states, provinces, and countries, as well as other names. A suffixed `+` or `*` will decrease the extent of the geographic area. A suffixed `-` will increase the extent of the geographic area.
    
3. STN

    This defines an area centered on a station found in the GEMPAK station table. A suffixed `+` or `*` will decrease the extent of the geographic area. A suffixed `-` will increase the extent of the geographic area.
    
4. DSET

    This includes all the stations in the current data set.

5. @ST

    This area includes those stations located in the state, province or country defined by ST. Only some countries are recognized (`US,CN,MX,CI,BW,AU`); other countries may be specified using method 6.

6. @CN:C

    This area includes those stations located in the country defined by CN.

7. @STN1;STN2;...;STNn

    This area includes the stations listed, where STNi may be a station identifier or a station number.

8. SHDR:iloval:ihival

    This area defines a range of integer values for the station header, SHDR. Valid keywords for SHDR are:

    * COUN -- country
    * SELV -- elevation (in meters)
    * SLAT -- latitude (in degrees x 100)
    * SLON -- longitude (in degrees x 100, West is negative)
    * SPRI -- priority
    * STAT -- state
    * STID -- character identifier (6 digits for surface--usually the WMO 5-digit number followed by a 0)

    where COUN, STAT and STID are not very useful, since the integer representation of characters is system dependent.

    For example, SELV:0:2000 specifies stations whose elevations are less than 2000 meters.

9. GAREA

    This area corresponds to a slightly larger area than that specified by GAREA. It will ensure that data will appear near the bounds of the view region.

---

## ATCF

ATCF is the initial time for the ATCF forecast tracks, the colors for each model track, the model names, flags for plotting the time, the storm name or number, the forecast wind speeds and markers at each forecast time, and an optional specific storm identifier.

    Initial time|Model clrs|Model names|Time flg|Id flg|Speed flg|Marker flag|Name

ATCF forecast tracks valid for the initial time will be plotted on the map. Forecast tracks show points at 6 or 12 hour intervals from 0 through 126 hours, if available. The actual storm track will be plotted as a white line, solid up to the initial time and dashed after the initial time (when data is available). The initial time is given as a GEMPAK date/time string. Any missing items from the string will be filled in by the system time. The initial time may also be LAST. LAST will use the system time to plot the most recent tracks whose initial time is within 9 hours of the current time. ALL is not accepted as an initial time for ATCF data.

The colors are separated by a semi-colon, as are the four-character model names. If no colors or model names are given, all model tracks will be plotted at default colors. If specific model names are listed, only those model tracks will be plotted. Default colors will be used unless colors are explicitly given, with a one-to-one correspondence between model names and colors. Specifying colors without model names has no effect.

The time flag is YES or NO and controls whether or not to plot the initial time of the ATCF forecast tracks on the map. The default is NO.

The storm id flag is YES or NO and controls whether or not to plot the storm name or number on the map. The default is NO.

The speed flag is YES or NO and controls whether or not to plot the speed in knots, when given, at each forecast time. The default is NO.

The marker flag is YES or NO and controls whether or not to plot a marker at each forecast time on the map. The default is NO.

The name or number of a tropical storm may be specified. If one is given, only tracks for that storm will be displayed for the date/time indicated. The default is all storms are plotted.

## AWPSFL

AWPSFL is the name of the AWIPS graphics file to be displayed. These files can be viewed by using the debug option.

    awpsfl | d

or

    awpsfl | D

the output file is called `awips.out`.

## BND

BND specifies the parameters needed for processing bounds areas:

    name[|tag]/color/fillsiz/fillpat/filt/minpts!
    lincol/linpat/linwid!
    mrksym/col/typ/siz/wid

FILLING BOUNDS AREAS:

* name - If name is "BG", then the entire background area in plot coordinates if filled with the given color, pattern, etc.
* tag - The tag option allows specification of a precise subarea to fill, e.g., fill a particular state instead of filling all the states. See examples.
* color - Additional help for color is available in COLORS.
* fillsiz, fillpat - The fill size is not yet implemented, however the fill pattern may be set to 1 (solid), 2 (slanted dash) or 3 (slanted line).
* filt - The filter value is used to reduce the number of points used to draw the bound areas. Valid values for the filter flag are between 0 and 1. The default is 0.
* minpts - The minpts variable allows plotting of only bounds with a number of points greater than a certain value, thus eliminating numerous small areas, such as islands.
* The overall effect is to increase performance. Care must be taken, however, since some large geographic areas may be described with just a few points (e.g., the state of Wyoming). Usually, a value of 100 or 200 will suffice.

OUTLINING BOUNDS AREAS:

* lincol - Color of bound outline. Additional help for color is available in COLORS.
* linpat - Outline line pattern.
* linwid - Outline line width.

Note that filt and minpts also apply to outlining.

PLOTTING A MARKER OR WEATHER SYMBOL AT BOUNDS CENTROID:

* mrksym - Either "MARK" or "WTHR" to plot either a marker or weather symbol.
* col - Color of marker or symbol. Additional help for color is available in COLORS.
* typ - Marker or weather symbol type (number).
* siz, wid - Marker or symbol size and width.

NOTE: any or all plot options may be requested at the same time.

The attributes for multiple bounds may be set by separating the settings for each bound

with a plus sign `+`.

Examples:

    BND = bg/4 + state_bnds/23 + lakes/

- fillsstate bounds and lakes


    BND = state_bnds/23 + great_lakes/

- fills state bounds and the great lakes


    BND = state_bnds|<STATE>FL/

- fills only the state of Florida


    BND = state_bnds|<STATE>FL/23! 2/2/
    
- fills only the state of Florida and outlines the state of Florida with a dashed red line of width 5


    BND = state_bnds|<STATE>FL/23! 2/2/5! wthr/3/65/5/
    
- fills only the state of Florida and outlines the state of Florida with a dashed red line of width 5. Also plots a weather symbol at the centroid of the state of Florida. This symbol is a green heavy rain symbol of size 5 and width 3.

## BORDER

BORDER is the color, line type and line width of the background and underground plot separated with slashes:

    background color; station line color; fill color/
    background line type; station line type /
    background line width; station line width

If the color is 0, no background will be drawn. The defaults for color, line type and line width are each 1.

## BOUNDS

BOUNDS specifies the bound area(s) to consider when performing the graph-to-grid procedure.

    name|<key>key_name|mask_flag

More than one bounds area may be indicated via the '+' separator.

* name - Name of the bounds area, eg., "SSA" for seamless surface analysis.
* key - The specific bound area keyword, eg., "AREA".
* key_name - The specific bound area, eg., "HPC050".
* mask_flag - "true" to perform the graph-to-grid analysis inside the above specified bounds area, "false" to perform the analysis outside.

Examples:

    BOUNDS = SSA|<AREA>HPC050|true

- Performs the graph-to-grid analysis only inside the HPC050 seamless surface analysis area.

## BUFRFIL

BUFRFIL is the BUFR output file names in the format of

    BUFRFIL = ...;...;...

where the individual items are the BUFR output file names corresponding to the individual `FXYTBL` file name (e.g., `JUBf99.YYYYMMDDHH;JUFf00.YYYYMMDDHH`) and the blank means that the output file names are gotten from the master table based on the requested FXY file names.

## CBTOP

CBTOP is the user estimated cloud top height in meters. The default value is 8000 m. This is for "convective (CB)" only.

## CENTER

CENTER allows the GD2NDFD user to specify the originating or generating center ID and sub-center ID. The input is given as follows:

    Center ID #/Sub-Center ID #

Values may be omitted in specifying CENTER. For example, to omit the first value, specify only the second, enter the following:

    /Sub-Center ID #

If values are omitted, the following defaults are inserted:

* Center 7 Center # (7->NCEP)
* Sub 5 Sub-center (5->HPC)

See NCEP OFFICE NOTE 388 for more information on how to set these parameters.

Below is a table of center numbers that may be used.
    
    ID# NAME ABBREV
    
    007 US Weather Svc - Nat. Cntrs for Envir. Prediction NCEP
    008 US Weather Svc - NWS Telecommunications Gateway NCEP
    009 US Weather Svc - Field Stations NCEP
    034 Japanese Meteorological Agency - Tokyo JMA
    052 US Weather Svc (NCEP/TPC) Nat. Hurricane Cntr - Miami NHC
    054 Canadian Meteorological Service - Montreal CMS
    057 US Air Force - Global Weather Central GWC
    058 US Navy - Fleet Numerical Oceanography Center FNOC
    059 NOAA Forecast Systems Laboratory - Boulder, CO FSL
    060 Nat. Center for Atmos. Research (NCAR) - Boulder, CO NCAR
    074 UK Meteorological Office - Bracknell UKMO
    085 French Weather Service - Toulouse FWS
    097 European Space Agency (ESA) ESA
    098 Eur. Cntr for Medium-range Wthr Frcsts - Reading ECMWF
    099 DeBilt, Netherlands DEBILT

##  CINT

CINT is the contour interval, minimum and maximum values separated by slashes:

    contour interval / minimum / maximum

The contour interval may be any real number. If it is not specified or if the value is 0, the program will select an interval which will generate 5 to 10 contour levels. The minimum and maximum values specify the range of data to use in selecting contour levels. If either value is not specified, the value will be obtained from the range of values in the dataset. If the minimum and maximum are equal, that value will be used and only one contour level will be selected.

A list of two or more contour levels may be entered using semicolons to separate the individual values. In this case, the minimum and maximum are ignored.

## CLEAR

CLEAR is a logical variable which determines whether the graphics screen is cleared before plotting. Enter YES to clear screen or NO to leave current graphics.

## CLRBAR

CLRBAR specifies the characteristics of a color bar associated with contour fill. The attributes are separated by slashes:

    color / orientation / anchor / x;y / length;width / frequency | text_info

* Color is the color of the labels and the bounding box around the color bar. If color is negative, the bounding box will not be drawn, and labels will be drawn in colors corresponding to the color bar. If the color is 0 or missing, no color bar is drawn.

* Orientation specifies a vertical or horizontal orientation of the color bar where 'V' is a vertical bar and 'H' is a horizontal bar. The default is 'V'.

* Anchor describes the location on the color bar corresponding to the location given in the next parameter. Valid inputs are LL, LC, LR, CL, CC, CR, UL, UC, and UR for lower-left, lower-center, lower-right, center-left, centered, center-right, upper-left, upper-center, and upper-right, respectively. For example, an 'LL' anchor point, with a x;y of .1,.1, will place the lower-left corner of the color bar at view coordinates .1, .1.

    The default anchor point is 'LL'. x;y is the position for the anchor point of the color bar in view coordinates. The default is .005, .05.

* Length;width are the length and width of the color bar, normalized to the view coordinates. The defaults are .5 for the length, and .01 for the width.

* Frequency describes the levels to be labeled. If the frequency is positive, the labels are plotted on the right/top of the color bar. If it is negative, the labels are plotted on the left/bottom of the color bar. The default is -1.

* Text_info describes the color bar label attributes. These attributes are of the same format as the TEXT parameter. If any part of text_info is missing, then default text information is used. The user may only change the font type, hardware/software flag, text size and text width. The default values are the bold courier hardware font and text size and text width of 1.

To disable the color bar, set `CLRBAR = 0` or leave it blank. If only contour lines are drawn (`CTYPE = C`), the CLRBAR variable is not used.

Examples:

    CLRBAR = 1

- text and bounding box in color 1
- defaults for the rest of the input


    
    CLRBAR = 5/V/ /.25;.1/1

- text and bounding box in color 5
- color bar plotted vertically
- length and width .25 and .1 of the view window
- all intervals labeled along left side of the color bar
- color bar anchor point at lower-left


    CLRBAR = 1//CL/.1;.5/.75;.1
    
- text and bounding box in color 1
- length and width .75 and .1 of the view window
- center-left of the color bar positioned at .1;.5 in view coordinates


    CLRBAR = 1//CL/.1;.5/.75;.1|.55/2/111/1/l/hw

- text and bounding box in color 1
- length and width .75 and .1 of the view window
- center-left of the color bar positioned at .1;.5 in view coordinates
- text size of color bar .55
- hardware helvetica font
- text width 2
- no box around text
- left justified


## CNTR

CNTR specifies the plot attributes for Cell Centroid movement barbs (knots):

    color / size / width ; filter

A value of 0 is used to turn off centroid plotting. FILTER is a logical variable or real number which controls the filtering of data in order to eliminate plotting of overlapping data.


## CNTRFL

CNTRFL is the name of the file containing contour information.

```
filename
```
3.19 CNTRPRM

CNTRPRM is the SFPARM to contour.

The parameter to contour must be one of the parameters listed in the SFPARM variable

for plotting. The user may set the plotting COLOR to 0 if the station data values are

not to be shown.

3.20 COLORS

COLORS specifies a list of color numbers which must be separated using semicolons:

```
color 1 ; color 2 ; ... ; color n
```
The last color in the list will be repeated, if necessary, to determine all colors required

by the program.

Colors may be entered as a range in the form: first-last-increment. If the increment is

not entered, 1 is assumed.

Color-coding of any parameter may be done based on its own value or on the value of

any other computable parameter. There is a one-to- one correspondence between the

elements in the color list and the elements in the parameter list (e.g., SFPARM). To

color-code any parameter, replace its corresponding color number in the COLORS list

with the form:

(v1;v2;...;vN-1/c1;c2;...;cN/PARM/E) or (v1-vN-1-vInc/c1-cN-cInc/PARM/E) or a

combination of a list and a range

where the v's are parameter values, the c's are color numbers, and PARM is the

parameter whose value determines the color. If PARM is omitted, the parameter will

be color-coded based on its own value. The number of values must be one less than the

number of colors. If a value range is used, an increment must be specified. If a color

range is used and no increment is specified, 1 is assumed. For example, to color-code

a marker based on the 3-hour flash flood guidance value, the color element

corresponding to the parameter MARK could be specified as:


### (0.5;1;1.5;2;3/2;19;21;22;6;4/FF03)

meaning that values less than or equal to .5 inch would be plotted with color 2, values

greater than .5 but less than or equal to 1 would be plotted with color 19, etc.

The E is a flag to indicate whether to include the break value with the lower range or

the upper range of values. Valid values are L and U, respectively. The default is L for

inclusion in the lower range of values. In the above example, the first data range is X

<= 0.5, the second is 0.5 < X <= 1.0, etc. If the U flag is added, the ranges change to

X < 0.5, 0.5 <= X < 1.0, etc.

If the input is blank, a default of 1 is used. If a color number of 0 is entered, that

parameter or line will not be plotted.

New color assignments may be made by appending the following commands to the

color number:

```
=INIT -- all colors are initialized to device-dependent
colors
```
```
=NAME -- this color number is set to the color specified
in NAME
```
```
=r:g:b -- this color number is set using RGB components in
r, g, and b which must be separated using colons.
These components must be in the range 0 - 255.
```
Note that only =INIT may be appended to a range.

Color number 101 is the background color.

Note that the GIF driver does not accept NAME.

The following colors are the default colors for the given devices:


## XW,NC PS,PSP PSC GEMPAK NAMES X COLOR NAMES

```
2 2 RED red
3 3 GREEN green
4 4 BLUE blue
5 5 YELLOW yellow
6 6 CYAN cyan
```
31 32 WHITE white

- 1 31 VANILLA bisque
- 8 8 BROWN sienna 7 7 MAGENTA magenta
- 9 9 CORAL sienna
- 10 10 APRICOT tan
- 11 11 PINK LightPink
- 12 12 DKPINK IndianRed
- 13 13 MDVIOLET firebrick
- 14 14 MAROON red
- 15 15 FIREBRICK red
- 16 16 ORRED OrangeRed
- 17 17 ORANGE DarkOrange
- 18 18 DKORANGE orange
- 19 19 GOLD gold
- 20 20 DKYELLOW yellow
- 21 21 LWNGREEN chartreuse
- 22 22 MDGREEN green
- 23 23 DKGREEN green
- 24 24 GRPBLUE DodgerBlue
- 25 25 LTBLUE DodgerBlue
- 26 26 SKY DeepSkyBlue
- 27 27 MDCYAN cyan
- 28 28 VIOLET MediumPurple
- 29 29 PURPLE purple
- 30 30 PLUM magenta
   - 2 G95 gray 32 1 1 BLACK black
   - 3 G90 gray
   - 4 G85 gray
   - 5 G80 gray
   - 6 G75 gray
   - 7 G70 gray
   - 8 G65 gray
   - 9 G60 gray
   - 10 G55 gray
   - 11 G50 gray
   - 12 G45 gray
   - 13 G40 gray
   - 14 G35 gray
   - 15 G30 gray
   - 16 G25 gray
   - 17 G20 gray
   - 18 G15 gray
   - 19 G10 gray
   - 20 G05 gray


On the color devices, colors 8 through 30 will provide a range of colors from brown to

red to orange to yellow to green to blue to violet. On the Postscript device, 20 gray

shades are used.

3.21 COLUMN

COLUMN specifies the number of columns for plotting the contents of an ASCII text

file specified by TXTFIL.

3.22 COMPRESS

COMPRESS is a flag to determine whether the output will be written in compressed

format.

Enter YES to write in compressed format. NO to write in uncompressed.

3.23 CONTUR

CONTUR sets attributes for the contour algorithms:

```
subbox factor / number of smoothing passes
```
Each grid box will be subdivided into the number of subboxes specified by the subbox

factor. Increasing this value will produce smoother contours, but will increase

contouring times and make metafiles larger. The new default value in GEMPAK 5.2 is

0.

A simple three-point smoothing filter is used if the number of smoothing passes is

greater than 0. The default is 0.

3.24 CPYFIL

FORM: Grid file whose navigation is to be used in new grid file | subarea

CPYFIL identifies the location of the grid navigation and analysis information to be

stored in a grid file, as well as an optional subarea. Three options are available:

1. If CPYFIL is blank, the information is taken from the input
    for PROJ, GRDAREA, KXKY, and ANLYSS.


2. If CPYFIL begins with a #, the information is read from the
    line in the grid navigation table which has the grid name or
    number corresponding to the rest of CPYFIL.
3. If CPYFIL is the name of a current grid file, the grid and
    navigation information will be copied from that file.

```
For cases 2) and 3), the information may be followed by a vertical bar
and a specified subarea. This subarea has the same meaning as GAREA and
may take on the same values. The grid file that gets created has the
same navigation and grid spacing as that indicated by the first part
except over a smaller area.
```
```
The input for CPYFIL may also be a file type (model alias). Valid
aliases may be found in the table "datatype.tbl" with CATEGORY
(column 4) equal to CAT_GRD. When one of these file types is input,
the program searches locally for the most recent file first, then
locally for the previous file. If no files are found locally, the
program searches remotely for the most recent file, then remotely for
the previous file.
```
```
NOTE: If CPYFIL is not blank, the values of PROJ, GRDAREA, KXKY,
and ANLYSS will be ignored.
```
3.25 CSIG

CSIG is the ending valid time for the convective sigmet and convective outlook, the

colors for the initial hour (0-hr) CSIG, extrapolated 1-hr CSIG, extrapolated 2-hr CSIG,

and outlook, and flags for plotting the 0-hr CSIG sequence number, end time, direction/

speed, flight level, intensity, 1-hr CSIG sequence number, and 2-hr CSIG sequence

number on the map.

```
End time|0_Hr;1_Hr;2_Hr;OL clrs|Seq0|Tm|Mv|Fl|Insty|Seq1|Seq
```
Convective sigmets, extrapolated 1-hr and 2-hr CSIGs, and outlooks that are valid at

the ending time will be plotted on the map. The ending time is given as a GEMPAK

date/time string. Any missing items from the string will be filled in by the system time.

The ending time may also be LAST or ALL. LAST will use the system time and plot

all current convective sigmets, extrapolated CSIGs, and outlooks. ALL will plot all the

convective sigmets, extrapolated CSIGs and outlooks in the data files for the last ten

days, whether they are active or expired.

The colors are separated by a semi-colon. If any color is set to 0, that line will not be

plotted. If any color is missing, a default will be used. Defaults are red for 0-hour

CSIG, blue for 1-hour CSIG, yellow for 2-hour CSIG, and green for outlooks.


The sequence number ID flag (Seq0) is YES or NO and controls whether or not to plot

the identifying text for the convective sigmets and outlooks on the map. The default is

NO.

The time flag is YES or NO and controls whether or not to plot the end time of the

convective sigmet and outlook on the map. The default is NO.

The movement flag is YES or NO and controls whether or not to plot the direction from

north in degrees and the speed in knots on the map. The default is NO.

The flight level flag is YES or NO and controls whether or not to plot the flight level in

hundreds of feet on the map. The default is NO.

The intensity flag is YES or NO and controls whether or not to plot the intensity of the

area, line or isolated area on the map. The default is NO.

The sequence number ID flag (Seq1) is YES or NO and controls whether or not to plot

the identifying text for the extrapolated 1-hr convective sigmets on the map. The

default is NO.

The sequence number ID flag (Seq2) is YES or NO and controls whether or not to plot

the identifying text for the extrapolated 2-hr convective sigmets on the map. The

default is NO.

3.26 CTLFLG

CTLFLG is a logical flag which indicates whether control characters are included in a

raw surface data set to be decoded.

The raw surface data set must contain either bulletins from the Domestic Data Service

or single station reports. If the data set contains bulletins, set CTLFLG = YES.

Otherwise, it will be assumed that the file contains single station reports with one report

per line.

If CTLFLG = YES, the input file must contain bulletins. If CTLFLG = NO, the file

must have reports.

3.27 CTYPE

CTYPE specifies the contouring algorithms to use separated by slashes:

```
type 1 / type 2 / ... / type n
```

Valid inputs for type are:

```
C the original GEMPAK contouring algorithm
L GEMPAK contouring algorithm without subboxes
F contour fill algorithm
B box algorithm -- same as contour fill, but draws lines
around the polygons rather than filling in polygons.
```
Note that contour attributes are specified in CONTUR. The contour interval and line

characteristics for types C, L, S and B are read from CINT and LINE and for type F

from FINT and FLINE.

Examples:

```
CTYPE = C -- draws contour lines
```
```
CTYPE =C/F -- draws filled contours overlaid with lines
```
3.28 CURVE

CURVE is a number corresponding to the method to be used to fit the curve. Generally,

CURVE should be set to 2.

The curve types currently available are:

```
1 - piecewise straight line
2 - cubic spline
21 - cubic spline with linear ends
22 - cubic spline with parabolic ends
23 - cubic spline with extrapolated ends
```
Note that types 2 and 21 are the same. Types 22 and 23 produce slightly different

results at the ends of the splines.

3.29 CXSTNS

CXSTNS defines the x-axis for a cross-section plot.

In SNCROSS, CXSTNS is the list of stations, separated with semicolons, to be used for

the cross section. Stations may be entered using either character or numeric identifiers.

In GDCROSS, CXSTNS contains the endpoints of the cross-section line separated

using a >. Each endpoint may be defined as follows:

1. a station character or numeric identifier;


2. a latitude and longitude pair separated by a semicolon;
3. an @ followed by a grid x and y coordinate pair separated
    using a semicolon, e.g., @1.5;2.3>@25.9;30.
4. graphically using the CURSOR command.

3.30 DATOUT

DATOUT is the date and time which will be assigned in the output file.

DATOUT is an 11-character string entered as YYMMDD/HHMM, where:

```
YY is the last two digits of the year
MM is the month
DD is the day of the month
/ is the date and time separator
HH is the hour
MM is the minutes past the hour
```
The part before the / is the DATE; the part after the / is the TIME.

A list of times may be entered for DATOUT. Times in the list must be separated with

semicolons. For example:

### 940513/1100;940412/1300;940515/

```
or
```
### 13/11;0412/13;0515/6.

In the above example, the times are:

### 940513/

```
940412/1300 and
940515/0600.
```
The options LAST, LIST, ALL and /ALL are not supported by DATOUT.

3.31 DATTIM

DATTIM is the date and time to be used by GEMPAK programs. DATTIM is an 11-

character string entered as YYMMDD/HHMM, where:


```
YY is the last two digits of the year
MM is the month
DD is the day of the month
/ is the date and time separator
HH is the hour
MM is the minutes past the hour
```
The part before the / is the DATE; the part after the / is the TIME.

DATTIM may be abbreviated. If the input has no / , it is assumed to be the TIME part.

An abbreviated version of either TIME or DATE is assumed to be the part closest to the

/. The rest of the standard time is obtained from the last time in the file. For example,

if the last time in the file is 940515/1200 the following translations will be done:

### 13/11 ----> 940513/

### 13 ----> 940515/

### 13/ ----> 940513/

### 0412/1300 ----> 940412/

A list of times may be entered for DATTIM. Times in the list must be separated with

semicolons. For example:

### 13/11;0412/13;0515/

In the above example, the times are:

### 940513/

```
940412/1300 and
940515/0600.
```
DATTIM may also be entered as a range. The first and last times must be separated by

a minus sign. For example:

### 13/11-15/

A range with an increment may also be entered using minus signs as separators. The

format of the increment is hhhmm. If the increment is one or two digits, it is assumed

to be in hours.

The following options are also valid for DATTIM:

```
LAST -- the last time in the file
LIST -- lists all times and waits for input
ALL -- all the times in the file
/ALL -- all the times for a single DATE.
```
If /ALL is entered, all the times for the most recent date are processed. If /ALL is

appended to a specific date, then all the times for that date will be processed.


3.32 DELTAN

DELTAN is the average station spacing in degrees of latitude. The Barnes objective

analysis programs use this number to compute weights for data interpolation.

3.33 DELTAX

DELTAX is the spacing between grid points in the x direction on CED grids. This

value is in degrees longitude.

3.34 DELTAY

DELTAY is the spacing between grid points in the y direction on CED grids. This

value is in degrees latitude.

3.35 DELZ

DELZ is the user chosen average height difference between pressure levels to be used

in the vertical interpolation. The default value is 500 m.

3.36 DEVICE

DEVICE specifies the graphics device:

```
device | name | xsize ; ysize | color type
```
The following graphics devices are available:

```
XW X Windows terminal
PS PostScript
NC NTRANS Computer Graphics Metafile
GF GIF file using X-server
GIF GIF file not using X-server
UTF Universal Transmission Format (AFOS)
RBK AWIPS RedBook Graphics
VG Vector Graphics (N-AWIPS Product Generation)
XWP Combination driver
FAX Fax driver
TIFF TIFF file
```

The following three PostScript device names are included for backward compatibility

only. The appropriate settings for the PS device are automatically selected based on the

information below.

```
PS1 PostScript - Landscape / Letter / Monochrome
PSC PostScript - Landscape / Letter / 32 Colors
PSP PostScript - Portrait / Letter / 20 Grays
```
The XWP driver combines the capabilities of several drivers. Currently, it includes the

XW and the PS drivers. XWP allows the user to display graphics to a window, then,

without closing the window, a PostScript file may be created.

The UTF driver produces output for AFOS devices and the NAFOS product. Note that

while both AFOS and NAFOS read files in universal transmission format, the exact file

formats for are not compatible. The default output is to an AFOS formatted file. The

color control (see below) is used to switch between output types.

The RBK driver produces output for AWIPS devices and OSO products. These

products are created according to the Standard Formats for Weather Data Exchange

format. However, the OSO format has an extra transmission header information

section. The default file is of an OSO format. The color control (see below) is used to

switch between output types.

The FAX driver produces an NMC 6 bit formatted file.

The TIFF driver produces a TIFF file that uses the Group 4 Fax compression scheme.

It can also optionally create a file with no compression.

The VG driver produces a GEMPAK Vector Graphics file. This file can then be used

in product generation for graphical editing of the elements. When producing a VG file,

CLEAR has no effect, i.e., all elements are added to the output file regardless of the

value of CLEAR.

NAME is the file or window name. In the NC driver, if the file already exists, new

frames will be appended to it unless the default file name of Nmeta is used. In this case,

the file is overwritten. The PS driver also uses the file name to generate plot files,

however no appending is attempted. Files are always overwritten. In the XW or XWP

drivers, up to five named windows may be opened.

If no file/window name is given, the following will be used:


### XWP GEMPAK

### XW GEMPAK

```
PS ps.plt
NC Nmeta
GF gempak.gif
GIF gempak.gif
VG vgf.vgf
FAX 999X;0167 (produces the file 999X.6bt)
UTF T01
RBK T01
TIFF AAAA00
```
```
PS1 ps1.plt
PSC psc.plt
PSP psp.plt
```
The UTF driver defaults to an AFOS test product PIL if no entry is given. The file name

may be the last three characters of a valid PIL, or a full file name. If the file name is a

PIL, the product table is read for specific size and time attributes. If a regular file name

is given, suitable defaults are chosen for the size and forecast time. The user may also

give a base time for the product. This is added to the file name following a semi-colon,

i.e., "T01;1200". This product would have a base time of 1200 GMT. The forecast

hour read from the product table is added to the base time to give a valid time for the

product.

The RBK driver defaults to an OSO formatted file if no entry is given. The file name

must be the last three characters of a valid AFOS PIL, or the test PIL id, T01. The PIL

id is used by the product table to find the specific size and time attributes. The

difference between the AWIPS format and the OSO format is the information given at

the beginning of the file and that the AWIPS format does not have any padding of

spaces at the end of the file. Similar to the UTF driver, the user may also give a base

time for the product. This is added to the file name following a semi-colon, i.e.,

"93E;311200". This product would have a base date of the 31st and a base time of 1200

GMT. The base date may also be given in the full GEMPAK date/time format, i.e.,

"9JH;000931/1200". The forecast hour read from the product table is added to the base

time to give a valid time for the product. If the base date or time is not specified, then

the system date or time will be used for the valid time.

The FAX file name entry equates to a wheel and subset for the output product. Every

fax product must have an associated wheel and subset number. The driver uses the

supplied wheel and subset number to determine the dimensions of the product. These

dimensions are defined in the product table, faxprod.tbl. This file will be named

"XXXX.6bt" where XXXX represents the wheel number of the product. The subset

number is embedded in the compressed 6 bit image and is only meaningful to OSO

systems that transmit products.


The TIFF file name entry is the WMO ID of the product. The driver uses the supplied

ID to determine the dimensions of the product. These dimensions are defined in the

product table, tiffprod.tbl. The output file will be named "AAAAII.tiff" where

AAAAII represents the 4 character and 2 digit WMO ID of the product.

XSIZE and YSIZE are the device dimensions. The specification of their values is a

function of the device driver.

For XW or XWP, xsize and ysize may be (1) the number of pixels in the corresponding

direction if the values are specified as integers, or (2) a fraction of the screen size if the

values are specfied as reals > 0 and < = 1.

For the PS driver, xsize and ysize are the paper dimensions in inches. If xsize is greater

than ysize, then a landscape orientation is used. For xsize less than ysize, the plot

orientation is portrait.

For UTF driver, xsize and ysize have no meaning, as the dimensions for the display are

determined by the entry on the product table (product.tbl) that matches the user

supplied pil.

For the NC driver, xsize and ysize are fractions of the maximum dimension of a

NTRANS CGM file. Their values must be > 0 and < = 1. If both values are less than

1, then the larger value is set to 1 and the smaller value is increased proportionally to

maintain the aspect value of the user input values.

If xsize and ysize are not specified the following defaults will be used:

```
XWP .7;.7 (70 percent of the screen size)
XW .7;.7 (70 percent of the screen size)
PS 11;8.5 (Standard letter size paper in landscape)
NC 1.0;1.0 (Standard square CGM plotting space)
```
COLOR TYPE is a flag for specifying the color scheme to use for the driver. Currently,

only PS will use the color type. Color type may have the following values:

```
C 32 Colors
G 20 Grays
M Monochrome lines with color fills
```
The default is to use 20 grays. The exceptions to this are noted below.

For UTF (which is a monochrome device), a value of C is used to switch the output to

an NAFOS formatted file. Any other value for color type results in an AFOS formatted

file.


For RBK (which is a monochrome device), a value of C is used to switch the output to

an AWIPS formatted file. Any other value for color type results in an OSO formatted

file.

For TIFF, the default is monochrome output with a white background and all graphics

drawn in black. This type of output will be compressed using the Group 4 Fax

compression scheme. A value of G will create an output file that has a black

background with all graphics drawn in white. It will also allow a grayscale image to be

written to the output file. This type of output will not be compressed.

The GF driver has the same options as the XW driver. The differences in execution are

that the GF device will not display a graphics window, and will create a separate file

for each active frame either when the device is closed or a new file name is given. The

file names are based on the user supplied file name, with the frame number appended

to the name after the initial file has been created. That is, given the default name, the

first GF file created will be named "gempak.gf", the following file will be named

"gempak.gf.001".

3.37 DITHER

DITHER is used to specify the plotting behavior of the reflectivity intensities. A value

of 0 specifies no dithering, the grid box is completely filled. Values 1-4 increase the

number of dither points per grid box, approaching an increasingly opaque value. Dither

can be used to overlay image data with radar reflectivity in order to simulate

transparency. A dither value >= 5 will outline the grid box.

3.38 DTAAREA

DTAAREA defines the area over which station data will be input to the Barnes

objective analysis. Since data must be interpolated from the first pass grid back to

stations, only data within the EXTEND area will be used after the first pass. If the

DTAAREA is not specified by the user, it will default to the EXTEND area in

OAGRID or to the data area stored in the grid file analysis block in OABSFC and

OABSND.

DTAAREA can be specified in three ways:

1. lat1;lon1;lat2;lon2
    This defines a latitude/longitude range where (lat1, lon1) is
    the lower left corner and (lat2, lon2) is the upper right corner.
    West longitude is negative.

```
#clat;clon;dlat;dlon
This defines a latitude/longitude range by the center latitude
```

```
and longitude. The lower left corner is (clat-dlat; clon-dlon);
the upper right corner is (clat+dlat; clon+dlon). No corrections
are made for the poles or the International Date Line.
```
### 2. GEOG

```
This is an abbreviation for a geographic area defined in the
GEMPAK geographic table which includes abbreviations for states,
provinces, and countries, as well as other names. A suffixed + or *
will decrease the extent of the geographic area. A suffixed - will
increase the extent of the geographic area.
```
### 3. STN

```
This defines an area centered on a station found in the GEMPAK
station table, which currently contains US, Canadian and
Mexican stations. A suffixed + or * will expand, and - will reduce
the area.
```
Note that the other ways of specifying AREA are not valid for DTAAREA in OAGRID

but are valid in both OABSFC and OABSND.

For all projections, the lat/lon corners defined will be used exactly. For projections

which include the pole, if lat1 = lat2 and lon1 = lon2, then lat1 will specify the range of

data from the pole and lon1 will specify the central longitude. West longitude is

negative.

Note that the stations actually used for the data area may be changed in the objective

analysis programs using DTAAREA.

3.39 ECHO

ECHO specifies whether to plot the grid box intesities. A value of 0 specifies no

plotting of echoes; 1 specifies plotting only those echoes from radars in precipitation

mode; 2 specifies plotting of echoes from radars in both precipitation and clear air

mode.

3.40 EXTEND

EXTEND specifies the numbers of grid points beyond the GRDAREA which define the

grid extend area in the Barnes objective analysis. The first pass is computed on the

extend area to reduce edge effects on the GRDAREA.

EXTEND is specified as four integers, which are the number of grid points to extend

the grid left, down, right, and up. The values are separated by semicolons. The default

for EXTEND is 2;2;2;2.


3.41 FAXFIL

FAXFIL is the name of a 6-bit FAX product.

3.42 FHOUR

FHOUR is the forecast hour, e.g., 18, or 24, which defines the "f" value in the

BUFRFIL name, as follows:

```
FHOUR f
----- ---
0A
6B
12 C
18 D
24 E
30 F
36 G
42 H
48 I
60 J
72 K
```
It also defines the forecast hour for the ASCII intermediate input/output file, UKAFIL.

3.43 FILTER

FILTER is a logical variable or real number which controls the filtering of data in order

to eliminate plotting of overlapping data.

If FILTER is YES, the data will be filtered.

If FILTER is NO, 0, or blank, all data will be plotted.

If FILTER is set to a real number, the default filter will be scaled by that number.

FILTER = 1 corresponds to FILTER = YES. 0 < FILTER < 1 allows some data overlap.

FILTER > 1 causes data to be more widely spaced.

3.44 FILTYP

FILTYP is the filter type. Valid input are NONE, LIGHT and HEAVY.


A LIGHT filter will smooth the data to allow a mesoscale analysis. A HEAVY filter

will smooth the data to only a large scale analysis. A filter of NONE will not smooth

the data.

Only the wind data and the virtual potential temperature are filtered to aid in the

computation of the derivatives needed for the shear and Scorer parameter calculations.

3.45 FINT

FINT is the contour fill interval, minimum and maximum values separated by slashes:

```
fill interval / minimum / maximum
```
The contour fill interval may be any real number. If it is not specified or if the value is

0, the program will select an interval which will generate 5 to 10 contour fill levels.

The minimum and maximum values specify the range of data to use in selecting the fill

levels. If either value is not specified, the value will be obtained from the range of

values in the dataset. If the minimum and maximum are equal, that value will be used

and only one contour fill level will be selected; however, since the number of colors is

one greater than the number of fill levels, two colors will be needed--the first for filling

regions with values less than the input value and the second for filling regions of greater

value.

A list of two or more fill levels may be entered using semicolons to separate the

individual values. In this case, the minimum and maximum are ignored.

3.46 FLINE

FLINE is the color and fill type to be used for contour fill:

```
colr1;..;colrn/type1;..;typen
```
The number of fill colors and types needed is one greater than the number of fill levels

in FINT. The number of fill colors may be entered as a list of color numbers separated

by semicolons or a range of colors. The number of fill types may be entered as a list of

numbers separated by semicolons. More information on color selection can be found

in the help for COLORS.

The fill type may be set any of the following values:


```
1 Solid
2 Slanted Dash
3 Wide-spaced Slanted Line
4 Medium-spaced Slanted Line
5 Zig-Zag Line
6 Dots
7 Thin-spaced Slanted Line
```
If fill type is set to 0, soild fill is used. If the fill type is set to a single negative number,

negative values will use the absolute value of the fill type, and positive values will be

solid.

3.47 FXYTBL

FXYTBL is the the FXY table file names in the format of

### FXYTBL = ...;...;...

```
or SWH
```
where the individual items are the FXY file names (e.g., fxyswhcld;fxyswhfrt) and the

SWH is an alias which refers to all FXY files for the high level sigwx message as listed

in the master table.

3.48 GAMMA

GAMMA, the convergence parameter, is a multiplier for the weight and search radius

for passes after the first pass of the Barnes analysis programs. GAMMA must be within

the range 0 - 1. Any value outside this range will default to a value of 0.3. If GAMMA

is 0, the number of passes will be set to 1. The recommended value for GAMMA is 0.3.

3.49 GAREA

GAREA is the graphics area. This is the area which will be displayed on a graphics

device.

GAREA can be specified in the following:

1. lat1;lon1;lat2;lon2
    This defines a latitude/longitude range where (lat1, lon1) is
    the lower left corner and (lat2, lon2) is the upper right corner.
    West longitude is negative.


```
#clat;clon;dlat;dlon
This defines a latitude/longitude range by the center latitude
and longitude. The lower left corner is (clat-dlat; clon-dlon);
the upper right corner is (clat+dlat; clon+dlon). No corrections
are made for the poles or the International Date Line.
```
### 2. GEOG

```
This is an abbreviation for a geographic area defined in the
GEMPAK geographic table which includes abbreviations for states,
provinces, and countries, as well as other names. A suffixed + or *
will decrease the extent of the geographic area. A suffixed - will
increase the extent of the geographic area.
```
### 3. STN

```
This defines an area centered on a station found in the GEMPAK
station table, which currently contains US, Canadian and
Mexican stations. A suffixed + or * will expand, and - will reduce
the area.
```
4. Graphically using the CURSOR command.

Note that the other ways of specifying AREA are not valid for GAREA.

For all projections, the lat/lon corners defined will be used exactly. (If GAREAX is

used with the CURSOR command, the display area attempts to fill the entire view area,

extending the area defined by the selected lat/lon). For projections which include the

pole, if lat1 = lat2 and lon1 = lon2, then lat1 will specify the range of data from the pole

and lon1 will specify the central longitude. If the projection has been rotated so that a

geographic pole is no longer at the center and lat1 = lat2 and lon1 = lon2, then lat1 will

determine the range of data from the rotated pole and lon1 specifies the central

longitude as before. If a cylindrical equidistant projection has been rotated for any

angle1 between 90 and -90 degrees, then the entire world can be displayed by entering

GAREA=0;0;0;0. The central longitude is taken from the PROJ specification for

angle2. This works for angle1 = 0 as well.

West longitude is negative.

For overlay on satellite and radar imagery, PROJ must be set to SAT and RAD,

respectively.

3.50 GBDIAG

GBDIAG allows for detailed GRIB message section information, byte-by-byte, to be

printed out for selected GRIB messages. Simply list those GRIB sections to be

examined (IDS, PDS, GDS, BMS, BDS, END or ALL for all sections), e.g., pds;gds.

Selected GRIB messages for examination may also be identified by number in list and/

or range format, e.g., 2;4;5-9, immediately following the section specification


separated by a "|". For example, GBDIAG=pds;gds|2;4;6-12. Please note that the

BMS section will be listed only if it exists and that the sections BDS and END will be

listed only if the GRIB data is actually being decoded and written into the GEMPAK

file.

3.51 GBFILE

GBFILE is the name of the file which contains gridded data in GRIB messages.

3.52 GBTBLS

GBTBLS allows for specification of the GRIB decoding tables. The format is

GBTBLS=W;N;V;C, where W is the WMO decoding table for parameters 1-127, N is

the NCEP decoding table for parameters 128-255, V is the vertical coordinate table and

C is the originating center table. The defaults are
$GEMTBL/grid/wmogribX.tbl,
$GEMTBL/grid/ncepgribX.tbl,
$GEMTBL/grid/vcrdgribX.tbl,
$GEMTBL/grid/cntrgribX.tbl,
where X is the version number of the GRIB message.

3.53 GCENTER

GCENTER sets the center latitude-longitude on the ETA model domain

### GCENTER = TPH0D;TLM0D

TPH0D and TLM0D are the center latitude (W is negative) and longitude (E is positive)

of the computational domain. Note that it is not necessary to to recompile the model if

you change the areal coverage of the model domain as long as the number of grid points

remains the same.

For a grid centered at 108 W, 39 N (Centered over Colorado)

### TPH0D = 39.0

```
TLM0D = -108.0 Note the Negative for W
```
set GCENTER = 39.0;-108.0


3.54 GDATTIM

GDATTIM is the date/time for the grid.

Grids may contain two date/time fields, in which case the fields must be separated using

a colon. If the grid to be selected contains only one time, the colon and second time

may be omitted.

The standard format for a grid time is a character string

YYMMDD/HHMMthhhmm
where:
YYMMDD is the year, month, day
/ is the date and time separator
HHMM is the hour, minute
t is the type
( F=forecast A=analysis G=guess V=valid )
hhhmm is the forecast hour, minute

If t is blank, an analysis grid is assumed. If hhhmm is blank, 00000 is assumed. If

hhhmm has one, two, or three digits, they represent hours. With four or more digits,

zeros will be added at the beginning of the field.

The field YYMMDD is called the date; HHMM is the time; t is the type; hhhmm is the

forecast time. The fields may be abbreviated. If the input has no /, it is assumed to be

the time part. Any abbreviated version of either date or time is assumed to be the part

closest to the /. The rest of the date and time is obtained from the last time in the file.

If the type is missing, then A is assumed, and a missing forecast time is replaced by

00000.

The type and forecast time fields are used with forecast model data. If GDATTIM is

940831/0000F24, the grid to be found is the 24-hour forecast from the model run at 00Z

on Aug 31. If GDATTIM is 940831/0000V24, the grid to be found is the 24 hour

forecast valid for 00Z on Aug 31 from the model run at 00Z on Aug 30.

Examples (with the last time = 941205/1600 ):

### GDATTIM TIME1 TIME2

### ----------- -------------- -----------

### 941205/1200 941205/1200

### 12:15 941205/1200 941205/1500

### LAST 941205/1600

### /00F24 941205/0000F24

```
FALL all forecast hours
ALLF24 24 h forecast of
all forecast cycles
```

The value in GDATTIM may be overridden by specifying ^GDATTIM with the grids

to be found. For example, GFUNC = SUB (TMPF^28/12,TMPF^27/12) will compute

the same time difference as GFUNC = TDF (TMPF) with GDATTIM=28/12:27/12.

Note that a grid from an objective analysis will have forecast type and time A00000,

which is the default for a blank forecast type and time.

3.55 GDEFIL

GDEFIL is the name of the grid edit file which will be used to update a grid file. The

edit file must contain only complete grids.

GDEFIL is a text file which may be created using the program GDLIST with F as an

output device and GAREA = DSET. A text editor may be used to create or change the

grid edit file.

GDEFIL must contain the following information before the complete grid data:

The time, level, vertical coordinate and parameter name MUST appear on the same line

in the order given. The time must be a fully qualified GEMPAK time. The parameter

name is the name of the grid.

The first and last row and column information must appear on one line. Column

information must be preceded by the word COLUMNS:. Row information must be

preceded by the word ROW:. The first row and column must be 1. The last row and

column must be the same as the grid size in the file. For example:

### COLUMNS: 1 8 ROWS: 1 6

If the string FACTOR:, appears and is followed by an integer, the data will be divided

by 10**SCALE before being stored in the grid file. In the following example, the data

will be divided by 10**5.

### SCALE FACTOR: 10**5

The string COLUMN: signals the beginning of the grid data. If the required grid

information has not already been found, an error will result.

The data for the grid follows the grid keywords. The data for a grid is listed from the

top (last) row to the bottom row in the grid. The beginning of each row of data must be

preceded by the string ROW. Data consists of numeric values for each data point;

missing data should be entered as -9999.0.


3.56 GDFILE

GDFILE is the name of the file which contains gridded data.

The input for GDFILE may contain more than one file name separated by +. Up to three

file names may be entered. Individual operands in GFUNC or GVECT can be accessed

from a specific file by placing +n after the operand name, where n is the number

corresponding to the placement of the file name in the GDFILE input. In the example

below, the temperature from the third file is advected by winds from the first file:

```
GDFILE = file1.grd + file2.grd + file3.grd
GFUNC = ADV ( TMPK+3, WND )
```
The input for GDFILE may also be a file type. The following file types are examples

of the possible input. See the table, datatype.tbl, for the actual file types that can be

used.

```
ETA ETA model grid
NGM NGM model grid
GFS GFS model grid
MRF MRF model grid
ECMWFG ECMWF model grid (global)
ECMWFT ECMWF model grid (tropical)
FNL Final analysis
RUC2 Rapid update cycle
RUCS Rapid update cycle surface
UKMET UKMET model grid
```
When one of these file types is input, the program searches locally for the most recent

file first. If no files are found locally, the program searches remotely for the most recent

file. The remote directories are specified in the table DATA.TBL.

An optional base time may be included with the file type name, following a bar (|). For

example:

```
GDFILE = eta | 00
```
This will attempt to find the most recent run of the ETA model at, or prior to, 00Z. The

base time may be any valid format for a GEMPAK date/time. Any missing parts of the

date/time will be supplied by the system time. For GDPLOT2, the base time is the cycle

time. It will attempt to find the most recent run of the ETA model at 00Z cycle.

3.57 GDNUM

GDNUM allows the user to select grids by number in the following ways:


GDNUM = range
The first and last grid numbers in the range are separated by a -. If an increment is

included, it will be ignored.

GDNUM = list
The grid numbers in the list are separated using semicolons.

### GDNUM = ALL

All the grids in the file will be included.

### GDNUM = LIST

All the grids in the file will be listed. The user will be prompted to select the grids to

be included.

3.58 GDOUTF

GDOUTF is the output grid data file name.

GDOUTF is used in programs which create a new grid data file. It is also used in

programs that move data from an input file to an output file. In that case, GDFILE is

the original file.

The file types that can be used for GDFILE cannot be used to specify the GDOUTF

value.

3.59 GDPFUN

GDPFUN specifies a grid diagnostic function which yields either a scalar or vector

quantity. For more information, see the GPARM documentation.

3.60 GFUNC

GFUNC specifies a grid diagnostic function which yields a scalar quantity. For more

information, see the GPARM documentation.

3.61 GGLIMS

GGLIMS is the parameter which controls the grid value limits and values.


```
lochk;loval | hichk;hival | default
```
Grid values less than lochk are set to loval. Grid values greater than hichk are set to

hival.

If no contours are available in the CNTRFL file, the output grid is assigned "default" at

all grid point locations.

Example: for a precipitation grid, set all negative values to zero:

### GGLIMS 0;0

Example: in previous example, if no contours are provided, set all grid point values to

zero:

### GGLIMS 0;0 | | 0.0

3.62 GLEVEL

GLEVEL is the vertical level for the grid.

Grids may contain two levels separated by a colon. If the grid to be selected contains

only one level, the colon and second level may be omitted. In this case, the second level

is stored in the grid file as -1.

Note that the vertical coordinate system for GLEVEL is specified by GVCORD.

The value in GLEVEL may be overridden by specifying @GLEVEL with the grids to

be found. For example, the following two computations are identical:

### GFUNC = SUB (TMPF@850,TMPF@500)

```
GFUNC = LDF (TMPF) and GLEVEL = 850:500.
```
3.63 GPACK

GPACK is the packing type and the number of bits (or data precision) to be used to pack

the grid data, separated with a slash:

```
packing type / number of bits (NONE, GRIB, DIF)
```
```
packing type / precision (DEC)
```
The valid packing types are:


```
NONE No packing
GRIB Data is packed in GEMPAK GRIB format
DEC Data is packed in GEMPAK GRIB format
DIF Data is packed in GEMPAK DIF format
```
If the packing type is DEC, the number of bits is replaced by the data precision. The

data will be scaled by 10**precision and rounded to the nearest integer. The number

of bits used to store the data will be the minimum number required to store the resulting

integers.

If the number of bits is less than 32, the packing type specified will be used. If no

packing type is given, the data will be packed using the GEMPAK GRIB scheme.

In general, data should be stored using packing type GRIB with 16 bits, by specifying

the precision in DEC or with no packing using type NONE. Care should be taken using

the other packing options.

```
Pack Type Minimum Maximum Default
NONE 32 32 32
DEC -5 5 2
DIF 23216
GRIB 2 32 16
```
The default data packing when GPACK is blank is GRIB/16.

3.64 GPARM

GFUNC and GVECT are the scalar and vector grid functions. They are input as nested

strings of operators and operands. The operand list for an operator is enclosed in

parentheses or square brackets, with operands separated by semicolons or commas.

If the entire grid is small enough (less than LLMXGD grid points) and GAREA is set

to GRID, then the computations below are done over the entire grid; otherwise, the

computations are done over a subset of the grid. This subset grid is large enough to

cover the display area.

The following grids will be computed automatically from grids in the grid file, if

possible:

### TMPK DWPK TVRK MIXR THTA DRCT TMWK

### TMPC DWPC TVRC SMXR STHA SPED TMWC

### TMPF DWPF TVRF MIXS THTE RELH TMWF

### THES SMXS STHE


Mixing ratios will be computed automatically from dewpoint temperatures, specific

humidity, or vapor pressure if a pressure grid exists.

The stability indices will be computed automatically from temperature, dewpoint

temperature, and wind speed and direction. These special scalar parameters are:

### CTOT VTOT TOTL KINX SWET

Haines Indices for fire weather detection will be computed automatically from

temperature and dewpoint at three different levels. These scalar parameters are:

```
LHAN Low elevation Haines Index
MHAN Middle elevation Haines Index
HHAN High elevation Haines Index
```
The Heat Index, HEAT, will also be automatically computed from the temperature and

relative humidity.

In addition, precipitation will be converted from inches (I) to millimeters (M) and vice

versa, if the grids are named P__M or P__I. The middle characters are numbers giving

the time interval over which the precipitation accumulated. For example, P24M is a 24-

hour precipitation total.

The units for sea surface temperature (SSTx), maximum temperature (TMXx), and

minimum temperature (TMNx) will be converted automatically. (x may be K, C, or F.)

These special scalar parameter names denote constant value grids:

```
DTR Conversion factor for degrees to radians = PI / 180
E Base of natural logarithms = 2.71828182
GRAVTY Gravitational constant = 9.80616 (note spelling)
KAPPA Gas constant/specific heat = 2/7
PI 3.14159265
RTD Conversion factor for radians to degrees = 180 / PI
nnn Any number (e.g., 2, -10.2)
```
Another class of special parameter names depends on the grid navigation:

```
CORL Coriolis force = 2. * OMEGA * SIN ( LATR )
LATR Latitude in radians
LONR Longitude in radians
XVAL Value of the x coordinate in graph coordinates
YVAL Value of the y coordinate in graph coordinates
MSFX Map scale factor in the x direction
MSFY Map scale factor in the y direction
LAND Land array; land=1, sea=RMISSD
SEA Sea array; sea=1, land=RMISSD
```
```
BETA Derivative of Coriolis force with respect to y direction
```

Note: This only works correctly for cylindrical grids.

A grid may be identified by its number in the grid file by prefixing the number with the

symbol #, e.g., #5.

Standard vector grids are:

```
WND Total wind
GEO Geostrophic wind
AGE Ageostrophic wind
ISAL Isallobaric wind
THRM Thermal wind
```
Time, level, and vertical coordinate as specified through the user interface may be

overridden by in-line parameters:

```
^time @level %ivcord
```
appended to an operand in any combination.

If more than one file is opened, +n may also be used as an in-line parameter, where "n"

is the number corresponding to the position of the file list entered in GDFILE. If +n is

omitted, the first file is used. For example, to advect temperature from file 3 using

winds from file 1:

```
GDFILE =file1.grd + file2.grd + file3.grd
GFUNC =ADV ( TMPK+3, WND )
```
Grid operators may be nested. Note that layer and time range operators expect operands

read directly from the grid file.

GFUNC may include a name for the computed grid. This name may be used in later

diagnostic functions in an application. The grid is stored in an internal, circular grid list

and will eventually be overwritten. It will never be available after exiting the

application. The name is specified after two slashes at the end of GFUNC. For

example,

### GFUNC = DIV ( WND ) // DVRG

permits the divergence to be used in a later calculation as DVRG, as in this example:

### GFUNC = LAP ( DVRG )

This name will also be the default name used in the title. If a computed grid is given

the same name as one computed in a previous GFUNC specification, then the earlier

grid is replaced by the one most recently computed, even if the grids have different level

or time identifiers. Use different names to preserve both grids. To recall saved grids,


it may be necessary to use in-line identifiers following the name if the inputs for

GDATTIM, GLEVEL, or GVCORD have been changed.

In the following list of diagnostic operators, scalar operands are named Si, and vector

operands are Vi. Vector components are denoted by u and v. All meteorological grids

are in MKS units, except as noted. POL following the description indicates that the

computation currently can only be performed on polar (R, THETA) grids. In the

trigonometric functions, the angles are expressed in radians.

The scalar constants used as arguments for the radial and tangential wind computations

are defined as follows:

```
LAT = latitude of storm center
LON = longitude of storm center
D = direction of storm motion
ST = speed of storm motion
```
The second input in each of the filter functions GWFS and GWFV is an integer N. N

gives the wavelength in units of the grid spacing for which the theoretical response of

a filter with normally distributed weights is 1/e = .3679. In practice, the actual response

function will exceed the theoretical values at all wavelengths. The larger N is, the

stronger the filtering will be.

SCALAR OUTPUT GRID

```
ABS (S) Absolute value
ACOS (S) Arc cosine function
ASIN (S) Arc sine function
ATAN (S) Arc tangent function
ATN2 (S1,S2) Arc tangent function
COS (S) Cosine function
EXP (S1,S2) Exponential to real
EXPI (S1,S2) Exponential to integer [uses NINT(S2)]
INT (S) Convert to an integer
LN (S) Natural logarithm
LOG (S) Base 10 logarithm
NINT (S) Round to nearest integer
SIN (S) Sine function
SQRT (S) Square root
TAN (S) Tangent function
NCDF (S1,S2,S3) Cumulative normal distribution function
for value, mean, and std dev
PAUB (S1,S2,S3) Combine probabilities (S1 OR S2) with
dependency parameter S3
```
```
ADD (S1,S2) Addition
MUL (S1,S2) Multiplication
QUO (S1,S2) Division
SUB (S1,S2) Subtraction
```

BOOL (S) Boolean function
SLT (S1,S2) Less than function
SLE (S1,S2) Less than or equal to function
SGT (S1,S2) Greater than function
SGE (S1,S2) Greater than or equal to function
SBTW (S1,S2,S3) Between two values function
SMAX (S1,S2) Maximum of S1 and S2
SMIN (S1,S2) Minimum of S1 and S2
MASK (S1,S2) Mask S1 based on S2
MISS (S1,S2) Set missing values in S1 to S2


ADV (S,V) Advection
AVG (S1,S2) Average
AVOR (V) Absolute vorticity
BVSQ (THTA) Brunt-Vaisala frequency squared in a layer
CROS (V1,V2) Vector cross product magnitude
DDEN (PRES,TMPC) Density of dry air (kg / m**3)
DDR (S) Partial derivative with respect to R (POL)
DDT (S) Time derivative
DDX (S) Partial derivative with respect to X
DDY (S) Partial derivative with respect to Y
DEF (V) Total deformation
DIRN (V) Direction relative to north
DIRR (V) Direction relative to grid
DIV (V) Divergence
DIVT (S,V) Divergence Tendency (only for cylindrical grids)
DOT (V1,V2) Vector dot product
DTH (S) Partial deriv. with respect to THETA (POL)
FCNT (S) Coriolis force at grid center (POL)
FOSB (TMPC,RELH,
SPED) Fosberg index, also called Fire Weather Index
FRNT (THTA,V) Frontogenesis
GWFS (S,N) Filter with normal distribution of weights
HIGH (S,RADIUS) Relative maxima over a grid
(RADIUS is expressed in grid points)
JCBN (S1,S2) Jacobian determinant
KNTS (S) Convert meters / second to knots
LAP (S) Laplacian operator
LAV (S) Layer average (2 levels)
LDF (S) Layer difference (2 levels)
LOWS (S,RADIUS) Relative minima over a grid
(RADIUS is expressed in grid points)
MAG (V) Magnitude of a vector
MASS Mass per unit volume in a layer from PRES
MDIV (V) Layer-average mass divergence
MIXR (DWPC,PRES) Mixing ratio kg/kg internally, g/kg on output
MRAD (V,LAT,LON,
D,ST) Magnitude of radial wind
MSDV (S,V) Layer-avg. mass-scalar flux divergence
MSFC (V) Psuedo angular momentum (for cross sections)
MTNG (V,LAT,LON,
D,ST) Magnitude of tangential wind
NORM (V) Normal component (for cross sections)
PLAT (S) Latitude at each point (POL)
PLON (S) Longitude at each point (POL)
PLCL (PRES,TMPC,
DWPC) Pressure of the lifting condensation level
POIS (S1,S2) Solve Poisson equation of forcing function
with boundary conditions
POLF (S) Coriolis force at each point (POL)
PVOR (S,V) Potential vorticity in a layer
RELH (TMPC,DWPT) Relative humidity
RICH (V) Richardson stability number in a layer
ROSS (V1,V2) Rossby number


SAVS (S) Average over display subset area of grid
SDIV (S,V) Flux divergence of a scalar
SHR (V) Shearing deformation
SM5S (S) 5-point smoother
SM9S (S) 9-point smoother
STAB (TMPC) Lapse rate over a layer in K/km
STR (V) Stretching deformation
TANG (V) Tangential component (for cross sections)
TAV (S) Time average
TDF (S) Time difference
THES (PRES,TMPC) Saturated equivalent potential temperature
in Kelvin
THTA (TMPC,PRES) Potential temperature
THTE (PRES,TMPC,
DWPC) Equivalent potential temperature
THWC (PRES,TMPC,
DWPC) Wet bulb potential temperature in Celsius
TLCL (TMPC,DWPC) Temperature of the lifting condensation level
TMST (THTE,PRES) Parcel temperature in Kelvin along a
moist adiabat
TMWK (PRES,TMPK,
RMIX) Wet bulb temperature in Kelvin
DSUB (V1,V2) DIRN (V1) - DIRN (V2)
UN (V) North relative u component
UR (V) Grid relative u component
VN (V) North relative v component
VOR (V) Vorticity
VR (V) Grid relative v component
WNDX (S1,S2,S3,S4) WINDEX (index for microburst potential)
WSHR (V) Magnitude of the vertical wind shear in
a layer
XAV (S) Average along a display subset grid row
XSUM (S) Sum along a display subset grid row
YAV (S) Average along a display subset grid column
YSUM (S) Sum along a display subset grid column

VECTOR OUTPUT GRID


```
AGE (S) Ageostrophic wind
CIRC (V,S) Circulation (for cross section)
DVDX (V) Partial x derivative of V
DVDY (V) Partial y derivative of V
GEO (S) Geostrophic wind
GRAD (S) Gradient of a scalar
GWFV (V,N) Filter with normal distribution of weights
INAD (V1,V2) Inertial advective wind
ISAL (S) Isallobaric wind
KCRS (V) Curl of V
KNTV (V) Convert meters/second to knots
LTRN (S,V) Layer-averaged transport of a scalar
NORMV (V) Vector normal wind (for cross sections)
QVEC (S,V) Q-vector at a level
QVCL (S,V) Q-vector of a layer
RAD (V,LAT,LON,
D,ST) Radial wind
ROT (angle,V) Coordinate rotation
SMUL (S,V) Multiply a vector's components by a scalar
SM5V (V) 5-point smoother
SQUO (S,V) Vector division by a scalar
TANGV (V) Vector tangential wind (for cross sections)
THRM (S) Thermal wind over a layer
TNG (V,LAT,LON,
D,ST) Tangential wind
VADD (V1,V2) Add the components of two vectors
VASV (V1,V2) Vector component of V1 along V2
VAVS (V) Average vector over display subset grid
VECN (S1,S2) Create vector from north relative components
VECR (S1,S2) Create vector from grid relative components
VLAV (V) Layer average for a vector
VLDF (V) Layer difference for a vector
VMUL (V1,V2) Multiply the components of two vectors
VQUO (V1,V2) Divide the components of two vectors
VSUB (V1,V2) Subtract the components of two vectors
VLT (V,S) Less than function
VLE (V,S) Less than or equal to function
VGT (V,S) Greater than function
VGE (V,S) Greater than or equal to function
VBTW (V,S1,S2) Between two values function
VMSK (V,S) Mask V based on S
```
3.65 GPOINT

GPOINT is the grid location to be used for the plot. The value can be entered in the

following ways:

1. station numeric or character identifier
2. a latitude and longitude pair separated with a semicolon


3. @ followed by a grid x and y coordinate pair separated
    with a semicolon.
4. Graphically using the CURSOR command.

If necessary, the grid data will be interpolated using a bilinear interpolation to the point

specified.

Examples:

```
GPOINT = @1;1.5 Grid point (1,1.5)
```
```
GPOINT = 30;-120 Grid point at 30 latitude and
-120 longitude
```
```
GPOINT = 5;5 Grid point at 5 latitude and
5 longitude. Note that this is
probably an error and the input
should have been @5;5
```
```
GPOINT = BWI Point located at Baltimore-Washington
International Airport.
```
3.66 GRDAREA

GRDAREA specifies the area to be covered by the grid. This area is combined with

PROJ to define the region over which the grid is evenly spaced.

GRDAREA can be specified in three ways:

1. lat1;lon1;lat2;lon2
    This defines a latitude/longitude range where (lat1, lon1) is
    the lower left corner and (lat2, lon2) is the upper right corner.
    West longitude is negative.

```
#clat;clon;dlat;dlon
This defines a latitude/longitude range by the center latitude
and longitude. The lower left corner is (clat-dlat; clon-dlon);
the upper right corner is (clat+dlat; clon+dlon). No corrections
are made for the poles or the International Date Line.
```
### 2. GEOG

```
This is an abbreviation for a geographic area defined in the
GEMPAK geographic table which includes abbreviations for states,
provinces, and countries, as well as other names. A suffixed + or *
will decrease the extent of the geographic area. A suffixed - will
increase the extent of the geographic area.
```

### 3. STN

```
This defines an area centered on a station found in the GEMPAK
station table, which currently contains US, Canadian and
Mexican stations. A suffixed + or * will expand, and - will reduce
the area.
```
Note that the other ways of specifying AREA are not valid for GRDAREA.

For all projections, the lat/lon corners defined will be used exactly. For projections

which include the pole, if lat1 = lat2 and lon1 = lon2, then lat1 will specify the range of

data from the pole and lon1 will specify the central longitude. West longitude is

negative.

3.67 GRDHDR

GRDHDR is a list of the valid grid header flags, separated by slashes.

```
Horizontal Remap Type / Directional Grid Type
```
The values in GRDHDR are set in the header array of the grid stored in a GEMPAK

data file. The values for horizontal remap type define how the grid is to be interpolated

to a new navigation. The directional grid type flag identifies grids that are direction

grids and therefore must be handled differently when interpolated from one navigation

to another.

The Horizontal Remap Type has values 0, 1 or 2 defined as the following:

```
0 = Regular area averaging for continuous gribs
1 = Discrete field, use nearest neighbor
2 = Force bilinear interpolation
```
The Directional Grid Type has values 0, 1, or 2 defined as the following:

```
0 = NOT a directional field
1 = IS a directional field in degrees
2 = IS a directional field in radians
```
If any flag is not given by the user, the value is set to a default of 0.

3.68 GRDLBL

GRDLBL is the color number to be used in plotting the grid index numbers. If

GRDLBL = 0 or blank, grid index numbers are not plotted.


3.69 GRDNAM

GRDNAM is the parameter name for the grid. In GDDIAG, this is the output name of

the grid to be created. Time, level, and vertical coordinate for the grid may be

overridden by in-line parameters:

```
^time @level %ivcord
```
appended to the output name of the grid in any combination. If the name is blank, the

default name generated by the grid diagnostics package will be used.

3.70 GSKIP

GSKIP allows the NDFDG2 user to specify a skip factor in order to reduce the

resolution of the resulting GEMPAK grid.

Any positive integer may be specified. This skip factor will be applied to both the I and

J dimensions.

A warning will be issued if no value is specified and the resulting grid is too large.

3.71 GSPACE

GSPACE sets the grid spacing of the ETA model domain where:

### GSPACE = DLHD

from the eta_run.csh script.

GSPACE is the grid spacing of the computational model domain in degrees longitude

respectively. A VERY rough conversion to km over the central US would be:

```
km/150 = delta degrees
```
Thus from above:

```
10km grid spacing: GSPACE = 0.067
15km grid spacing: GSPACE = 0.098
32km grid spacing: GSPACE = 0.213
```
GSPACE is the same as the values for DLMD and DLHD in eta_run.csh.


3.72 GUESS

GUESS contains the information to use as a first guess for objective analysis programs.

The name of the grid file containing the guess field and the time to be used to extract

the field must be entered using * as a separator:

```
guess file * grid time
```
3.73 GVCORD

GVCORD is the vertical coordinate of the grid to be selected.

The standard values are:

```
NONE for surface data
PRES data in pressure coordinates (millibars)
THTA data in isentropic coordinates (Kelvin)
HGHT data in height coordinates (meters)
SGMA data in sigma coordinates
```
The value in GVCORD may be overridden by specifying %GVCORD with the grids to

be found. For example:

### GFUNC = SUB ( TMPC @850 %PRES, TMPC @1500 %HGHT )

will compute the difference between temperatures on the 850-mb level and the 1500-

meter level.

3.74 GVECT

GVECT specifies a grid diagnostic function which yields a vector quantity. For more

information, see the GPARM documentation.

3.75 HILO

HILO contains the information for plotting relative highs and lows in the following

format:

```
colorh;colorl/symbolh;symboll/rangeh;rangel/radius/counth;countl/interp
```

Colorh and colorl are the colors for the high and low symbols to be plotted. If only a

single number is entered, it will be used for both highs and lows. The default for this

entry is 0.

Symbolh and symboll specify the symbols to be plotted. The format for the symbol

input is:
character # precision

where the character is the character to be plotted. If the character is an integer, markers

corresponding to that number will be plotted. If the character is an integer preceeded

by the character 'S', special symbols corresponding to that number will be plotted.

Information about markers can be found in the help for MARKER. The # is a flag to

plot values beneath the marker. The integer following the # is the number of decimal

places to display in the value. If a # is present without the following number, integer

values are printed. The default for the symbols is H;L.

Rangeh and rangel are ranges for highs and lows specified as:
minval - maxval

where minval and maxval are integers which specify the range of values to be

considered for designation as a high or low. The default is to consider all data.

The search radius is a sqaure region of grid points. The region is a moving search area

in which all points are compared to derive relative extrema. The default is 3, which is

generally the most effective. To find more concentrated areas of highs and lows, use a

smaller radius. A large radius, such as 10 or higher, is not very effective.

Counth and countl are integer values for the maximum number of high and low values

to be displayed. The default is 20;20.

Interp is an interpolation flag which specifies whether the values and locations of the

highs and lows will be at grid points, or will be interpolated between grid points. The

default is NO.

In general, the above defaults are used if there is no entry for a part. For the parts which

have values for both relative highs and lows, a single entry will be used for both highs

and lows.

Examples:

### HILO = 1/H#;L#/1020-1040;980-1000/2/30;20

```
Color 1 for highs and lows.
H and L characters with numeric values plotted below.
Plot highs from 1020 to 1040; plot lows from 980 to 1000.
Search radius of 2.
Plot the 30 highest highs and the 20 lowest lows in the specified
```

```
range.
```
```
HILO = 2;4/X#;Y#//30/yes
```
```
Colors 2 and 4 for highs and lows.
X and L characters with numeric values plotted below.
Plot the 30 highest highs and 30 lowest lows.
Locations of highs and lows interpolated between
grid points.
```
```
HILO = 2;4/S12#;S13#//30/yes
```
```
Same as previous example except use special symbols 12 and 13
for indicating relative minima and maxima.
```
3.76 HISTGRD

HISTGRD toggles the writing of the graph-to-grid history grid to the GEMPAK grid

file. The history grid contains integral values which indicate which method was used

to assign the data grid value. The grid is primarily a diagnostic tool and is usually not

saved.

```
yes - save history grid
anything else - do not save history grid
```
The name of the history grid will be the name of the data grid concatenated with

"HIST", e.g., data P06I will be P06IHIST. The vertical coordinate and level will be the

same as the data grid.

3.77 HLSYM

HLSYM defines the characteristics for the HILO symbols specified in HILO. The text

sizes, value position, fonts, text widths and hardware/software flags are specified for

the symbols (s) and plotted values (v) as:

```
sizes;sizev/position/fonts;fontv/widths;widthv/hwflgs;hwflgv
```
The size, font, width, and hw flag are the same as the TEXT variable. If only one value

is given, it is used for both the symbol and value.

The value plotting position may be 1, 2, or 3 where 2 is the default. The number selects

the position of the value string beneath the symbol string. The three positions are

shown below:


### H

### 123

It is common for HILO symbols near the edge of the display to be hidden when

hardware text font is used. Therefore, when using hardware text font, the number of

HILO symbols displayed may be slightly less than what the user specifies.

Examples:

```
HLSYM = 2;1/3/2//HW -- symbol text size = 2
value text size = 1
plot value in position 3
hardware text font 2 applies to both
```
```
HLSYM = 2/1/1;2/ -- symbol text size = 2
value text size = 2
plot value in position 1
symbol text font = 1
value text font = 2
```
3.78 HRCN

HRCN is the ending valid time for the tropical disturbance, the colors for the

hurricanes, tropical storms, tropical depressions and directional arrows, the symbols for

the hurricanes, tropical storms and tropical depressions, and flags for plotting the center

located time, the name and minimum central pressure, the speed, the wind and sea

quadrant radii, and the forecast track on the map.

```
End time|HU;TS;TD;DA clrs|HU;TS;TD syms|Time flg|Label flg|
Speed flg|Quad Wind flag|Fcst track flg|Name
```
Locations that are valid less than 24 hours before the ending time, given as a GEMPAK

date/time string, will be plotted on the map. Any missing items from the date/time

string will be filled in by the system time. The ending time may also be LAST or ALL.

LAST will use the system time and plot all current tropical weather. ALL will plot all

the tropical weather in the data files for the last ten days.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are red for

hurricane, yellow for tropical storm, green for tropical depression, and magenta for

directional arrow.

The symbols are separated by a semi-colon. If they are not given, the default symbols

will be used. The defaults are Northern Hemisphere hurricane, Northern Hemisphere

tropical storm and tropical depression.


The time flag is YES or NO and controls whether or not to plot the location time of the

tropical disturbance on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the name of the

disturbance and the minimum central pressure on the map. The default is NO.

The speed flag is YES or NO and controls whether or not to plot the speed in knots of

the disturbance. The default is NO.

The quad wind flag is YES or NO and controls whether or not to plot the 64, 50 and 34

kts wind and 12 foot seas quadrant arcs. Only the most recent time period of each storm

will have the arcs drawn. The default is NO.

The fcst track flag is YES or NO and controls whether or not to plot the valid forecasted

latitudes and longitudes for all available forecast estimated positions for the

disturbance. Only the most recent time period of each storm will have the track drawn.

The default is NO. If both the quad wind flag and the fcst track flag are YES, the wind

and sea quadrant arcs will be plotted for the most recent forecast positions as well as

for the most recent current position.

The name of a tropical storm may be specified. If a name is declared, then only that

storm will be displayed for the date/time indicated. The default is all storms are plotted.

ALL will also plot all storms within the given time range.

3.79 IDNTYP

IDNTYP sets character or numeric station identifiers to be used for input or output. The

valid values are STID and STNM. STID specifies station character identifiers; STNM

specifies station numbers.

If the value in IDNTYP is not STNM, the default of STID will be used.

For example, to update station headers in SFSTNS:

```
IDNTYP = STID -- compares character ids in file with those
in the station table
```
```
IDNTYP = STNM -- compares numeric ids in file with those
in the station table
```

3.80 IJSKIP

IJSKIP is used to control subsetting of the internal grid by declaring bounding index

values and numbers of points to skip in each index direction, I and J. Input is entered

as follows:

Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
or
Yes
or
No

where Iskp and Jskp are the number of grid points to skip in the I and J grid index

directions, respectively; Istrt and Istp are the beginning and ending I index values,

respectively; and Jstrt and Jstp are the beginning and ending J index values,

respectively. GEMPAK grids have (I,J) = (1,1) at the lower left corner, with I

increasing in the X (horizontal or east-west) direction. J increases in the Y (vertical or

north-south) direction. The internal grid navigation is determined from the GDFILE

entry.

The defaults for Iskp and Jskp are zero. The other parameters default to missing values

so that they will be determined automatically, depending on the area of the grid required

by the map display.

If only Iskp is given, then Jskp = Iskp, and the bounds are determined automatically.

The skip values thin the grid by skipping over the number of points specified.

If IJSKIP = NO, then the results are the same as for IJSKIP set to blank or zero. If

IJSKIP = YES, then all the skipping values are computed automatically. When IJSKIP

= YES, skipping is only done if the number of grid points required to cover the display

area is too large. The automatic number of points skipped is the same in each index

direction, and it will be the minimum number required to make the internal

computational grid fit in allocated memory.

IJSKIP and SKIP are functionally independent. If SKIP is also specified, IJSKIP

operates first to create the subset grid, then the SKIP information is applied to that

subset grid, which has already been thinned by IJSKIP. Since IJSKIP and SKIP are

done in series, the combined effect is multiplicative. IJSKIP determines the internal

computational grid; therefore, changing it changes the results of computations

involving derivatives. The SKIP parameter is applied to the internal grid after compu-

tations and affects only the display. SKIP and IJSKIP results, when done separately

and compared, will not always yeild coinci- dent points for the same skipping values,

because IJSKIP creates a computational halo of grid points around the display region

resulting in different initial indexes for IJSKIP vis-a-vis SKIP.


In programs like GDPLOT2, which allows different specifications of IJSKIP for

different GDPFUN entries, IJSKIP must be the same for all fields if the // grid naming

feature is used in GDPFUN. This is because IJSKIP determines the navigation of the

internal grid, which must remain the same if saved grids are to be kept for reuse.

If user-supplied index bounds are not consistent with the given number to skip, then the

bounds will be reset, taking into account any nearby grid boundaries. The reset values

will be close to what the user intended. For best results, provide consistent bounding

index values. If the full range of the index is required, then the unit (1) value of the

index is the starting point.

If the grid is a global grid and the "seam" falls within the display area, then if Istrt is

given, Istp must also be given; otherwise, an error message is generated. If both Istrt

and Istp are given in the case of the global grid, the grid "seam" will not be shifted out

of the display area.

If a start value (strt) exceeds a stop value, an error message appears.

Some examples follow:

Example Action

IJSKIP = YES Skip only if necessary, skipping as few points
as possible; if the subset area is reduced fewer
points will be skipped

IJSKIP = 5 Skip 5 points in both index directions; beginning and ending indexes are

automatically determined

IJSKIP = 5/3 Skip 5 points in the I index direction; skip 3
points in the J index direction; beginning and
ending indexes are automatically determined

IJSKIP = 5;3/2 Skip 5 points in the I index direction; skip 2
points in the J index direction; beginning I
index is 3; all other indexes are determined
automatically

3.81 IMCBAR

IMCBAR specifies the characteristics of a color bar for images. The attributes are

separated by slashes:

```
color / orientation / anchor / x;y / length;width / frequency
```

Color is the color of the labels and the bounding box around the color bar. If color is

negative, the bounding box will not be drawn, and labels will be drawn in colors

corresponding to the color bar. If the color is 0 or missing, no color bar is drawn.

Orientation specifies a vertical or horizontal orientation of the color bar where 'V' is a

vertical bar and 'H' is a horizontal bar. The default is 'V'.

Anchor describes the location on the color bar corresponding to the location given in

the next parameter. Valid inputs are LL, LC, LR, CL, CC, CR, UL, UC, and UR for

lower-left, lower-center, lower-right, center-left, centered, center-right, upper-left,

upper-center, and upper-right, respectively. For example, an 'LL' anchor point, with a

x;y of .1,.1, will place the lower-left corner of the color bar at view coordinates .1, .1.

The default anchor point is 'LL'.

x;y is the position for the anchor point of the color bar in view coordinates. The default

is .005, .05.

Length;width are the length and width of the color bar, normalized to the view

coordinates. The defaults are .5 for the length, and .01 for the width.

Frequency describes the levels to be labeled. If the frequency is positive, the labels are

plotted on the right/top of the color bar. If it is negative, the labels are plotted on the

left/bottom of the color bar. The default is -1.

Examples:

```
IMCBAR = 1 text and bounding box in color 1;
defaults for the rest of the input;
```
```
IMCBAR = 5/V/ /.25;.1/1 text and bounding box in color 5;
color bar plotted vertically;
length and width .25 and .1 of the
view window;
all intervals labeled along left
side of the color bar;
color bar anchor point at lower-left;
```
```
IMCBAR = 1//CL/.1;.5/.75;.1 text and bounding box in color 1;
length and width .75 and .1 of the
view window;
center-left of the color bar
positioned at .1;.5 in view
coordinates;
```

3.82 IMJM

IMJM sets the number of grid points for workstation eta in the N-S and E-W direction

where,

### IMJM = IM;JM

IM and JM are the number of grid points over the computational domain domain where:

```
IM is the number of grid points in the E-W (roughly) direction
JM is the number of grid points in the N-S (roughly) direction
```
IMPORTANT: The horizontal grid dimensions MUST be an ODD integer!

BINARY DISTRIBUTION USERS: The IM and JM values must be set to the

dimensions of your compiled distribution. LM should be 45.

For a grid with 71 grid points in the E-W and 121 points in the N-S direction:

### IMJM = 71;121

Note that during creating on the Arakawa-E grid the IM dimension becomes IM*2-1.

This is the value that is used in etamap for display of the workstation Eta computational

domain.

3.83 INDXFL

INDXFL is the name of the file which contains the GRIB message header information.

If this parameter is left blank, the header information will be read from the GRIB data

file specified in GBFILE.

3.84 INFO

The location of the object is defined in LOCI.


```
TEXT size/font/width/border/rotation/justify/text_string
POLYGON line type
REGPOLY line type/radius/number points/rotation
ARC line type/radius/number points/start ang/end ang
CURVE line type/curve type
LINE line type
ARROW line typ/arrw typ/arrw siz/head siz/
head ang mult/rotation
COLDFRONT number pips/pip size/line type/# smoothing passes
WARMFRONT number pips/pip size/line type/# smoothing passes
OCCLFRONT number pips/pip size/line type/# smoothing passes
STATFRONT number pips/pip size/line type/# smoothing passes
WEATHER symbol code/size
CLOUD symbol code/size
SKY symbol code/size
TURB symbol code/size
ICNG symbol code/size
SPCL symbol code/size
MARKER symbol code/size
PRESTEND symbol code/size
PASTWTHR symbol code/size
```
Special notes:

- For fronts, a positive value for the number of pips will
    plot on one side of the curve, and negative values will
    plot on the opposite side.
- The symbol codes can be found in the users' manual.

3.85 INTERP

INTERP is a logical variable which determines whether interpolation between sweeps

will occur. If INTERP=NO, only values within the beam scan volume will be

displayed. If INTERP=yes, data values between scan tilts will be interpolated using

bilinear interpolation.

3.86 ISIG

ISIG is the ending valid time for the international SIGMET, the colors for the

thunderstorms, turbulence, hurricanes, tropical storms, tropical depressions, volcanic

ash clouds, marked mountain waves, tropical cyclones, squall lines, CAT, icing, hail,

duststorms, sandstorms, cumulonimbus, and low level wind shear, flags for plotting

symbols or storm names, the start and end times, the message id, the direction and

speed, and the flight level or the central pressure and maximum wind speed associated

with a tropical cyclone on the map.

```
End time|TS;TB;HU;TR;TD;VA;MW;TC;SQ;CT;IC;GR;DS;SS;CB;WS clrs|Symbol flg|
```

```
Time flg|Id flg|Motion flg|Flight lvl flg
```
SIGMETs that are valid at the ending time will be plotted on the map. The ending time

is given as a GEMPAK date/time string. Any missing items from the string will be

filled in by the system time. The ending time may also be LAST or ALL. LAST will

use the system time and plot all current SIGMETs. ALL will plot all the SIGMETs in

the data files for the last ten days, whether they are active, cancelled, or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are cyan

for thunderstorm, yellow for turbulence, red for hurricane, red for tropical cyclone,

yellow for tropical storm, green for tropical depression, orange for volcanic ash cloud,

magenta for marked mountain wave, magenta for squall line, yellow for CAT, green for

icing, red for hail, orange for duststorm, orange for sandstorm, and red for

cumulonimbus.

The symbol flag is YES or NO and controls
(1) for thunderstorms and turbulence: whether or not to plot the
symbol at the center of the defined area.
(2) for volcanic ash clouds: whether or not to plot the symbol
at the volcano location, if given, or at the center of the
defined area.
(3) for named storms: whether or not to display the storm name.
The default is NO.

The time flag is YES or NO and controls whether or not to plot the start and end times

of the SIGMET on the map. The default is NO.

The message id flag is YES or NO and controls whether or not to plot the SIGMET

message identification and sequence number on the map. The default is NO.

The motion flag is YES or NO and controls whether or not to plot the direction in

degrees and the speed in knots of the SIGMET. The default is NO.

The flight level flag is YES or NO and controls whether or not to plot the SIGMET

flight level in hundreds of feet on the map. Or if a tropical cyclone is the phenomenon,

this position controls whether or not to plot the central pressure in mb and the maximum

winds in kts. The default is NO.

3.87 KEYCOL

KEYCOL indicates which contour lines to process.

```
KEYCOL = 0 - process all lines
KEYCOL = n - process all lines whose color is n
```

3.88 KXKY

KXKY specifies the size of a grid as two numbers separated by a semicolon:

```
kx ; ky
```
These numbers are KX and KY, which represent the number of grid points in the x- and

y- directions.

If the projection is CED, these numbers may be DELTAX and DELTAY, the grid

spacing in degrees in the x and y directions. If the spacing is input, the first character

in KXKY must be #. For example:

### KXKY = #2.5;2.0

will create a grid with 2.5 degree spacing in x (longitude) and 2.0 degree spacing in y

(latitude).

3.89 LAT

LAT specifies the latitude grid lines to be drawn. It is set to 10 degrees.

3.90 LATLON

LATLON specifies the latitude and longitude grid lines to be drawn. The line color,

line type, line width, label frequency and increment are separated by slashes:

```
color/line_type/line_width/x-freq;y-freq/x-inc;y-inc/lt_label;ln_label/format
```
The label frequency specification contains the latitude and longitude label frequencies

separated by a semicolon. The increment specification contains the latitude and

longitude increments in degrees separated by a semicolon.

If the color is 0 or LATLON is blank, grid lines are not drawn. The latitude and

longitude increments will default to 10 degrees. The label frequency defaults to 1.

The latitude and longitude labels refer to the latitude at which to label the longitudes

lines and the longitude at which to label the latitude lines. The defaults for the latitude

and longitude value placements if either or both are not specified are usually the left

side for latitude labeling and the bottom for longitude labeling.

The format value specifies the current format of the labels. If format is equal to 1, the

latitude and longitude labels have a '-' prepended to the southern and western number


labels. If the format is equal to 2, 'N', 'S', 'E' and 'W' are appended to the latitude and

longitude labels. The default is that the '-' will be prepended to the southern latitudes

and western longitudes.

Examples:

### LATLON = 1

Lat/lon grid lines are drawn every 10 degrees in color 1 using line type 1 and line width

1.

### LATLON = 4/8/3/2/5;5

Lat/lon grid lines are drawn every 5 degrees in color 4 using line type 8 and line width

3. Every other line is labelled.

### LATLON = 24/12/1/2;2/4;4/48.;-100./2

Lat/lon grid lines are drawn every 4 degrees in color 24 using line type 12 and line

width 1. Every other line is labelled. The latitude labels appear on 100.0 W longitude

and the longitude labels appear on 48 N latitude appended to the numerical labels.

3.91 LEVELS

LEVELS specifies the vertical levels to be extracted from the data set. The coordinate

system for the levels is specified in the variable VCOORD as PRES, HGHT or THTA.

LEVELS may be a list separated by semicolons. The following items may be included

in the list:

```
a single level;
MAN for the mandatory levels below 100 mb;
VAS for the standard VAS levels;
a range of levels with an increment separated by -.
```
The following items are also valid, provided they are not part of a list:

```
ALL for all levels;
a range of levels without an increment.
```
SFC or 0 may be entered for surface data. TOP or -1 is the top level at the station.

These values may not be entered for a range with an increment.

The flag /MAN may be used after any specification of LEVELS so that any application

will use only mandatory level data with no interpolation.


3.92 LINE

LINE is the color, line type, line width, line label freqency and smoothing separated by

slashes. The individual values in each group are separated by semicolons:

```
colr1;..;colrn/type1;..;typen/width1;..;widthn/labl1;..;labln/smth/fltr
```
For example:

### LINE = 1;2 / 7;8 / 4;5 / 2 / 2

assigns colors 1 and 2 to alternate lines, dashing patterns 7 and 8 to alternate lines, line

widths 4 and 5 to alternate lines, and labels every other line. Smoothing level 2 is used

on all lines.

If any specification is missing, a default of 1 is used. Except for the smoothing, which

defaults to 0, or no smoothing. If a color number of 0 is given, no plotting will be done.

Information on color selection can be found in the help for COLORS.

There are ten distinct line types:

```
1 - solid
2 - short dashed
3 - medium dashed
4 - long dash short dash
5 - long dash
6 - long dash three short dashes
7 - long dash dot
8 - long dash three dots
9 - medium dash dot
10 - dotted
```
These patterns can be expanded or compressed by prefixing the single digit with a

number from 1 to 9. A prefix of 1 compresses the pattern, 2 is the default and prefixes

3 -- 9 expand the basic pattern. For example, 32 expands line type 2 while 12

compresses the same pattern.

If the line type is set to a single negative number, negative contour values will use the

absolute value of the line type, and positive values will be solid.

If the label information is a single number, n, then every nth line will be labelled. If the

label information is a single number, -n, then every nth line will be labelled, however,

there will not be a break in the line for the label. If a label is a sequence of numbers

separated by semicolons, lines corresponding to values other than 0 will be labelled.

The pattern established will be repeated to accommodate all lines.


The smoothing level is an integer which specifies the amount of smoothing to apply

using a parametric curve function. The current values are 0, 1 and 2 which correspond

to no smoothing and two levels of increasing smoothing.

The filter is a real value that specifies the amount of filtering of the points on a line.

Smaller values allow for more points, while larger values reduce the number of points

on the line. Valid values are in the range of 0.0 to 1.0. The default is 0.0 which means

that no reduction in the number of points will occur.

3.93 LOCI

```
TEXT x;y
POLYGON x1;y1/x2;y2/x3;y3/.../xN;yN
REGPOLY x;y
ARC x;y
CURVE x1;y1/x2;y2/x3;y3/.../xN;yN
LINE x1;y1/x2;y2/x3;y3/.../xN;yN
ARROW x;y
COLDFRONT x1;y1/x2;y2/x3;y3/.../xN;yN
WARMFRONT x1;y1/x2;y2/x3;y3/.../xN;yN
OCCLFRONT x1;y1/x2;y2/x3;y3/.../xN;yN
STATFRONT x1;y1/x2;y2/x3;y3/.../xN;yN
WEATHER x;y
CLOUD x;y
SKY x;y
TURB x;y
ICNG x;y
SPCL x;y
MARKER x;y
PRESTEND x;y
PASTWTHR x;y
```
If LOCI is preceded by a '#' then the x and y values should be latitudes and longitudes.

If the first character is a '@' then all points will be grid point columns and rows.

3.94 LON

LON specifies the longitude grid lines to be drawn. It is set to 10 degrees.

3.95 LSTALL

LSTALL is a flag indicating whether the full contents of a file are to be listed. For

example, in GDINFO, if LSTALL=YES, the grid identifiers will be listed; if

LSTALL=NO, grid identifiers will not be listed.


3.96 LTNG

LTNG is the ending valid time for lightning data, the time increments (in minutes going

back from the ending time) and the corresponding colors for the lightning data, and the

positive and negative markers to display.

```
End time|time increments/colors|positive marker/negative marker
```
The ending time is the latest time for which lightning data will be plotted. The ending

time is given as a GEMPAK date/time string. Any missing items from the string will

be filled in by the system time. The ending time may also be LAST. LAST will use

the system time and plot all lightning data for the preceding times as defined by the time

increments. ALL is not accepted as an ending time for lightning data.

The time increments and colors may each be specified using either a list separated by

semicolons or a range in the form first-last-increment. Time increments are given in

minutes going back sequentially from the ending time. The number of time increments

must be the same as the number of colors. If no time increment and color are given, a

single time increment of 5 minutes with a default color of red will be used. If the range

format is used for the time, an increment must be specified. If the range format is used

for the color and an increment is not specified, an increment of one is used.

The marker data includes the marker type, size and width, separated by semicolons.

The default marker for positive strikes is a plus sign, and for negative strikes a minus

sign. Default marker size multiplier is one, as is default width.

Examples:

### LTNG = 991130/2315|1/5

All lightning data will be plotted in yellow for times 2314Z and 2315Z. Default marker

values will be used.

### LTNG = LAST

All lightning data for the preceding five minutes will be plotted in red.

```
LTNG = ltng=991201/0000|10;20;30;90/2;17;5;3|4;1.5;2/6;2;3
```
Lightning data for 991130/2350 to 991201/0000 will be plotted in red; for 2330 to 2349

in orange; for 2300 to 2329 in yellow; and for 2130 to 2259 in green. Positive strikes

will be plotted as squares with a size multiplier of 1.5 and a line width of 2; negative

markers will be plotted as diamonds with a size multiplier of 2 and a line width of 3.


3.97 LUTFIL

LUTFIL specifies a lookup table file name used to enhance the colors for satellite or

radar images. Color enhancement allows certain features in a satellite or radar image

to be highlighted.

Keywords may be specified for LUTFIL with the following definitions:

```
LUTFIL = DEFAULT (Use the color enhancement specified for this
image type in the table IMGTYP.TBL)
LUTFIL = (Same as DEFAULT)
LUTFIL = GRAY (Image gray values, no enhancement)
LUTFIL = GREY (Image gray values, no enhancement)
LUTFIL = RADAR (Standard radar colors for reflectivity)
LUTFIL = NONE (Use the current look up table with no changes)
```
3.98 MAP

MAP is the map color, line type and line width separated by slashes:

```
map color / line type / line width / filter flag
```
If the color is 0, the map is not drawn. If the map color, line type, or line width is blank,

a default of 1 is used.

Additional help for colors is available in COLORS; help on line types can be found in

LINE.

The filter flag is used to reduce the number of points used to draw the map lines. Valid

values for the filter flag are YES and NO. The default is NO.

The attributes for multiple maps may be set by separating the settings for each map with

a plus sign (+). If the number of attribute groups is less than the number of map files,

the last group of settings is used for the remaining maps.

Examples:

### MAP = 1//2 + 2//3

```
$MAPFIL = hicnus.nws + hipowo.cia
```
### MAP = 17

```
$MAPFIL = hipowo.cia + lorvwo.cia + lakes.cia
```

3.99 $MAPFIL

$MAPFIL is the name(s) of the map file(s) to be used for maps drawn by GEMPAK

programs. If no directory is specified, the GEMPLT map files in GEMMAPS will be

used.

Multiple maps may be drawn at one time by separating the names with a plus sign (+).

The map files in GEMMAPS are named by concatenating the resolution, map

boundaries, and area with the three-letter source file type. For example, the medium-

resolution political world map from GSFC is called MEPOWO.GSF.

### RESOLUTIONFEATURESAREASOURCE

```
ToP POlitical WOrld GSFc
HIgh COastline NW quadrant NWS
MEdium REgional NE quadrant CIA
LOw CouNty SE quadrant USGeol survey
MiXed CountrY SW quadrant NCeP
RiVers North Hemisphere RFC
InterState highways South Hemisphere CPC
U.s. Highways US
State Highways Us Oceans
RoaDs North America
River Forecast Cntrs Atlantic North
County Watch Areas Atlantic1
ZoNes Atlantic2
High Seas Atlantic3
OffShore Pacific North
Forecast Bounds Pacific East
AViation Pacific Surface
Flight Info region Pacific1
Convective Sigmet Reg Pacific2
Tpc High seas Pacific3
Tpc Sigmets MarylanD
Tpc surface Bounds Basic Weather
Tpc surface Analysis Medium Range
Tpc aViation bounds Qpf Verification
Continental Divide Western Region
Mar pred ctr Offshore Arkansas-Red Basin
River fcst ctr Basins Lower Mississippi
Climate Zones Middle Atlantic
Palmer climate Zones Missouri Basin
STates North Central
LaKes NorthEast
OHio
SouthEast
West Gulf
AlasKa
```
The default map is $GEMMAPS/HIPOWO.CIA.


The following map files are currently available:


### TPSTUS.NWS HISTUS.NWS MESTUS.NWS LOSTUS.NWS

### TPCNUS.NWS HICNUS.NWS MECNUS.NWS LOCNUS.NWS

### TPCWA.NWS HICWA.NWS MECWA.NWS LOCWA.NWS

### TPZNUS.NWS HIZNUS.NWS MEZNUS.NWS LOZNUS.NWS

### TPCSUS.NWS HIPOWO.GSF MEPOWO.GSFLOPOWO.GSF

### TPCDUS.USG HICDUS.USG MECDUS.USGLOCDUS.USG

### HIRVUS.USG MERVUS.USG LORVUS.USG

### HIISUS.NWSLOISUS.NWS

### HIUHUS.NWSLOUHUS.NWS

### HISHUS.NWSLOSHUS.NWS

### HIRDUS.NWSLORDUS.NWS

### HIOSUO.NWSLOOSUO.NWS

### HIPOWO.CIA

### HICOWO.CIA

### HICYWO.CIA

### HIAVUS.NWS

### HIPONE.CIA

### HIPONW.CIA

### HIPOSE.CIA

### HIPOSW.CIA

### HICNMD.GSF

### HIFIWO.NCP

### HIFIWO.NCP

### BWX1224.NCP

### BWX3648.NCP

### ISLANDS.CIA

### LAKES.CIA

### RFC.NWS

### TECTONIC.CIA

### TPCZUS.CPC

### TPPZUS.CPC

### TPFBWR.NCP

### TPTANA.NCP

### TPTBNA.NCP

### TPTHNA.NCP

### TPTSNA.NCP

### TPTVNA.NCP

### TPRBAB.RFC

### TPRBLM.RFC

### TPRBMA.RFC

### TPRBMB.RFC

### TPRBNC.RFC

### TPRBNE.RFC

### TPRBOH.RFC

### TPRBSE.RFC

### TPRBWG.RFC

### MEFBNA.NCP

### TPHSUO.NWS HIHSUO.NWS MEHSUO.NWS LOHSUO.NWS

### MEHSUO.NCP

### MEREUO.NCP

### MEFBAN.NCP

### MEFBPE.NCP

### MEFBPN.NCP


### MEFBBW.NCP

### MEFBMR.NCP

### MEFBPS.NCP

### MEFBQV.NCP

### MEFBA1.NCP

### MEFBA2.NCP

### T PFBA3.NCP HIFBA3.NCP MEFBA3.NCP LOFBA3.NCP

### MEFBP1.NCP

### MEFBP2.NCP

### TPFBP3.NCP HIFBP3.NCP MEFBP3.NCP LOFBP3.NCP

### TPMOUO.NWS HIMOUO.NWS MEMOUO.NWS LOMOUO.NWS

### LOCONH.GSF

### LOCOSH.GSF

### LOCOWO.GSF

### LORVWO.CIA

### MXPOWO.NCP

### TPLKUS.NWS HILKUS.NWS MELKUS.NWS LOLKUS.NWS

### HIFIAK.NCP

### HIZNAK.NCP

The maps from GSFC were obtained for GEMPAK Version 1.0. The history of these

maps is unknown. The maps from the University of Wisconsin were digitized there.

The CIA map files were created from a subset of the CIA map database. The United

States Geological Survey maps were converted from the USGS databases. The NWS

maps were obtained from the National Weather Service/Modernization Systems

Implementation Group. The NCEP map files were developed at the National Centers

for Environmental Prediction. The RFC map files were provided by the Arkansas-Red

Basin River Forecast Center.

There are 5 maps designed to be used for the seamless surface analysis which shows

the area of responsibility for the Tropical Prediction Center, Marine Prediction Center

and the Hydrometeorological Prediction Center. Also, these maps have the numbers

050 and 215 in their names which refer to the amount of time after the analysis time;

50 minutes past and 2 hours and 15 minutes past the analysis time.

The capability to use map alias names is available. This makes it easier to use the map

files without previously knowing the long file name. The following is a list of alias

definitions.

```
bwx1224 Basic Weather 12 & 24hr bwx1224.ncp
bwx3648 Basic Weather 36 & 48hr bwx3648.ncp
bwxgif Basic Weather GIF mefbbw.ncp
medrange Medium Range Weather mefbmr.ncp
cwa County Warning Areas hicwa.nws
rfc River Forecast Centers rfc.nws
abrfc AR-Red Basin RFC Basins tprbab.rfc
lmrfc Lower MS RFC Basins tprblm.rfc
marfc Mid Atlantic RFC Basins tprbma.rfc
mbrfc MO Basin RFC Basins tprbmb.rfc
ncrfc North Central RFC Basins tprbnc.rfc
nerfc Northeast RFC Basins tprbne.rfc
```

```
ohrfc Ohio RFC Basins tprboh.rfc
serfc Southeast RFC Basins tprbse.rfc
wgrfc West Gulf RFC Basins tprbwg.rfc
states US states, NWS histus.nws
county US counties, NWS hicnus.nws
zones US zones, NWS hiznus.nws
climate US CPC Climate Zones tpczus.cpc
palmer US Palmer Climate Zones tppzus.cpc
coast World Coasts hicowo.cia
country Hi-res World Countries hicywo.cia
inter US Interstates loisus.nws
base Poltcal Bndry,World,CIA hipowo.cia
nmap Poltcal Bndry,World,CIA hipowo.cia
nsat Poltcal Bndry,World,CIA hipowo.cia
roads US Roads lordus.nws
rivers Rivers lorvwo.cia
hirivers Hi-res US Rivers hirvus.usg
state State Highways loshus.nws
ushwy US Highways louhus.nws
islands Islands islands.cia
lakes Lakes lakes.cia
nafos Mix-res Pol Bndry,World mxpowo.ncp
hiseas High Seas Forecast Areas mehsuo.ncp
rseas Regional Sea Fcst Areas mereuo.ncp
offshore Offshore Forecast Areas hiosuo.nws
offsh2 Offshore Forecast OPC himouo.nws
sigwx Significant AVN Areas sigwx.ncp
nam North America Fcst Bnds mefbna.ncp
epacprod East Pacific Products mefbpe.ncp
npacprod North Pacific Products mefbpn.ncp
natlprod North Atlantic Products mefban.ncp
pacsfcpd Pacific Surface Products mefbps.ncp
pacpart1 Pacific SFC Anal Part 1 mefbp1.ncp
pacpart2 Pacific SFC Anal Part 2 mefbp2.ncp
atlpart1 Atlantic SFC Anal Part 1 mefba1.ncp
atlpart2 Atlantic SFC Anal Part 2 mefba2.ncp
qpfver QPF Verification mefbqv.ncp
fir Flight Information Regn hifiwo.ncp
convsigmet Convective Sigmet Bounds tpcsus.ncp
wregion Western Region Boundary tpfbwr.ncp
tpcsfc TPC Surface Analysis tptana.ncp
tpcsfcbnd TPC Surface Boundary tptbna.ncp
tpchsf TPC High Seas tpthna.ncp
tpcsig TPC SigMets tptsna.ncp
tpcavn TPC Aviation tptvna.ncp
dcvaac DC VAAC Boundary dcvaac.ncp
hpcsfc HPC SFC Anl Boundary hpcsfc.ncp
```
The alias names are specified in the table: $GEMPAK/tables/config/mapfil.tbl.


3.100 MARKER

MARKER specifies the marker color, type, size, line width and hardware/software flag

separated by slashes:

```
marker color / marker type / size / width / hw flag
```
If the marker color is 0, no markers will be drawn. If the marker color is not specified,

a default of 1 will be used.

The marker type specifies the shape of the marker to be drawn. If the type is

unspecified or zero, the current marker type (usually 1) will be used. The software

marker types are:

```
1 plus sign 13 hourglass X
2 octagon 14 star
3 triangle 15 dot
4 box 16 large X
5 small X 17 filled octagon
6 diamond 18 filled triangle
7 up arrow 19 filled box
8 X with top bar 20 filled diamond
9 Z with bar 21 filled star
10 Y 22 minus sign
11 box with diagonals 23 tropical storm
12 asterisk 24 hurricane
```
Hardware marker types may differ.

The marker size is a real number multiplier for the default marker size. If the size is

zero or unspecified, the current size will be used.

The hardware/software marker flag must be HW or SW. Otherwise, the current value

is assumed.

3.101 MAXGRD

MAXGRD is the maximum number of grids that can be stored in the grid file being

created.

3.102 MAXTOP

MAXTOP specifies the color and filter attributes for cell top annotations (feet):

```
color ; filter
```

A color value of 0 is used to turn off cell top annotations.

FILTER is a logical variable or real number which controls the filtering of data in order

to eliminate plotting of overlapping data.

3.103 MESO

MESO specifies the plot symbol and filter attributes for mesocyclones:

```
color / marker / size / width / hw ; filter
```
A color value of 0 is used to turn off mesocyclone plotting.

Marker attributes behave as MARKER for other plotting programs.

FILTER is a logical variable or real number which controls the filtering of data in order

to control plotting of overlapping data.

3.104 MIXRLN

MIXRLN specifies the color, line type, line width, minimum, maximum, and increment

for the background mixing ratio lines on thermodynamic diagrams:

```
line color / line type / width / minimun / maximum / increment
```
The values should be separated by slashes. If the color is 0, or MIXRLN is blank, no

lines will be drawn.

3.105 MRGDAT

MRGDAT is a logical variable indicating whether sounding data is to be merged or

unmerged. The type of unmerged data follows a slash:

```
merge flag / unmerged data type
```
The valid unmerged data types are:


```
1 = mandatory data below 100 mb ( TTAA, PPAA, TRPA,
MXWA )
2 = mandatory and significant level data below 100 mb
( TTAA, PPAA, TRPA, MXWA, TTBB, PPBB )
3 = mandatory and significant level data below and
above 100 mb ( TTAA, PPAA, TRPA, MXWA, TTBB, PPBB,
TTCC, PPCC, TRPC, MXWC, TTDD, PPDD )
```
The default is 3. If the data is merged, the data type is ignored.

When MRGDAT is used to create a sounding file, it indicates whether the file is to

contain merged or unmerged data. Unmerged datasets will store the data as separate

parts. Merged data sets contain values for each parameter at each level.

If MRGDAT is used in a program to list data, the data will be merged if MRGDAT =

YES. If MRGDAT = NO, the separate parts requested by the type will be listed.

3.106 MXDPTH

MXDPTH is the user estimated mixed layer depth in meters. The default value is 0 m,

this uses surface values as the average for the mixed layer.

3.107 NCON

NCON is the ending valid time for the non-convective sigmet, the colors for the icing,

turbulence, duststorm and sandstorm, and volcanic ash, and flags for plotting symbols,

the end time, the message id, and the flight levels on the map.

```
End time|IC;TB;DU;VA clrs|Symbol flg|Time flg|Id flg|Flight lvl flg|
Id1 flg|Id2 flg| ... ... |Id10 flg|
```
Non-convective sigmets that are valid at the ending time will be plotted on the map.

The ending time is given as a GEMPAK date/time string. Any missing items from the

string will be filled in by the system time. The ending time may also be LAST or ALL.

LAST will use the system time and plot all current non-convective sigmets. ALL will

plot all the non- convective sigmets in the data files for the last ten days, whether they

are active, cancelled, updated or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are green

for icing, light blue for turbulence, gold for duststorm and sandstorm and orange for

volcanic ash.


The symbol flag is YES or NO and controls whether or not to plot a symbol for the non-

convective sigmet on the map. The default is NO.

The time flag is YES or NO and controls whether or not to plot the end time of the non-

convective sigmet on the map. The default is NO.

The message id flag is YES or NO and controls whether or not to plot the message

identification and sequence number on the map. The default is NO.

The flight level flag is YES or NO and controls whether or not to plot the flight level(s)

in hundreds of feet on the map. The default is NO.

The Id1~10 flags are YES or NO and controls whether or not to plot the NCON data in

corresponding names. The defaults are YES. The Id names are:
1 NOVEMBER
2 OSCAR
3 PAPA
4 QUEBEC
5 ROMEO
6 UNIFORM
7 VICTOR
8 WHISKEY
9 XRAY
10 YANKEE

3.108 NDVAL

NDVAL is the data value to be used where NIDS producte report "ND" (none

detected). By default, tis is the RMISSD value of -9999. For certain products, the user

may wish to change this for later computations..

3.109 NPASS

NPASS controls the number of passes for the Barnes objective analysis. Valid values

are in the range 1 - 5. Note that two passes are STRONGLY RECOMMENDED.

3.110 NTRACE

NTRACE is the number of traces to be drawn in SFGRAM.

If NTRACE is less than 5, the user input for TRACEx where x is greater than NTRACE

will be ignored.


The default value for NTRACE is 5.

3.111 OUTFIL

OUTFIL is the name of an output satellite image file.

3.112 OUTPUT

OUTPUT determines the output devices. The valid devices are the terminal (T), a file

(F) or none (N).

Specify the output devices desired by entering the appropriate letters with no

separators.

If output is sent to a file, the file name may be specified after a slash. If no file name is

entered, the output will be written to "program.fil", where "program" is the name of the

program being executed.

If N appears anywhere in the output string other than in the file name, no output will be

generated.

Examples:

```
OUTPUT = f / data.dat -- output will be written
to data.dat
```
```
OUTPUT = -- when no output device is
specified, is sent to the
terminal
```
```
OUTPUT = tf -- output sent to the terminal
and to the file program.fil
where program is the GEMPAK
program being run.
```
3.113 PANEL

PANEL specifies the panel location, panel outline color, line type and width separated

with slashes:

```
panel location / outline color / line type / width / region
```

The panel location determines the location of the view region on the graphics device.

It may be specified using a number or abbreviation as follows:

### NUMBER ABBREVIATION DESCRIPTION

```
0 ALL Entire device
1 UL Upper left quadrant
2 UR Upper right quadrant
3 LL Lower left quadrant
4 LR Lower right quadrant
5 L Left half
6 R Right half
7 T Top half
8 B Bottom half
```
An easy 9-panel capability uses abbreviations M1,...,M9 which will be laid out on the

page as:

### M1 M2 M3

### M4 M5 M6

### M7 M8 M9

Horizontal or vertical panels which divide the screen into thirds or fourths may be

created using the syntax Tij where T is either V for vertical or H for horizontal, i is 3

for thirds or 4 for fourths, and j is the actual panel counting from the top or left.

The view region may also be specified as four numbers separated with semicolons,

giving the lower left and upper right corners in fractions of the graphics display area.

The origin is in the lower left of the display. For example, the lower left quadrant can

be given as:

### 0;0;.5;.5

If the panel location is unspecified, the current location is unchanged.

The panel outline color, line type and line width specify the values used to draw a box

around the specified region. The valid regions are VIEW, PLOT and DEVICE. If the

color is 0 or unspecified, no box is drawn.

3.114 PDSEXT

PDSEXT is a logical flag which only becomes applicable when a PDS extension exists

in the GRIB message.

```
YES or NO
```

If PDSEXT is YES, then a sequence of characters specifying the extension information

will be appended onto the standard GEMPAK parameter name. If PDSEXT is NO,

then the append will not be performed. For instance, if the parameter is HGHT and the

PDS extension is an ensemble extension indicating that this particular height field is an

ensemble mean average, then the PDS extension suffix will be "ENMA". The final

GEMPAK parameter name will be "HGHTENMA" and must be referenced as such in

any GEMPAK program.

3.115 PDSVAL

PDSVAL provides a way to enter explicitly PDS numbers identifying a grid by

parameter, level, vertical coordinate type, and time. None, some, or all of them may be

entered. Default values are determined from the GEMPAK grid label information and

the GEMPAK grid GRIB lookup tables.

The input has the following form:

parameter # %vertical coordinate type # @level # ^time

or

parm name %vertical coordinate name @level # ^time

The parameter number or name, if given, must appear first. It is not preceded by any

marking delimiter. The parameter number will be assigned to PDS byte 9. After the

parameter number, the entries may be given in any order. The parameter may be

entered as a name to be found and matched to a number using the GEMPAK grid GRIB

lookup tables.

The vertical coordinate type number is preceded by % as a marking delimiter. The

number entered will be assigned to byte 10. The vertical coordinate may be entered as

a name to be found and matched to a number using the GEMPAK grid GRIB lookup

tables.

The level information is preceded by @. A single level value or two level values

separated by a colon may be entered following the @ symbol. This information is

assigned to bytes 11 and 12.

The time information is entered following the ^ symbol in the form YYYYMMDD/

HHMMFhhhmm. This is a standard GEMPAK time, except that the year must have

four digits. This information is assigned to bytes 13 through 25, with forecast time units

assumed to be hours. Any accumulation period must come from information imbedded

within the GEMPAK parameter name.


3.116 PIXRES

PIXRES is the input for how many pixels and lines to include in the new image. If

PIXRES is 4, then every 4th pixel and line will be used in the new image.

3.117 PLUS

PLUS specifies the size and width of a plus sign:

```
plus sign size / width
```
The plus sign size is a real number multiplier for the default size. If the size is zero or

unspecified, the current size will be used.

3.118 POSN

POSN is the position number to be used to plot data in GDMAP.

If the position number is 0, the data will be plotted centered at the station.

The position numbers are:

### 7

### 13

### 204

### 56

### 8

3.119 PRECSN

PRECSN is the binary or decimal precision to be preserved in the packing of the

gridded data.

Binary precision is entered as a B/n, where B indicates binary precision and n is an

integer indicating the nearest power of 2 to which to pack the data. Power of 10 scaling

of the data is NOT done when binary precision is specified. The following table relates

values of n to the precision retained.

```
n Binary Precision Description
```

```
-4 Data values are rounded to the nearest 1/16.
-3 Data values are rounded to the nearest 1/8.
-2 Data values are rounded to the nearest 1/4.
-1 Data values are rounded to the nearest 1/2.
0 Data values are rounded to the nearest 1.
1 Data values are rounded to the nearest 2.
2 Data values are rounded to the nearest 4.
3 Data values are rounded to the nearest 8.
4 Data values are rounded to the nearest 16.
```
Decimal precision is entered as D/r, where D indicates decimal precision and r is a

positive floating point number indicating the number of decimal significant digits to

preserve. The fractional part of r is retained in the calculation of the number of packing

bits required and may be used to enhance precision in cases where binary precision

might require too many bits. The necessary power of 10 scaling is determined

automatically.

3.120 PROJ

PROJ is the map projection, projection angles, and margins separated by slashes and an

optional image drop flag separated from the rest by a bar:

```
map proj / ang1;ang2;ang3 / l;b;r;t (margins) | image drop flag
```
For all map projections, the lower left and upper right corners of the graphics area

should be specified in GAREA.

The following simple map projections may be specified:

```
MER Mercator
NPS North Polar Stereographic
SPS South Polar Stereographic
LCC Northern Hemisphere Lambert Conic Conformal
SCC Southern Hemisphere Lambert Conic Conformal
CED Cylindrical Equidistant
MCD Modified Cylindrical Equidistant
NOR North Orthographic
SOR South Orthographic
```
The following full map projections may also be specified:


```
MER (CYL) Mercator
CED (CYL) Cylindrical Equidistant
MCD (CYL) Modified Cylindrical Equidistant
STR (AZM) Polar Stereographic
AED (AZM) Azimuthal Equidistant
ORT (AZM) Orthographic
LEA (AZM) Lambert equal area
GNO (AZM) Gnomonic
LCC (CON) Northern Hemisphere Lambert Conic Conformal
SCC (CON) Southern Hemisphere Lambert Conic Conformal
```
In addition for full map projections, three angles MUST be specified in PROJ. The

angles have the following meanings for the different projection classes:

CYL angle1 -- latitude of origin on the projection cylinder
0 = Equator
angle2 -- longitude of origin on the projection cylinder
0 = Greenwich meridian
angle3 -- angle to skew the projection by rotation of
the cylindrical surface of projection about
the line from the Earth's center passing
through the origin point. This results in
curved latitude and longitude lines.

```
If angle3 is greater than 360 or less than -360
degrees, then the rectangular Cartesian coordinate
system on the projection plane is rotated
+/- |angle3|-360 degrees. This results in
latitude and longitude lines that are skewed
with respect to the edge of the map.
```
```
The difference between |angle3| < 360 and
|angle3| > 360 is that, in the former case,
the rotation is applied to the developable
cylindrical surface before projection and
subsequent development; while, in the latter
case, the rotation is applied to the Cartesian
coordinate system in the plane after development.
Development here refers to the mathematical
flattening of the surface of projection into a
planar surface.
```
```
Exception:
```
MCD angle1 -- scaling factor for latitude
0 = default scaling (1/cos(avglat))
angle2 -- longitude of origin (center longitude) angle3 -- not used

```
AZM angle1 -- latitude of the projection's point of tangency
```

angle2 -- longitude of the projection's point of tangency angle3 -- angle to skew the

projection by rotation about
the line from the Earth's center passing
through the point of tangency

CON angle1 -- standard latitude 1
angle2 -- polon is the central longitude angle3 -- standard latitude 2

The angles for the full map projection types are given as three numbers separated with

semicolons. Note that THREE angles must be entered even if some angles are not used.

Note that transverse projections may be obtained using a cylindrical projection with the

first angle set to either 90 or -90. The second angle is the longitude at which the

cylinder axis intersects the equator. This will be the transformed location of the "south"

pole when the first angle is 90 or the "north" pole when the first angle is -90. For

example, if angle1 = 90 and angle2 = 0, the axis of the cylinder of projection is

perpendicular to the earth's axis and enters the earth at 0N 0E and emerges at 0N 180E.

The great circle formed by 90E and 90W becomes the "equator" on the cylinder. This

cylinder is ideal for a transverse cylindrical projection of locations on the continent of

North America.

Satellite and radar projections are available:

SAT specifies remapped or nonremapped satellite
projections.

```
RAD specifies the radar projection.
```
Currently, the McIDAS area format is supported for satellite and radar images. NIDS

radar images from WSI may also be displayed.

The image drop flag may be specified as a D or an ND, and is valid for SAT and RAD

only. A value of D causes the image to be dropped before any additional graphics are

drawn. Conversely, a value of ND does not drop the image. When the image drop flag

is undefined, the image is dropped when CLEAR is set to YES, and is not dropped when

CLEAR is set to NO.

If the projection is DEF or is blank, a default projection will be obtained from the

geographic table for the specified GAREA. If no default projection has been defined

for the specified GAREA and the projection is DEF, the current map projection will be

used.

In the grid programs the following graph projections are also available:


```
LIN linear x, linear y
LOG linear x, logarithmic y
KAP linear x, y ** KAPPA
POL polar coordinates ( R, THETA )
```
Margins may be input as four numbers separated with semicolons. The four numbers

represent the left, bottom, right, and top margin sizes in character widths. If no margins

are specified, the default will be (0,3,0,0) in map mode and (6,4,4,1) in graph mode. If

the input for the margins is NM, all four margins will be set to 0.

3.121 PTYPE

PTYPE is the type of y axis plot to be used, the height-to-width ratio of the plot, and

the margins, separated by slashes.

The valid inputs for type for the y axis are:

```
LIN linear
LOG logarithmic
STUVE scaled by raising to KAPPA, 2/7, power
KAP same as STUVE
SKEWT logarithmic y-axis; skewed x-axis
SKEW same as SKEWT
```
If the height-to-width ratio is 0, the entire screen will be used. This is also the default

when ratio is not specified.

The margins are specified as: left;bottom;right;top. The values are multiples of the

current character size. If the margin specification is missing or incomplete, a default

appropriate to the program will be used. The default in the profile programs is: 10;3;(6

times wind barb size);5. The right margin allows room for plotting winds and typically

has a value around 20.

3.122 QCNTL

QCNTL is the quality control threshold values. It is used only when the first-guess field

exists. If the difference between the original data and the interpolated values from the

first guess is greater than the threshold values, the data will be discarded. Only positive

threshold values are used. The default value is 0.


3.123 QSCT

QSCT is the ending valid time for the QuikScat wind data, the speed intervals and

colors, the wind barb size and width and plotting flags. Skip is a value that indicates

how many rows and data points to skip when plotting. The flags include High Wind

Speed, Low Wind Speed, Rain, Sensor Availibility.

Data type | End time | Speed intervals | colors1 | colors2 | Arrow/Barb shaft size;Arrow/

Barb width;Arrow head size; Type of wind vector | Skip | Time stamp interval | Time

stamp color | Line Width | High Spd | Low Spd | Rain | Availibility | Rain Colors

The data type is a selection to plot QSCT or QSCT_HI data.

The ending time is the latest time for which QuikScat data will be plotted. The ending

time is given as a GEMPAK date/time string. Any missing items from the string will

be filled in by the system time. The ending time may also be LAST. LAST will use

the system time and plot all QuikScat data for the preceding 4 hours. ALL is not

accepted as an ending time for QuikScat data.

The speed intervals and colors may each be specified using either a list separated by

semicolons or a range in the form first-last-increment. Speed intervals are given in

knots. The number of intervals must be the same as the number of colors. If no interval

and color are given, a single speed interval of 200 knots will be used. This means that

any wind speed less than 200 knots will be plotted. The default color is green. If the

rain flag in the data is true, and the rain colors flag is true, the wind bard is plotted in

the second colors for that wind speed. If the rain flag is false or the rain colors flag is

false, then the first color is used.

The wind arrow/barb attributes are the shaft size of the arrow or barb, width of the

arrow or barb, size of the arrow head, and the type of wind vector separated by a

semicolon. The default shaft size is 0.2, the default width is 1, the default arrow head

size is .4 , and the default type of wind vector is 5 for wind barb. The other values for

type of wind vector is 1 or 3 for directional arrow and 4 for regular arrow. If no value

is given for the type of wind vector, then if the arrow head size is greater than 0,

directional arrows will be plotted, otherwise wind barbs will be plotted.

The Skip value is an integer used for skipping rows and columns of data when plotting

the winds.

The time stamp interval is the time interval in minutes. Line width is the width of the

time stamp. The default value is 30 time stamp interval and 1 for line width.

The flags correspond to flags in the QuikScat data. The High Speed, Low Speed, Rain

present and Sensor Availibility flags allow the user to display wind data that has any of

these attributes. The default for all flags is NO.


3.124 RADDUR

RADDUR is the length of time (in minutes) prior to RADTIM for which data will be

used in composites.

3.125 RADFIL

RADFIL is the name of a radar image file.

For animation programs, a list of radar image files, separated by semicolons, may be

entered. For long image names, part of the first name may be separated by backslashes

( ); the following values in the list will be substituted for the characters within the

backslashes,

i.e.
$RADAR/REF_941017_1200;05;10;15

is interpreted as
$RADAR/REF_941017_1200;$RADAR/REF_941017_1205;$RADAR/
REF_941017_1210; $RADAR/REF_941017_1215

3.126 RADFRQ

RADFRQ is the update frequency for RADAR composites.

RADFRQ is a time, in seconds, for which the program will wait before creating the next

composite.

3.127 RADINFO

RADINFO specifies the color for radar site operational status annotations. NA, data

not available; OM, out for maintenance; NE, no echoes.

3.128 RADIUS

RADIUS is the search radius (in meters) for which data will be considered.


3.129 RADMODE

RADMODE allows the user to select whether to include radar data from sites operating

in (P) precipitation/storm mode, (C) clear air mode, and/or (M) maintainence mode.

The default, if none are specified is data from all 3 modes (PCM). Multiple modes may

be specified.

3.130 RADPARM

RADPARM is the Radar parameter to be displayed. Valid values are dz (reflectivity),

vr (radial velocity), sw (spectrum width).

3.131 RADTIM

RADTIM is the date and time to be used for RADAR composites.

RADTIM may either be an 11-character string entered as YYMMDD/HHMM

following the format of DATTIM, or the word "current" which refers to the current

SYSTEM time.

3.132 RCMFIL

RCMFIL is the RCM data file. Bulletins with radar observation times in the RCM file

will be plotted if the observation time is within the range specified by the starting and

ending times.

RCMFIL may be a file alias defining a file template.

3.133 REFVEC

REFVEC specifies the size and location on the screen of the reference arrow using the

following format:

```
Magnitude; x; y; text size/font/width/HW; string
```
Magnitude refers to the size of the reference wind arrow to be plotted. For example,

using 10.0 results in a reference arrow of 10.0 units being plotted.

If the magnitude specified is 0, no reference arrow is plotted. The default is 0.


The user specifies the location of the reference arrow by entering values between 0.0

and 1.0 for x-location and y- location. These values are normalized to the display so,

for example, the center of the display is 0.5;0.5. If no values are entered, the reference

arrow is plotted in the lower left hand corner: 0.05;0.05.

The text attributes: size, font, width and HW flag; are applied only to the text displayed

with the reference vector. See the TEXT help for further details on these attributes.

The string specifies the label for the reference vector. This text may contain any

keyboard character. If the string is empty, an appropriate label will be generated.

3.134 REGION

REGION specifies an areal location.

In GPBOX, REGION may be VIEW, PLOT or DEVICE where:

```
VIEW corresponds to the view region (includes margins);
```
```
PLOT corresponds to the plot region (excludes margins);
```
```
DEVICE corresponds to the entire device space.
```
In OABOX, the region must be GRID, DATA, or EXTEND where:

```
GRID corresponds to the area over which the objective
analysis is performed;
```
```
DATA corresponds to the area from which data will be
extracted for the objective analysis;
```
```
EXTEND corresponds to the extended grid area over which
the first pass of the objective analysis is performed.
```
3.135 $RESPOND

$RESPOND is a logical variable indicating whether the user will respond interactively

to GEMPAK programs. If set to NO, programs will not wait for input from the user.

3.136 ROTATE

ROTATE is the angle of rotation for the coordinate axes. This is used to determine the

wind component in a user definded plane. Default = 'blank' or 270 means no rotation.


3.137 RTRAJ

RTRAJ is a logical variable which determines whether the trajectory will start or end

at the specified GPOINT.

Enter YES for the trajectory to end at GPOINT (reverse trajectory). Enter NO to begin

at the specified GPOINT.

3.138 SATFIL

SATFIL is the name of a satellite image file.

For animation programs, a list of satellite image files, separated by semicolons, may be

entered. For long image names, part of the first name may be separated by backslashes

( ); the following values in the list will be substituted for the characters within the

backslashes,

i.e.
$GOES7/VIS_941017_1201;13;14

is interpreted as
$GOES7/VIS_941017_1201;$GOES7/VIS_941017_1301;$GOES7/VIS_941017_1401

3.139 SCALE

SCALE is the scaling factor for the data. All data will be multiplied by 10 ** SCALE.

The user specifies the scaling factor for scalar grids and/or vector grids by entering:

```
scalar / vector
```
If SCALE is not specified, an appropriate scaling will be selected. In the grid programs,

a data-dependent scaling will be selected. In the surface, sounding and the grid to

station interpolation programs, a default of 0 will be used. If no vector scale is specified

the scalar scaling factor will be used in its place.

If the absolute value of SCALE is greater than 5 in a surface or sounding program, a

value of 0 will be used. If the absolute value is greater than 20, a default will be

computed in a grid program. In the grid to station interpolation program, a default of 0

will be used.

Note that scaling data may create integer overflow problems in programs, such as

SFMAP, which round data to the nearest integer. Valid integers must be in the range -

2*10**9 to 2*10**9, approximately.


3.140 SEARCH

SEARCH controls the search radius in an objective analysis program. The radius is the

maximum distance that a station may be from a grid point to be used in the analysis for

that point. The search radius is set so that stations whose weighting factor is less than

EXP (-SEARCH) are not used. SEARCH must have a value in the range 1 - 50. If the

value is outside this range, a default value of 20 is used. If SEARCH is given a very

small value, many grid points will not have three stations within the search area, and

consequently will be set to the missing data value.

SEARCH may also be used to set a flag to allow extrapolation of data values to grid

points on the periphery of the data area. To do this, follow the value of SEARCH with

/EX; for example, SEARCH = 20/EX will result in data extrapolation.

3.141 SFEFIL

SFEFIL is the name of the surface edit file to be used to update a surface file.

SFEFIL is a text file which may be created using the program SFLIST with F as an

output device. A text editor may be used to create or change the surface edit file.

The parameters to be edited must be specified at the beginning of the surface edit file.

For example:

### PARM = TMPF;DWPF

Parameters that have character values, such as WTHR, cannot be edited. Instead, the

numeric value must be used, e.g., WSYM in the case of WTHR. All the parameters

which are to be edited must already exist in the surface data file, since no parameter

transformations will be done. Station information, such as latitude, longitude and

elevation, cannot be changed with SFEFIL. Use GEMPAK program, SFSTNS, to

update station information in a surface file.

The data follow the parameter list.

Stations and times which do not already exist in the surface data file will be added.

3.142 SFFILE

SFFILE is the name of the surface data file to be accessed.

The input for SFFILE may also be a file type. The following file types may be input:


```
METAR Surface observations
SYNOP Synoptic observations
ACFT Aircraft observations
SHIP Ship/Buoy observations
SHIP6HR 6-hour ship observations
SCD Supplemental Climatological Data
FFG Flash flood guidance
NGMMOS NGMMOS model
GFSMOS GFSMOS model
GFSXMOS GFSXMOS model
```
When one of these file types is input, the program searches locally for the most recent

file first. If no files are found locally, the program searches remotely for the most recent

file. The remote directories are specified in the table DATATYPE.TBL.

An optional base time may be included with the file type name, following a bar (|). For

example:

```
SFFILE = hrly | base time
```
The base time may be any valid format for a GEMPAK date/time. Any missing parts

of the date/time will be supplied by the system time.

SFFILE may also be entered as a file template for the SFGRAM program. The program

searches for all the files with the specified path and template.

3.143 SFFSRC

SFFSRC is the surface file source. With this parameter, the type of data the can be

contained in the file is specified.

SFFSRC may be set to either decoded data (including UNKN, AIRW, METR, SHIP,

BUOY, and SYNP) or TEXT. If both types are requested, they are separated by a '|'.

The decoded file type MUST be specified first, otherwise a default to a text only file

will be created.

3.144 SFOUTF

SFOUTF is the output surface data file.


SFOUTF is used in programs which create a new surface data file. It is also used in

programs in which data is moved from an input file to an output file, in which case,

SFFILE is the original file.

When used with program NAMSND, SFOUTF can be used to specify two output

surface files. This is done by appending a + to the output file name. In this case, the

second file will have the same name as the first, with the suffix _aux. The second file

will contain surface data for the diagnostic or overflow parameters listed in the

auxiliary surface packing file (described in SFPRMF). For example,

```
SFOUTF = new.sfc+
```
will create output files new.sfc and new.sfc_aux, if both a primary and an auxiliary

surface packing file have been provided.

3.145 SFPARM

SFPARM is a list of surface parameters to be used in a surface program.

The four-character parameter names must be separated by semicolons. For example:

### SFPARM = TMPF;DWPF;WTHR

If a parameter is not present in a data set, the program will attempt to derive it from

those parameters which are present. For example, relative humidity can be computed

from the temperature and dewpoint temperature. Note that STHA, STHE, and SMXR

are computed from PALT.

In plotting programs, the position of the parameter determines where it will appear with

respect to the station location. The first position is centered on the station. The other

positions are shown below:

### 18 14 8 16 22

### 19 2 10 4 23

### 1231513

### 20 6 11 7 24

### 21 15 9 17 25

In the example above, WTHR is plotted in position 3.

Functions can be used to modify/qualify the output parameters. These functions must

follow the parameter name and are applied in the order given; however, arithmetic

operators must precede the other operators. The function symbols must precede the

qualifying value and are defined as follows:


```
* multiply = equal to
/ divide $ direction
+ add < less than or equal to
```
- subtract > greater than or equal to

In the case of weather symbols, the size, line width and other information needed to

draw the symbol may be specified following a colon after the symbol name and after

the function specifications if any exist. The drawing attributes are separated by colons.

For all the symbols, the information is entered as:size:width;for arrows and barbs the

information is entered as :size:width:type:headsiz, where the headsiz applies only to

arrows. The type is a three-digit number, ABC, interpreted digit by digit as follows:

### ABC

```
1 = plot calm wind 1 = start at stn 1 = not filled
2 = ignore calm wind 2 = center at stn 2 = filled
3 = start on sky symbol 3 = not filled with box
4 = filled with box
```
Calm winds are plotted as a circle or an arrowhead for barbs and arrows, respectively.

Arrowheads and 50-unit barb flags are filled if C=2 or 4. A box is plotted with

background color if C = 3 or 4. For example, if TYPE=132, a barb will be plotted as a

circle if the wind is calm; it will begin on the edge of the cloud cover symbol, and 50-

unit flags will be filled. The default is 111 unless sky symbols are being plotted in

which case it is 131.

For all symbols ( weather, cloud, pressure tendency, sky cover, past weather, icing,

combination, etc. ) and wind barbs and arrows, the area immediately surrounding the

symbol may be blanked out. The resulting effect allows the object to be more easily

identified. This capability may be invoked by adding the additional blank-out width

(*100) to the object width. For example, to blank-out an extra 8 width units around the

weather symbols of size 2 and width 3, specify the SFPARM as WSYM:2:803.

A special value of M may be used for the = function to denote a missing value, so that

values will be plotted only when a specific parameter is missing. For example, to plot

an X when the visibility is missing, specify SFPARM=MARK:16;VSBY=M.

Aliases are available to make it easier for the user to plot typical surface data by using

pre-set parameters and colors. To use an alias, enter the alias name for SFPARM and

leave COLORS blank. If the user does not leave COLORS blank, the selected colors

will be used instead of the pre-set alias colors.

See PRMLST.TBL for a list of current aliases for surface data.

The GEMPAK surface parameters and the corresponding four-character abbreviations

are:


### TEMPERATURE

TMPC - Temperature in Celsius
TMPF - Temperature in Fahrenheit
TMPK - Temperature in Kelvin
STHA - Surface potential temperature in Kelvin
STHK - Surface potential temperature in Kelvin
STHC - Surface potential temperature in Celsius
STHE - Surface equivalent potential temperature in Kelvin
STHS - Surface saturation equiv. pot. temp. in Kelvin
TVRC - Virtual temperature in Celsius
TVRF - Virtual temperature in Fahrenheit
TVRK - Virtual temperature in Kelvin
THTV - Virtual potential temperature in Kelvin
TDXC - Maximum 24-hour temperature in Celsius
TDNC - Minimum 24-hour temperature in Celsius
TDXF - Maximum 24-hour temperature in Fahrenheit
TDNF - Minimum 24-hour temperature in Fahrenheit
T6XC - Maximum 6-hour temperature in Celsius
T6NC - Minimum 6-hour temperature in Celsius
T6XF - Maximum 6-hour temperature in Fahrenheit
T6NF - Minimum 6-hour temperature in Fahrenheit
DMAX - Daily weather map maximum temperature in Fahrenheit
DMIN - Daily weather map minimum temperature in Fahrenheit
SSTC - Sea surface temperature in Celsius
SSTF - Sea surface temperature in Fahrenheit
LTMP - Temp. in Celsius of surface air lifted to 500 or !x mb

### MOISTURE

DWPC - Dewpoint in Celsius
DWPF - Dewpoint in Fahrenheit
DWPK - Dewpoint in Kelvin
DPDC - Dewpoint depression Celsius
DPDF - Dewpoint depression Fahrenheit
DPDK - Dewpoint depression Kelvin
SMXR - Surface mixing ratio in g/kg
SMXS - Surface saturated mixing ratio in g/kg
RELH - Relative humidity in percent
TMWK - Wet bulb temperature Kelvin
TMWC - Wet bulb temperature Celsius
TMWF - Wet bulb temperature Fahrenheit
VAPR - Vapor pressure in millibars
VAPS - Saturation vapor pressure in millibars

### PRESSURE AND ALTIMETER

PRES - Station pressure in millibars
PMSL - Mean sea level pressure in millibars
PALT - Pressure in millibars from altimeter
ALTI - Altimeter setting in inches of mercury
ALTM - Altimeter setting converted to millibars
SALT - Abbreviated altimeter in millibars


```
SMSL - Abbreviated mean sea level pressure
SALI - Abbreviated ALTI
RMSL - First 3 digits left of decimal of PMSL * 10
RSLI - First 3 digits left of decimal of ALTI * 100
RSLT - First 3 digits left of decimal of ALTM * 10
PTND - Complete pressure tendency with symbol
PTSY - Pressure tendency symbol only
P03C - Pressure change over 3 hours
P03D - Pressure tendency and change group, appp
P24C - Pressure change over 24 hours
ZMSL - Estimated height at PMSL
Z000 - Estimated height at 1000 mb
Z850 - Estimated height at 850 mb
Z800 - Estimated height at 800 mb
PANY - Returns PMSL, if avaliable, if not, returns ALTM
RANY - Computes the 3 digit display of pressure
SANY - Creates a 3 character string from integral part
of PMSL or ALTM
```
### WIND

```
ARRW - Wind arrow (m/s)
ARRM - Wind arrow (m/s)
ARRK - Wind arrow (knots)
BARB - Wind barb (m/s)
BRBM - Wind barb (m/s)
BRBK - Wind barb (knots)
BRBS - Wind barb (mi/hr)
DARR - Wind arrows of uniform length
UWND - U component of the wind in meters/second
VWND - V component of the wind in meters/second
UKNT - U component of the wind in knots
VKNT - V component of the wind in knots
DRCT - Wind direction in degrees
SPED - Wind speed in meters/second
SKNT - Wind speed in knots
SMPH - Wind speed in miles/hour
PSPD - Packed speed and direction (ddfff) in meters/second
PKNT - Packed speed and direction (ddfff) in knots
GUST - Wind gusts in knots
GUMS - Wind gusts in meters/second
PWDR - Peak wind direction in degrees
PWSP - Peak wind speed in meters/sec
PWHR - Hour of peak wind
PWMN - Minutes of peak wind
```
### CLOUD

For the following parameters, the character x may be replaced by L, M, or H, indicating

low, middle or high clouds. The character T indicates the value of the parameter at the

level of maximum cloud coverage.


```
xCLD - Character cloud coverage
TCLD - Character maximum cloud coverage
xCLO - Fractional cloud coverage
TCLO - Fractional maximum cloud coverage
CLCx - Numeric cloud coverage
CLCT - Numeric maximum cloud cover
CLDS - Combined cloud coverage from three levels
CMBC - Numeric combined cloud coverage from three levels
CLHx - Cloud height in hundreds of feet
CLDx - Combined cloud height and coverage
CLDT - Maximum value of CLDx
COMx - Numeric combined cloud height and coverage
COMT - Maximum value of COMx
CEIL - Ceiling in hundreds of feet
```
For the next two cloud parameters the x can be replaced by 1, 2, or 3 and represents the

cloud report number.

```
CHCx - Numeric combined cloud height and coverage
CHDx - Combined cloud height and short code
```
The following numeric cloud parameters are WMO standard codes.

```
CFRT - Fraction of celestial dome covered by cloud from WMO Code 2700
CFRL - Fraction of celestial dome covered by low or mid clouds from WMO Code
2700
CTYL - Low-level cloud genera from WMO Code 0513
CTYM - Mid-level cloud genera from WMO Code 0515
CTYH - High-level cloud genera from WMO Code 0509
CBAS - Cloud base height from WMO Code 1600
CSYL - Low cloud type symbol
CSYM - Middle cloud type symbol
CSYH - High cloud type symbol
CSYT - Cloud type symbol on first reported level
SKYC - Sky coverage symbol
SKYM - Sky coverage symbol with wind barbs in m/s
SKYK - Sky coverage symbol with wind barbs in knots
```
The following numeric parameter follows the guidelines in the National Weather

Service Operations Manual, Chapter D-31.

```
XVFR - Categorical identification of flight rules
0 = Low Instrument Flight Rules (LIFR)
1 = Instrument Flight Rules (IFR)
2 = Marginal Visual Flight Rules (MVFR)
3 = Visual Flight Rules (VFR)
```
### WEATHER

```
WTHR - Character weather code from any numeric code
WCOD - Character weather code
WNUM - Numeric weather code
```

WSYM - Present weather symbol
WTMO - Character weather code from WMO
WWMO - Numeric WMO weather code
PWTH - Past weather symbol
PWWM - Numeric past weather in WMO code

### STATION VARIABLES

STID - Character station identifier
STNM - 5-digit station identifier
SLAT - Station latitude in degrees
SLON - Station longitude in degrees, West is negative
SELV - Station elevation in meters
SPRI - Station priority

### MODEL OUTPUT STATISTICS

MXMN - Maximum or minimum temperature in Fahrenheit
TNTF - Night temperature fcst in Fahrenheit
TNCF - Night temperature climatology in Fahrenheit
TNAF - Night temperature anomaly in Fahrenheit
TDYF - Day temperature fcst in Fahrenheit
TDCF - Day temperature climatology in Fahrenheit
TDAF - Day temperature anomaly in Fahrenheit
CL12 - Prevailing total sky cover fcst for a 12-hr period
1 = CL = mostly clear
4 = OV = mostly overcast
7 = PC = mixed clouds and clear skies
SK12 - Maximum sustained surface wind speed fcst for a 12-hr period
5 = light 0 - 12 knots
15 = Moderate 13 - 21 knots
25 = Strong 22 - 33 knots
40 = High greater than or equal to 34 knots
PP06 - Probability of precipitation fcst in a 6-hr period
PP12 - Probability of precipitation fcst in a 12-hr period
PP1C - Probability of precipitation climatology in a 12-hr period
PP1A - Probability of precipitation anomaly in a 12-hr period
PP24 - Probability of precipitation fcst in a 24-hr period
PP2C - Probability of precipitation climatology in a 24-hr period
PP2A - Probability of precipitation anomaly in a 24-hr period
QPX2 - Maximum amount of precipitation in inches fcst in a 12-hr period. Values
are same as QP12
QP06 - Quantitative precipitation fcst in a 6-hr period
1 = 0.01 - 0.09 inches
2 = 0.10 - 0.24 inches
3 = 0.25 - 0.49 inches
4 = 0.50 - 0.99 inches
5 = 1.00 - 1.99 inches
QP12 - Quantitative precipitation fcst in a 12-hr period
0 = no precipitation expected
1 through 5 same as QP06
6 = greater than or equal to 2.00 inches
QP24 - Quantitative precipitation fcst in a 24-hr period


0 through 5 same as QP12
6 = 2.00 - 2.99 inches
7 = greater than or equal to 3.00 inches
TS06 - Unconditional probability of thunderstorms occurring in a 6-hr period
TS12 - Unconditional probability of thunderstorms occurring in a 12-hr period
TS24 - Unconditional probability of thunderstorms occurring in a 24-hr period
TC06 - Conditional probability of severe weather occurring in a 6-hr period
TC12 - Conditional probability of severe weather occurring in a 12-hr period
PCPT - Categorical forecast of precipitation
0 = R = rain
1 = S = snow
2 = Z = freezing
POZP - Conditional probability of freezing precipitation (not included during the
warm season)
POSN - Conditional probability of snow (not included during the warm season)
SN06 - Categorical forecast of snow amount falling in a 6-hr period
0 = no snow
1 = trace to less than 2 inches
2 = greater than or equal to 2 inches
SN12 - Categorical forecast of snow amount falling in a 12-hr period
0 and 1 same as SN06
2 = 2 to less than 4 inches
4 = 4 to less than 6 inches
6 = greater than 6 inches
(not included during the warm season)
SN24 - Categorical forecast of snow amount falling in a 24-hr period
0 = no snow or a trace
1 = greater than a trace to less than 2 inches
2 = 2 to less than 4 inches
4 = 4 to less than 6 inches
6 = 6 to less than 8 inches
8 = greater than 8 inches
(not included during the warm season)
PZ12 - Conditional probability of freezing precipitation in a 12-hr period
PS12 - Conditional probability of snow in a 12-hr period
PR12 - Conditional probability of mixed liquid/frozen precipitation in a 12-hr period
PC12 - Categorical forecast of precipitation type in a 12-hr period
0 = R = liquid
1 = S = frozen
2 = Z = freezing
3 = RS = mixed liquid and frozen precipitation
4 = RZ
5 = SZ
6 = RSZ
FCIG - Categorical forecast of ceiling height conditions
1 = less than 200 feet
2 = 200 - 400 feet
3 = 500 - 900 feet
4 = 1000 - 1900 feet
5 = 2000 - 3000 feet
6 = 3100 - 6500 feet
7 = 6600 - 12,000 feet
8 = greater than 12,000 feet or unlimited
FVIS - Categorical forecast of visibility conditions


1 = less than 0.5 miles
2 = 0.5 - 0.875 miles
3 = 1.0 - 2.75 miles
4 = 3.0 - 5.0 miles
5 = greater than 5.0 miles
FVSA - Categorical forecast of visibility conditions (for new MOS)
1 = less than 0.5 miles
2 = 0.5 mile to less than 1.0 mile
3 = 1.0 to less than 2.0 miles
4 = 2.0 to less than 3.0 miles
5 = 3.0 to 5.0 miles
6 = 6.0 miles
7 = greater than 6.0 miles
OVIS - Categorical forecast in plain language of obstructions
to vision
0 = N = none of the following
1 = FG or F = fog or ground fog (vis. less than .625 mile)
2 = HZ or H = haze, smoke, dust
3 = BR = mist (fog with visibility greater than .625 mile)
4 = BL = blowing dust, sand, snow
WXPB - Categorical weather precipitation probability or areal coverage determined
by the precipitation parameter having the highest probability or areal cov-
erage in WNUM.

```
For probability:
1 = slight chance
2 = chance
3 = likely
4 = occasional
5 = definite
For areal coverage:
1 = isolated
2 = widely scattered
3 = scattered
4 = numerous
5 = widespread
```
### TERMINAL AERODROME FORECAST (TAF)

TDRC - Temporary/probability wind direction in degrees
TSKN - Temporary/probability wind speed in knots
TGST - Temporary/probability wind gusts in knots
BRGK - Gust barb feathered in knots
TCHx - Temporary/probability numeric combined cloud height and coverage, as for
CHCx
TCEL - Temporary/probability ceiling in hundreds of feet, as for CEIL
TSKC - Temporary/probability cloud coverage graphics symbol, as for SKYC
TXVF - Temporary/probability categorical identification of flight rules, as for XVFR
TWNM - Temporary/probability numeric weather code, as for WNUM
TWSY - Temporary/probability graphics weather symbol corresponding to TWNM,
as for WSYM
TVSB - Temporary/probability visibility in statute miles
PPRB - Probability for TAF forecast change indicator


30 = PROB30 - 30 percent probability condition
40 = PROB40 - 40 percent probability condition
50 = TEMPO - temporary condition
VWNM - Vicinity numeric weather code, as for WNUM
VWSY - Vicinity graphics weather symbol corresponding to VWNM, as for WSYM
TVWN - Temporary/probability vicinity numeric weather code, as for WNUM
WSKC - Worst case cloud coverage graphics symbol, as for SKYC
WXVF - Worst case categorical identification of flight rules, as for XVFR
TPWN - Temporary/probability/vicinity numeric weather code, as for WNUM
TPWS - Temporary/probability/vicinity graphics weather symbol corresponding to
TPWN, as for WSYM
AWNM - Prevailing/temporary/probability/vicinity numeric weather code, as for
WNUM
AWSY - Prevailing/temporary/probability/vicinity graphics weather symbol corre-
sponding to AWNM, as for WSYM
LLWS - Low level wind shear forecast flag
MOTV - Mountain obscuration threshold value in hundreds of feet
CMSL - Ceiling converted to mean sea level in hundreds of feet
MOBS - Mountain obscuration threshold met indicator
TCMS - Temporary/probability ceiling converted to mean sea level in hundreds of
feet
TMOB - Temporary/probability mountain obscuration threshold met indicator
WCMS - Worst case ceiling converted to mean sea level in hundreds of feet
WMOB - Worst case mountain obscuration threshold met indicator

MARINE

WHGT - Wave height in meters
WHFT - Wave height in feet
WPER - Wave period in seconds
HOWW - Height of wind wave in meters
POWW - Period of wind wave in seconds
HOSW - Height of predominant swell wave in meters
POSW - Period of predominant swell wave in seconds
DOSW - Direction of predominant swell wave in degrees
HOS2 - Height of secondary swell wave in meters
POS2 - Period of secondary swell wave in seconds
DOS2 - Direction of secondary swell wave in degrees
WAV2 - Combined wind wave period and height in feet ("2 group")
WAV3 - Combined predominant and secondary swell wave direction in tens of
degrees ("3 group")
WAV4 - Combined predominant swell wave period and height in feet ("4 group")
WAV5 - Combined secondary swell wave period and height in feet ("5 group")
WPHM - Combined wave period and height in half meters
WVSW - Combined swell wave direction, period and height in half meters
SWEL - Character combined swell wave direction, period and height in half meters
DAWV - Swell wave direction arrows of uniform length
IDTH - Thickness of ice on ship in meters
ROIA - Rate of ice accretion on ship from WMO code table 3551
IGRO - Rate of ice accretion on vessel in salt water in inches per 3 hours
DIGR - Character rate of ice accretion in inches per three hours
SHPD - True direction from which ship is moving (for 3 hours before obs) in degrees
SHPK - Ship's average speed (for 3 hours before obs) in knots
DASH - Ship's true direction arrows of uniform length


### AIRCRAFT

TURB - Amount of turbulence
TBSE - Base of turbulence in feet
TTOP - Top of turbulence in feet
HBOT - Base of turbulence in meters
HTOT - Top of turbulence in meters
FQOT - Frequency of turbulence
TPOT - Type of turbulence
TBSY - Turbulence symbol
ICNG - Amount of airframe icing
IBSE - Base of icing in feet
ITOP - Top of icing in feet
HBOI - Base of icing in meters
HTOI - Top of icing in meters
TPOI - Type of icing
ICSY - Icing symbol
WBSE - Base of weather in feet
WTOP - Top of weather in feet
HBWX - Base of weather in meters
HTWX - Top of weather in meters
CLC1 - Numeric cloud coverage 1
CBS1 - Cloud base 1 in feet
CTP1 - Cloud top 1 in feet
CB1M - Cloud base 1 in meters
CT1M - Cloud top 1 in meters
CLC2 - Numeric cloud coverage 2
CBS2 - Cloud base 2 in feet
CTP2 - Cloud top 2 in feet
CB2M - Cloud base 2 in meters
CT2M - Cloud top 2 in meters
ACRT - Aircraft report type
SELV - Flight level in meters
FELV - Flight level in hundreds of feet
ITSY - Icing type symbol
TTSY - Turbulence type symbol
TFSY - Turbulence frequency symbol
ACTP - Character aircraft type
ATP1 - Numeric aircraft type

MISCELLANEOUS

DDEN - Density of dry air in kg/(m**3)
VSBY - Visibility in statute miles
VSBK - Visibility in kilometers
VSBN - Visibility in nautical miles
VSBF - Character visibility in fractions of statute miles for visibilities between 0.
and 1.
VSBC - Character visibility in fractions of statute miles for all visibility numbers
PnnI - Precipitation over last nn hours in inches
nn = 01, 03, 06, 09, 12, 18 or 24
PnnM - Precipitation over last nn hours in millimeters
nn = 01, 03, 06, 09, 12, 18 or 24


```
DPRC - Character daily weather map precipitation in inches
PR24 - Precipitation over last 24 hours in inches, as sum of four successive 6-hour
precip amounts
SNOW - Snow depth in inches
SNEW - Amount of new snow in inches
SNRT - Forecast snow and ice pellet accumulation to watch threshold ratio
SI12 - Forecast snow and ice pellet 12-h accumulation in inches
SNIP - Snow and ice pellet watch threshold in inches
FZRT - Forecast freezing rain accumulation to watch threshold ratio
FZ12 - Forecast Freezing rain 12-h accumulation in inches
FZRN - Freezing rain watch threshold in inches
WEQS - Water equivalent of snow on the ground in inches
HAIL - Hail flag
HLSZ - Hail size in centimeters
HEAT - Heat index in Fahrenheit
HMTR - Humiture (apparent temperature) in Fahrenheit
WCEQ - Wind chill equivalent temperature in Fahrenheit
WCHT - Revised wind chill temperature in Fahrenheit
MSUN - Duration of sunshine in minutes
FFnn - Flash flood guidance for next nn hours in inches
nn = 01, 03, 06, 12 or 24
TOST - Type of station (manned or automatic)
0 = automatic
1 = manned
STIM - Report hour and minutes as hhmm
TEXT - Undecoded data
SPCL - Undecoded special reports
MARK - Markers
DTNM - Sea ice drifting distance in nautical miles
FOSB - Fosberg index, also called fire weather index
```
```
SPACING
```
```
SPAC - Plot a space, accounted for in FILTER
BLNK - Plot a blank, not accounted for in FILTER
```
3.146 SFPRMF

SFPRMF specifies the packing information for the surface file to be created. It may be

the name of a surface parameter packing file or the information itself entered as

follows:

### SFPRMF=PRM1/MIN1-MAX1-RES1;PRM2/MIN2-MAX2-RES2; ...

where each PRMi, MINi, MAXi, and RESi is the parameter name, minimum,

maximum and resolution for a parameter to be included in the file.

If SFPRMF specifies a file name, then each line of the file must contain the following

information separated by blanks or tabs:


```
parameter name CHAR*4
minimum data value REAL
maximum data value REAL
resolution REAL
```
The resolution should be an integral power of 10; otherwise the next smaller resolution

will be used. For example, res = .5 will become .1.

If the data are not to be packed, the minimum and maximum data values and the

resolution should not be included. Note that either all of the parameters or none of them

must have packing information.

METAR.PACK is a typical table used to pack surface data. This file may be used as

an example when creating a packing file.

When SFPRMF is used with program NAMSND, the user must supply the parameter

packing file name. In this case, a fifth entry is required for each line of the file,

containing a real conversion factor used to scale from BUFR to GEMPAK. Diagnosed

or extra computed parameters may be added at the end of the parameter list in the

packing file. If diagnosed parameters are to be added, or if there is not enough room in

the packing file for all the primary parameters, an auxiliary packing file must be used.

This file will have the same name as that specified for SFPRMF, with the suffix _aux.

The auxiliary file is not specified explicitly in SFPRMF. Rather, the presence of the

auxiliary file is indicated by suffixing the output surface file name given in SFOUTF

with a +. Note that the order of the parameters in the packing file(s) must be the same

as the order of the surface parameters listed in the NAMSND output file

bufr_table.dump. The parameter names themselves do not have to be the same as those

in the bufr_table.dump file, but the order must be preserved.

3.147 SHAPE

SHAPE is the object that the user wishes to plot on the current graphics device. Valid

shapes include:


```
TEXT A string of text.
POLYGON An irregular polygon.
REGPOLY A regular polygon.
ARC An arc connected to the center point (pie).
CURVE A smooth curve, or piecewise line.
LINE A line segment.
ARROW Two types of arrows.
COLDFRONT A cold front.
WARMFRONT A warm front.
OCCLFRONT An occluded front.
STATFRONT A stationary front.
WEATHER Weather symbols.
CLOUD Cloud type symbols.
SKY Sky cover symbols.
TURB Turbulence symbols.
ICNG Icing symbols.
SPCL Special symbols.
MARKER Marker symbols.
PRESTEND Pressure tendency symbols.
PASTWTHR Past weather symbols.
```
For all SHAPEs the INFO, LOCI and FILL parameters must also be specified.

3.148 SHIPFL

SHIPFL is a logical variable which indicates whether the surface file contains stations

which are not at a fixed location, such as moving ships, aircraft, or floating buoys.

A ship file will store and write data differently from a standard GEMPAK surface file.

However, these files can be read using the surface library and all programs which

access data in surface files can be used unchanged.

Note that station information cannot be added to these files, since the station

information must be stored with the data.

3.149 SKIP

SKIP is a variable which determines the contour points or plot points to skip. Input is

entered as:

```
skip_contour / skip_plot_x ; skip_plot_y
```
The defaults for skip_contour and skip_plot are 0.

Skip_contour thins the input grid before computing the contours to be drawn.


Skip_plot_x and _y specify the points at which data is to be displayed. If skip_plot_x

is positive and skip_plot_y is not specified, skip_plot_y is set to skip_plot_x.

If skip_plot_x is negative, the x plot locations on alternate rows are indented by half the

skip_plot_x value. In this case, the absolute value of skip_plot_x must be odd. If not,

the absolute value minus 1 is used. If no value for skip_plot_y is specified, half the

skip_plot_x value is used.

Examples:

### SKIP SKIP_CNTR SKIP_PLOT_X SKIP_PLOT_Y STAGGER

```
'' 0 0 0 no
22 00no
-1 0 0 0 no
2;3 2 0 0 no
```
```
/3 0 3 3 no
/2;3 0 2 3 no
/;1 0 0 1 no
```
```
/-1 0 1 0 yes
/-3 0 3 1 yes
/-1;1 0 1 1 yes
```
Winds may be thinned by latitude by setting skip_plot_y to a negative value. This

feature is most useful when plotting winds on a cylindrical grid on a projection with

converging meridians. In this case, ABS(skip_plot_y) determines how often rows are

plotted. The variable skip_plot_x is not used.

Examples:

### SKIP SKIP_CNTR SKIP_PLOT_X SKIP_PLOT_Y LAT THIN

```
/1;-1 0 1 1 yes
-- every row is plotted with winds thinned along each row.
```
```
/1;-2 0 1 2 yes
-- every other row is plotted with winds thinned.
```
3.150 SKPMIS

SKPMIS is a logical variable which indicates whether non-reporting stations will be

listed.

If SKPMIS is YES, nonreporting stations will not be listed. Otherwise, all stations will

be listed.


In SFMAP, markers can be plotted at nonreporting stations when SKPMIS is NO.

3.151 SNBUFR

SNBUFR is the name of the BUFR model sounding file to be used as input to create

GEMPAK sounding and surface data files using program NAMSND.

The first message of the BUFR file must be a table of information about the file

contents.

If the BUFR sounding file name is followed by |sss=#####, where sss is a 3-character

station ID and ##### is the corresponding station number, then an ASCII file named

prof.sss is generated. This file will contain only the output for station #####. For

example,

```
SNBUFR = eta.T00Z.class1.bufr|pit=72520
```
creates an ASCII file prof.pit containing sounding data for station 72520.

The number of stations and times which may be written to the output files is determined

by TIMSTN.

3.152 SNEFIL

SNEFIL is the name of the sounding edit file to be used to update a sounding data file.

SNEFIL is a text file which may be created using the program SNLIST with F as an

output device. A text editor may be used to create or change the edit file.

The parameters to be edited must be specified at the beginning of the edit file. For

example:

### SNPARM = PRES;TMPF;DWPF

If the output sounding file exists, the data set parameters must be the same as those

listed in the edit file. Station information, such as latitude, longitude, and elevation,

cannot be changed with SNEFIL. Use GEMPAK program SNSTNS to update station

information in a sounding file.

If the file does not exist, a new, unpacked file with these parameters will be created.


The data follows the parameter list. Only level data will be added to the file. Stability

indices from STNDEX will be ignored. The data should not include undecoded text

(raw reports), although the output sounding file, if it exists, may include undecoded

text.

Stations and times which do not already exist in the surface data file will be added.

3.153 SNFILE

SNFILE is the filename for an upper air data set.

The input for SNFILE may also be a file type. The following file type may be input:

```
UAIR observed upper air
```
When this file type is input, the program searches locally for the most recent file. If no

files are found locally, the program searches remotely for the most recent file. The

remote directories are specified in the table DATA.TBL.

An optional base time may be included with the file type name, following a bar (|). For

example:

```
SNFILE = uair | base time
```
The base time may be any valid format for a GEMPAK date/time. Any missing parts

of the date/time will be supplied by the system time.

3.154 SNOUTF

SNOUTF is the output sounding data file.

SNOUTF is used in programs which create a new sounding data file. It is also used in

programs in which data is moved from an input file to an output file, in which case

SNFILE is the original file.

3.155 SNPARM

SNPARM is a list of upper air parameters to be used in an upper-air program.

The four-character parameter names must be separated by semicolons. For example:

### SNPARM = PRES;TMPC;DWPC;THTA


If a parameter is not present in a data set, the program will attempt to derive it from

those parameters which are present. For example, if pressure, temperature and

dewpoint are present in a data set, then relative humidity can be derived.

In plotting programs, the position of the parameter determines where it will appear with

respect to the station location. The first position is centered on the station. The other

positions are shown below:

### 8

### 2104

### 315

### 6117

### 9

In the example above, DWPC is plotted in position 3.

The layer parameters are computed over a default layer between the specified level and

the next level upward in the sounding. The depth of the layer for layer averages may

be specified preceded by! in the user input. The shear and RICH calculations can be

done relative to a direction by specifying the direction preceded by $ in the user input.

In-line functions can be used to modify/qualify the output parameters. These functions

are applied in the order given; however, arithmetic operators must precede other

operators. The function symbols must precede the qualifying value and are defined as

follows:

```
* multiply = equal to
/ divide $ direction
+ add % vertical coordinate
```
- subtract! depth
< less than or equal to
> greater than or equal to

In the case of wind symbols, the size, line width, type and headsiz (arrows) for drawing

the symbol may be specified following a colon after the symbol name and after the

function specifications if any exist. The drawing attributes are separated by colons.

The type is a three-digit number, ABC, interpreted digit by digit as follows:

### ABC

```
1 = plot calm wind 1 = start at stn 1 = not filled
2 = ignore calm wind 2 = center at stn 2 = filled
3 = start on sky symbol 3 = not filled with box
4 = filled with box
```
Calm winds are plotted as a circle or an arrowhead for barbs and arrows, respectively.

Arrowheads and 50-unit barb flags are filled if C=2 or 4. A box is plotted with

background color if C = 3 or 4. For example, if TYPE=122, a barb will be plotted as a


circle if the wind is calm; it will be centered on the station location, and 50-unit flags

will be filled. The default is 111.

Aliases were created to make it easier for the user to plot typical sounding data by using

pre-set parameters and colors. To use an alias, enter the alias name for SNPARM and

leave COLORS blank. If the user does not leave COLORS blank, the selected colors

will be used instead of the pre-set alias colors.

See PRMLST.TBL for a list of current aliases for sounding data.

The GEMPAK sounding parameters and the corresponding four-character

abbreviations are:

### TEMPERATURES

```
TMPC - Temperature in Celsius
TMPF - Temperature in Fahrenheit
TMPK - Temperature in Kelvin
THTA - Potential temperature in Kelvin
THTE - Equivalent potential temperature in Kelvin
THTS - Saturation equiv. pot. temp. in Kelvin
THTV - Virtual potential temperature in Kelvin
TVRK - Virtual temperature in Kelvin
TVRC - Virtual temperature in Celsius
TVRF - Virtual temperature in Fahrenheit
```
### MOISTURE PARAMETERS

```
DWPC - Dewpoint in Celsius
DWPF - Dewpoint in Fahrenheit
DWPK - Dewpoint in Kelvin
DPDC - Dewpoint depression in Celsius
DPDF - Dewpoint depression in Fahrenheit
DPDK - Dewpoint depression in Kelvin
MIXR - Mixing ratio in g/kg
MIXS - Saturation mixing ratio in g/kg
PWTR - Precipitable water (mm) up to the specified level
RELH - Relative humidity in percent
TMWK - Wet bulb temperature in Kelvin
TMWC - Wet bulb temperature in Celsius
THWC - Wet bulb potential temperature in Celsius
TMWF - Wet bulb temperature in Fahrenheit
VAPR - Vapor pressure in millibars
VAPS - Saturation vapor pressure in millibars
LHVP - Latent heat of vaporization in J/kg
```
### HEIGHT

```
HGHT - Height in meters
HGTM - Height in meters (alternate name)
HGTK - Height in kilometers
```

```
HGTD - Height in decameters
HGFT - Height in feet
HGFH - Height in hundreds of feet
HGFK - Height in thousands of feet
HGML - Height in miles
STDZ - Character standard height convention used on upper air charts
RSTZ - Numeric standard height convention used on upper air charts
DHGT - Dry hydrostatic height in meters
MHGT - Moist hydrostatic height in meters
```
### PRESSURE AND ALTIMETER

```
PRES - Station pressure in millibars
PMSL - Mean sea level pressure in millibars
ALTI - Altimeter setting in inches from PRES
```
### WINDS

```
ARRW - Wind arrow (m/s)
ARRM - Wind arrow (m/s)
ARRK - Wind arrow (knots)
BARB - Wind barb (m/s)
BRBM - Wind barb (m/s)
BRBK - Wind barb (knots)
DARR - Wind direction arrows of uniform length
UWND - U component of the wind in meters/second
VWND - V component of the wind in meters/second
UKNT - U component of the wind in knots
VKNT - V component of the wind in knots
DRCT - Wind direction in degrees
SPED - Wind speed in meters/second
SKNT - Wind speed in knots
PSPD - Packed speed and direction (ddfff) in meters/second
PKNT - Packed speed and direction (ddfff) in knots
WCMP - Wind component toward a specific direction
WNML - Wind component toward 90 deg from a specific direction
```
### LIFTED CONDENSATION LEVEL

```
TLCL - Temperature in Kelvin of parcel raised to LCL
PLCL - Pressure in millibars of parcel raised to LCL
```
### STABILITY INDICES

Note: Default depths are enclosed in parentheses.

```
SHOW - Showalter Index
LIFT - Lifted Index (100 mb)
LFTV - LIFT computed by using virtual temperature
SWET - Sweat Index
KINX - K Index
CTOT - Cross Totals Index
```

```
VTOT - Vertical Totals Index
TOTL - Total Totals Index
CAPE - Convective Available Potential Energy (500 m)
CAPV - CAPE computed by using virtual temperature
CINS - Convective Inhibition (500 m)
CINV - CINS computed by using virtual temperature
EQLV - Equilibrium Level (500 m)
EQTV - EQLV computed by using virtual temperature
LFCT - Level of Free Convection
LFCV - LFCT computed by using virtual temperature
BRCH - Bulk Richardson Number (500 m, 6000 m)
BRCV - BRCH computed by using CAPV
MLTH - Mixed layer mean potential temperature (500 m)
MLMR - Mixed layer mean mixing ratio (500 m)
LCLT - Temperature in Kelvin of the LCL
LCLP - Pressure in millibars of the LCL
PWAT - Precipitable water (mm) for the whole sounding
```
### LAYER QUANTITIES

Note: Default depths are the layer depths in the sounding except for SEPA for which

the layer depth is 5 K.

```
RICH - Richardson number
BVFQ - Brunt-Vaisala frequency
BVSQ - Brunt-Vaisala frequency squared
BVPD - Brunt-Vaisala period
LAPS - Temperature lapse rate
STAB - Potential temperature lapse rate
STAP - Stability with respect to pressure in k/mb
SHRM - Wind shear magnitude
SHRD - Wind shear direction
SEPA - Vertical separation in mb between isentropes
```
### STATION VARIABLES

```
STID - Character station identifier
STNM - 5-digit station identifier
SLAT - Station latitude in degrees
SLON - Station longitude in degrees, West is negative
SELV - Station elevation in meters
RANG - Range (specialized data)
AZIM - Azimuth (specialized data)
LATI - Latitude in degrees
LONG - Longitude in degrees, West is negative
DELT - Delta time (specialized data)
```
### MISCELLANEOUS

```
DDEN - Density of dry air in kg/(m**3)
PSYM - Montgomery stream function
MARK - Markers
```

```
TEXT - Undecoded data
LHAN - Low level Haines Index
MHAN - Middle level Haines Index
HHAN - High level Haines Index
```
### SPACING

```
SPAC - Plot a space, accounted for in FILTER
BLNK - Plot a blank, not accounted for in FILTER
```
3.156 SNPRMF

SNPRMF specifies the packing formation for the sounding file to be created. It may be

the name of a sounding parameter packing file or the information itself entered as

follows:

### SNPRMF = PRM1/MIN1-MAX1-RES1;PRM2/MIN2-MAX2-RES2; ...

where each PRMi, MINi, MAXi, and RESi is the parameter name, minimum,

maximum and resolution for a parameter to be included in the file.

If SNPRMF specifies a file name, then each line of the file must contain the following

information separated by blanks or tabs:

```
parameter name CHAR*4
minimum data value REAL
maximum data value REAL
resolution REAL
```
The resolution should be an integral power of 10; otherwise the next smaller resolution

will be used ( e.g., res = .5 will become .1).

If the data are not to be packed, the minimum and maximum data values and the

resolution should not be included. Note that either all of the parameters or none of them

must have packing information.

It is important to remember to include the vertical coordinate in the parameter packing

list.

SNMERG.PACK is a typical table used to pack sounding data. This file may be used

as an example when creating a packing file.

Note that if MRGDAT is NO, the data will be packed using the standard packing for

unmerged data, and the value of SNPRMF will be ignored.


When SNPRMF is used with program NAMSND, the user must supply the parameter

packing file name. In this case, two more entries are required for lines of the file, in

addition to the four listed above. These entries must contain the GEMPAK names

corresponding to the BUFR sounding data parameters listed in the NAMSND output

file bufr_table.dump. The order of the GEMPAK names must be the same as the order

of the BUFR parameters, although the names themselves do not have to be the same.

The fifth column is the real conversion factor to be applied to the GEMPAK parameter

named in the sixth (last) column. Note that the last two columns of data are independent

of the first four, and are used only for BUFR to GEMPAK conversion. The parameter

named in column one does not necessarily have any relationship to the one referenced

in columns five and six. Diagnosed or extra computed parameters may be added

anywhere in the packing file for profile data.

3.157 SOURCE

SOURCE indicates whether the data used to compute the average station spacing are to

be read from a surface or sounding file. Use SF to read from a surface file and SN to

read from a sounding file.

3.158 SPLINE

SPLINE is a logical for using splines to interpolate the data to height levels. Valid input

are YES and NO. If NO is selected for SPLINE, linear interpolation, with respect to

log pressure, is used.

3.159 SQUALL

SQUALL is the length of the squall line used for the air and moisture flux

calculations.The default value is 20000 m. The coordinate rotation is implied in the

calculation of the fluxes, since only the V component of the wind is used in the

equations. Therefore, ROTATE should be set such that the long axis of the sqall line

is along X-axis.

3.160 STATION

STATION is the station to use in SFGRAM. Either a station character identifier or

station number may be entered.


If a single station is entered, all traces will use data from that station. If a list of stations

is entered, trace 1 will be for station 1, trace 2 for station 2, ... Stations in the list must

be separated using semicolons.

3.161 STNCOL

STNCOL specifies the color for the station identifier, time and the parameters specified

in STNDEX. These parameters are written at the top of the plot.

3.162 STNDEX

STNDEX is the list of stability indices or station parameters for upper-air data. The

items in the list must be separated using semicolons. The depth for the layer averages

may be specified preceded by a! in the user input. For example, BRCH!1000!8000

instructs the program to average over a mixed layer 1000 meters deep and lower

tropospheric layer 8000 meters deep. Similarly MLTH!750 results in an average

potential temperature over a mixed layer 750 meters deep while the default is 500

meters.

The following is a list of valid names:


```
SHOW Showalter index
LIFT Lifted index
LFTV LIFT computed by using virtual temperature
SWET SWEAT index
KINX K index
CTOT Cross totals index
VTOT Vertical totals index
TOTL Total totals index
CAPE Convective Available Potential Energy
CAPV CAPE computed by using virtual temperature
CINS Convective Inhibition
CINV CINS computed by using virtual temperature
EQLV Equilibrium Level
EQTV EQLV computed by using virtual temperature
LFCT Level of Free Convection
LFCV LFCT computed by using virtual temperature
BRCH Bulk Richardson Number
BRCV BRCH computed by using CAPV
LCLT Temperature in Kelvin of the LCL
LCLP Pressure in millibars of the LCL
MLTH Mean mixed layer potential temperature
MLMR Mean mixed layer mixing ratio
STID Station identifier
STNM Station number
SLAT Station latitude
SLON Station longitude
SELV Station elevation
TEXT Undecoded data
LHAN Low level Haines Index
MHAN Middle level Haines Index
HHAN High level Haines Index
```
3.163 STNFIL

STNFIL is the name of a file which contains station information which includes the

character identifier, number, name, state, country, latitude, longitude and elevation for

each station.

All this information, except the station name, is stored as station header information in

surface and sounding data files.

The file SFSTNS.TBL is a surface station table. SNSTNS.TBL is an upper-air table.

Information in the files must be stored using the exact format used in current tables,

since they are read with a FORTRAN FORMAT statement. They may be changed

using a text editor.


WARNING: The variable STNFIL is used for both surface and upper-air programs.

Care should be taken that the correct file is specified.

3.164 STNPLT

STNPLT allows the user to plot station markers and station information.

```
text color / text attributes | marker attributes | stnfile # column
```
Text color is the color of the station information. If the text color is set to 0, no station

information will be plotted. Text attributes are text size, font, width, border, rotation,

justification, and hw flag.

Marker attributes are color, type, size, width, and hw flag. If the marker color is 0, no

markers will be drawn and the station information will be centered on the station. If the

marker color is not specified, a default of 1 will be used.

Stnfile is the station table from which the station information is read.

Column refers to the station table and is the column number from which to get the

information to display. The following shows the columns for a station table:

### 1ID

```
2 Number
3 Name
4 State
5 Country
6 Latitude
7 Longitude
8 Elevation
9 Priority
10 Extra information (not always present)
```
If no column number is given, the station ID will be displayed.

Multiple files with differing colors, types, etc. may be processed by listing the

parameters individually separated with a '+' sign. See the third example.

Examples:

```
STNPLT = 5/1|3/12/1.25/2|sfstns.tbl
```
The text color of the station ID is color 5. The text size 1 is used. The marker color is

3. The marker type is 12. The marker size is 1.25 times larger than the default size and

the marker width is 2 times larger than the default width. The station table filename

from which the station information is read is sfstns.tbl.


```
STNPLT = 17/1|0|snstns.tbl
```
The text color of the station ID is color 17. The text size is 1. The marker color is 0.

No markers appear and the station ID will be centered on the station. Other marker

values are set to the defaults. The station table filename from which the station

information is read is snstns.tbl.

```
STNPLT = 17/1|0|snstns.tbl#3
```
Same as the previous example, but displays the station name instead of the ID.

```
STNPLT = 5/1|3/12/1.25/2|sfstns.tbl + 17/1|0|snstns.tbl
```
Combine and plot the first two examples.

3.165 STNTYP

STNTYP is used to select the data reporting characteristic of a station. Stations will

only be listed if they meet the selected characterstic(s).

The valid types are as follows:

```
A = list all stations
R = list reporting stations
M = list missing stations
U = list stations not in station table
L = list stations in station table
```
The characteristics may be combined. For example, STNTYP = ML will list missing

stations that are in the station table.

It is necessary to set AREA = DSET in order to view any unlisted stations.

IF STNTYP is blank, the default value of 'A' is used.

3.166 STREAM

STREAM lines/arrows/stop/slow/scale

The STREAM parameter controls several parameters dealing with the overall

streamline calculation and display. "Lines" is a real number multiplier which controls

the number (density) of streamlines drawn (default 1.0); "arrows" is a real number

multiplier which controls the number of arrowheads displayed (default 1.5*lines);


"stop" is a real number multiplier which controls how close a streamline comes to

another streamline before drawing is discontinued (default 0.5); "slow" is a real number

multiplier which controls the minimum vector speed threshold for stopping a

streamline (default 0.67); "scale" is a real number multiplier which controls how much

the input vector field is scaled prior to streamline calculation (default 0.33).

3.167 STRMID

STRMID specifies the storm identifier used by programs GPTPC, GPKGRF and

GPTCWW:

```
Storm id / advisory num / device / num fcst days / text flag
```
The storm id is the six or eight character storm identifier given for each particular

storm, for example, AL0300 or EP072001. The inclusion of the 2-digit century is

optional.

The advisory num is the number of the advisory that the user wishes to see displayed.

Both the storm id and the advisory num must be specified in order to view a particular

storm. If either or both of these fields are blank, the program will get the storm(s) to be

displayed by comparing the contents of the storm history file to the files in the storm

advisory directory. Each storm id and advisory number in the history file is checked

against the storms in the directory. If a storm advisory is found in the directory which

has a higher number than the advisory number given in the history file for that storm,

the new advisory is displayed and the history file is updated. If more than one new

storm advisory is in the directory for a specific storm, only the latest advisory will be

displayed. If new advisories for multiple storms are in the directory, the latest new

advisory for each storm will be displayed. (If a storm id and advisory number are

explicitly specified using the STRMID parameter, the history file is neither referenced

or updated.) The history file must be a local file named 'history.YYYY' for program

GPTPC, 'history_kg.YYYY' for program GPKGRF, and 'history_tc.YYYY' for

program GPTCWW, where YYYY is the 4-digit year. It looks like this:

```
n
stormid1_adv1
stormid2_adv2
.
.
.
stormidn_advn
```
where n is the number of storm identifiers in the file (in I4 format), and where stormid#

is the 8-character storm identifier (e.g., al892001) and adv# is the 3-character storm

advisory number (e.g., 009) for the most recently displayed advisory for stormid#.


The device is the device driver to which the graphics will be sent. The current choices

are limited to XW, GF, PSC and, for GPTPC and GPKGRF only, TEST. If TEST is

chosen, the program will display the graphics in an XW window only, but the program

becomes more interactive than if XW were specified. With TEST, the user has the

option of which graphics to see, and is prompted to move from one graphic to the next.

XW will create four graphics in successive windows for GPTPC with no user

intervention, and a single graphic for GPKGRF with no user intervention except for the

possible entering of breakpoints. If no device is specified, the default is GF. For

GPKGRF, regardless of the device specified, the track error watch/warn graphic will

be shown initially in XW, to allow for confirmation of user-specified breakpoints, if

any.

The num fcst days is the number of forecast days of data to display. (This field is not

used by program GPTPC.) Permissible values are 3 and 5. The default is 5.

The text flag is Y or N and controls whether or not the watch/ warning breakpoint text

product will be created. (This field is only used by program GPKGRF.) The default is

Y (yes).

3.168 SVRL

SVRL is the ending valid time for the SLS watches, the colors for the severe

thunderstorm and tornado (SLS) watches, a flag for plotting the start and stop times, a

flag for plotting the county names for the watches on the map, and a flag to outline the

county.

```
End time|SVRL colors|Time flag|Label flag|Outline flag|Color code
```
SLS watches that are valid at the ending time will be plotted on the map. The ending

time is given as a GEMPAK date/time string. Any missing items from the string will

be filled in by the system time. The ending time may also be LAST and ALL. LAST

will use the system time and plot all current SLS watches. ALL will plot all the watches

in the data files, whether they are active, cancelled or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are cyan

for thunderstorm and red for tornado.

The time flag is YES or NO and controls whether or not to plot the start and stop times

of the SLS watch on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the county name of the

SLS watch on the map. The default is NO.


The outline flag is YES or NO and controls whether to outline the SLS watch county

instead of plotting a marker. The default is NO. The outline colors are those specified

previously. If a watch is a "test" only, a hollow marker will be plotted at the county

centroid to indicate this, and no outlining will be done.

The color code flag is YES or NO, if YES, the colors are associated with the last digit

of the watch number, otherwise, based on the weather type ( tornado or thunderstorm ).

3.169 SYSTEM

SYSTEM is the system (storm) speed (m/s) and direction separated by a "/". These are

used to compute the winds relative to the system.

3.170 TAXIS

TAXIS contains the range, increment and location for labels on a time axis input as:

```
start-stop-inc;lb;gl;tm
```
START and STOP are GEMPAK date/times which may be abbreviated. If the values

are omitted, the data range will be used. The time axis will be reversed if the TAXIS

specification begins with R or if the times are input with a later time first.

INC is the time increment in hours and minutes. The form for INC is HHHMM. If one

or two digits are entered, hours will be assumed. If INC is omitted, a default

appropriate for the range will be used. If the time range exceeds 720 days, the

increment is ignored, and an appropriate labelling interval is selected automatically.

LB, GL, and TM are the frequencies for labels, grid lines, and tick marks.

Examples:

```
5/12-6/18-6 Draw the axis from 5/12 to 6/18 with
labels at 6-hour increments.
```
```
17/12-20/12-3;2;4;1 Draw the axis from 17/12 to 20/12 with
labels at 6-hour increments, grid lines
at 12-hour increments, and tic marks at
at 3-hour increments.
```

3.171 TCMG

TCMG is the ending valid time for the tropical disturbance, the colors for the

disturbance symbol, the arrows, and the storm danger area, and the name of the center

issuing the graphic.

```
End time|SY;AR;DA colors|center name
```
Tropical disturbances that are valid at the ending time will be plotted on the map. The

ending time is given as a GEMPAK date/time string. Any missing items from the string

will be filled in by the system time. The ending time may also be LAST. LAST will

use the system time and plot all current tropical disturbances. ALL is not valid for

TCMG.

The colors are separated by a semi-colon. If any color is set to 0, the color will not be

changed. If any color is missing, a default will be used. Defaults are red for the tropical

disturbance symbols, orange for the track arrows, and magenta, yellow, green, purple,

cyan, blue, coral, pink, dark green and dark orange for the danger areas for tropical

systems 1 through 10, respectively, in either the Atlantic or Pacific.

The name of the center is either TPC or CPHC. If anything other than CPHC is

specified, the center will default to TPC. TPC will plot Atlantic tropical disturbances

and Pacific tropical disturbances east of the dateline. CPHC will only plot Pacific

tropical disturbances east of the dateline.

Recommended values of GAREA for use with TCMG are GAREA = -4;-104;61;-2 for

TPC Atlantic disturbances GAREA = -4;-180;41;-80 for TPC Pacific disturbances

GAREA = -4;180;41;-140 for CPHC Pacific disturbances

3.172 TEXT

TEXT is the size, font, text width and hardware/software flag for graphics text

separated with slashes:

```
text size / font / width / border /
rotation / justification / hw flag
```
The size may be a real number multiplier for the default text size. If the size is zero or

unspecified, the current size will be used.

The size may also be a name, or the first character of a name. The name is converted

to a real number multiplier. For hardware text, the named sizes correspond to discrete

point sizes. Sizes other than the named sizes will be rounded to the nearest point size.

Any size may be given for software text. The standard names and size values are given

in the table FONTSZ.TBL:


```
Name Size HW Point
---- ---- --------
Tiny 0.714 10
Small 0.857 12
Medium 1.000 14
Large 1.286 18
Huge 1.714 24
Giant 2.429 34
```
The text width is the integer line width to be used in generating software text. If the

text size, font or width is not specified, the current value is used.

The text border is a border/blank fill flag. If the input value is less than or equal to zero,

the current value is used. Border is a three digit number, ABC, where:

```
A - Border B - Blank Fill C - Border Type
---------- -------------- ---------------
1 = No 1 = No 1 = Regular Box
2 = Yes 2 = Yes 2 = Low Pressure Box
3 = High Pressure Box
4 = Freezing Level Box
5 = Underline
6 = Overline
```
Low and high border types refer to the low and high outlines for use on aviation forecast

products.

The text rotation is a character input that specifies whether the text is aligned with the

screen (S) or with north (N) on a given image. If the choice is invalid or not specified,

the current value will be used.

The text justification is a character input that specifies whether the text is justified to

the center (C), right (R), or left (L). If the choice is invalid or not specified, the current

value will be used.

The hardware/software selector must be HW or SW to change to hardware- or software-

generated text. This selector can appear anywhere in the string.

The font number must be specified by using the HW selector and choosing a font

number from the list below.

### REGULAR ITALIC BOLD ITALIC-BOLD

```
Courier 1 11 21 31
Helvetica 2 12 22 32
Times 3 13 23 33
```
There are also two software fonts. Font 1 has the full set of upper and lower case letters.

Font 2 has only the upper case letters. Both fonts also contain all symbols and


numerals. If the font number, 1 or 2, is preceded by a digit greater than or equal to 2,

then the line width of the characters is increased to emulate a bold font. If a particular

device driver does not support hardware fonts, the requested font is emulated using the

software fonts.

Examples:

```
TEXT = 1/21/SW -- text size = 1; bold software font 1
```
```
TEXT = 1/2/HW -- text size = 1; hardware text font 2
```
```
TEXT = 2.5 -- text size = 2.5; current text font
```
```
TEXT = 1.24///221/s/c -- text size = 1.24; current text font;
current text width; Border = yes,
Blank fill = yes, Border type = box;
screen relative; center justified
```
3.173 THTALN

THTALN specifies the color, line type, line width, minimum, maximum, and increment

for the background dry adiabats (potential temperature lines) on thermodynamic

diagrams:

```
line color / line type / width / minimun / maximum / increment
```
The values should be separated by slashes. If THTALN is blank, no lines will be

drawn.

3.174 THTELN

THTELN specifies the color, line type, line width, minimum, maximum, and increment

for the background moist adiabats (equivalent potential temperature lines) on

thermodynamic diagrams:

```
line color / line type / width / minimun / maximum / increment
```
The values must be separated by slashes. If THTELN is blank, no lines will be drawn.

3.175 TILT

TILT is the Radar beam elevation/tilt angle. The tilt angle can be a real number. The

sweep with the closest mean elevation angle will be used.


3.176 TIMSTN

TIMSTN contains the maximum number of times to include in a file and the number of

stations to be included in addition to the stations in STNFIL.

3.177 TITLE

TITLE is the title color, title line, and title string separated by slashes:

```
title color / title line location / title string | short title
```
If the title color is 0, a title is not plotted.

The title line specifies the line on which the title will be written. The value of the title

line has the following meanings:

```
0 bottom line
-n n lines from bottom
+n n lines from top
```
If the line is not specified, the default is program dependent.

The title string is the title to be written. If no title string is specified, a default title will

be determined by the program.

In the grid display programs, special characters will be replaced as follows:

```
^ Forecast date/time
~ Valid date/time
@ Vertical level
_ Grid function
$ Nonzero scaling factor
# Grid point location
? Day of the week flag
```
If the information for which a character stands is not applicable to the program, nothing

is output in its place. Zero values of the scaling factor are not displayed.

If the "?" is included the abbreviated day of the week is added to the beginning of the

date/time string. The day of the week flag must always be used in combination with

either special character for specifying the date/time string. The result is the day of the

week for the valid date/time.

A short title may also be input by the user after a |. This is used to label the metafile

frame in the NC device driver. If the short title is blank, a suitable label is generated

for the frame. The day of the week is not included in the short title.


3.178 TOPOFL

TOPOFL is topographic input file. A topography file is 2 byte little endian (LSB) raster

of elevation data prepended by an 80 byte header that describes the navigation bounds.

TOPOFL can be either a file name, or a known abbreviation (current abbreviations are

DEM30 and DEM5 as described below).

If TOPOFL is DEM30, the source terrain file is the 30 second topographic data set

obtained from NGDC's GLOBE data set:
[http://www.ngdc.noaa.gov/seg/topo/globe.shtml](http://www.ngdc.noaa.gov/seg/topo/globe.shtml)
Note that this data set does not contain bathymetry. Sea floor is set to constant -500m.

If TOPOFL is DEM5, the source file is the 5 minute topographic and bathymetric data

set from NGDC's ETOP terrainbase set:
[http://www.ngdc.noaa.gov/seg/fliers/se-1104.shtml](http://www.ngdc.noaa.gov/seg/fliers/se-1104.shtml)

Topography files can be obtained from:
5 minute dataset (aka DEM5):
[http://my.unidata.ucar.edu/downloads/168/world_topo.5min.gz](http://my.unidata.ucar.edu/downloads/168/world_topo.5min.gz)
30 second dataset (aka DEM30):
[http://my.unidata.ucar.edu/downloads/169/world_topo.30s.gz](http://my.unidata.ucar.edu/downloads/169/world_topo.30s.gz)

Topography files can be placed in $GEMTBL/unidata/.

3.179 TRACE

TRACE parameters contain specifications for each trace on the meteogram in the

following format:

```
parameters/colors/range/witnes!parameters/colors/range/witnes
```
The parameters before the! will be plotted on the left of the plot; those after the! will

be plotted on the right. The parameters may be any GEMPAK surface parameter. Real-

valued parameters will be drawn as a graph. Character valued parameters will be

rotated 90 degrees and written on the plot. Symbol parameters will be drawn as

symbols.

For GUST and GUMS, the character G will be plotted. Up to four parameters may be

plotted along each axis. The parameters must be separated using semicolons.

Character and weather symbol data may only be plotted in positions 1, 2 or 3.

Each parameter name may be followed by a colon, the size or line type, a second colon

and the width. For example, WSYM:.5:5 will draw weather symbols half the default


size with a line width of 5. For example, TMPF:3 will plot a temperature line using

dash pattern 3.

The colors for the parameters must also be separated using semicolons. If a single

number is entered, all parameters are drawn in that color. If a zero is entered, the

current default color is used.

The range specifies the scaling of the y axis. The format is: start;stop;increment. Note

that in this program, the parts of range must be separated using semicolons. If no range

is given, it is selected using the data values.

Witness lines are specified in WITNES. These are horizontal dotted lines. A list of y

values may be entered separated by semicolons. If the value of witnes is YES, a witness

line will be centered on the plot.

3.180 TROPHT

TROPHT is the user estimated tropopause height in meters. The default value is 10000

m.

3.181 TRPINT

TRPINT is the user chosen distance above (and below) the tropopause which is used

for layer calculations. The default value is 1000 m.

3.182 TSTEP

TSTEP specifies the time step, in minutes, for the calculation of updated position of the

parcel within the grid domain.

3.183 TVS

TVS specifies the plot symbol and filter attributes for tornado vortex signatures:

```
color / marker / size / width / hw ; filter
```
A color value of 0 is used to turn off TVS plotting

FILTER is a logical variable or real number which controls the filtering of data in order

to eliminate plotting of overlapping data.


3.184 TXTFIL

TXTFIL specifies an ASCII text file to be read and displayed to the the current device

driver.

If the input file is LOGO, the NOAA logo is plotted. To specify plotting of the NOAA

LOGO enter:

LOGO | size | mode

or

NOAA | size | mode

The NWS logo can be plotted by entering:

NWS | size | mode

The size is a real number that specifies the size of the logo. The default size is 1.

The mode is a character that specifies the color mode of the logo. The options are 'M'

for monochrome or 'C' for full color. The default is set for 'M'.

3.185 TXTLOC

TXTLOC specifies the start location for plotting the contents of an ASCII text file

specified by TXTFIL. The X and Y coordinates are separated by a semi-colon:

```
x ; y
```
The X and Y are given in normalized-type coordinates for the given panel. They range

from 0.0 to 1.0 in each dimension. The origin is at the lower left corner of the panel.

For example,

### TXTLOC = .25 ; 1

The values are treated as a percentage of the panel size in each dimension. Therefore,

in this example, the text starts at a point 25% of the X direction panel size and at the top

of the panel in the Y direction.

TXTLOC can also specify the location in map coordinates. The map coordinates start

with a # and are separated by a semicolon:

```
#lat ; lon
```

3.186 TYPE

TYPE specifies the processing type for the GDPLOT2 GDPFUN parameter. The

TYPE list does not need separators, however slashes could be used for clarity:

```
type 1 / type 2 / ... / type n
```
Valid inputs for type are:

```
SCALAR TYPEs:
C the original GEMPAK contouring algorithm
L GEMPAK contouring algorithm without subboxes
F contour fill algorithm
X box algorithm -- same as contour fill, but draws lines
around the polygons rather than filling in polygons.
P plot grid point values
D plot scaler as a directional arrow
```
```
VECTOR TYPEs:
A wind arrows
B wind barbs
S streamlines
```
```
OTHER TYPEs:
M plot grid point markers
G plot grid indices (row/column numbers)
```
Note that contour attributes are specified in CONTUR. The contour interval and line

characteristics for types C, L, S and X are read from CINT and LINE and for type F

from FINT and FLINE. The wind arrow and barb characteristics are read from WIND.

The streamline characteristics are read from STREAM. The marker characteristics are

read from MARKER. The grid point index characteristics are read from GRDLBL.

Examples:

```
TYPE = C -- draws contour lines
```
```
TYPE =C/F -- draws filled contours overlaid with lines
```
```
TYPE = A -- plots wind arrows
```
```
TYPE = BS -- plots wind barbs and streamlines
```
```
TYPE = CFMP -- draws contour lines and fill, plots a marker
at all grid point locations and the
grid point values at all locations
```

3.187 UKAFIL

UKAFIL is the intermediate input/output ASCII file, e.g., SIGWXHI.txt.

3.188 VCOORD

VCOORD specifies the vertical coordinate system of the levels to process. Currently,

there are four coordinates:

```
NONE = surface data only
PRES = pressure
THTA = theta (isentropic)
HGHT = height
```
A list or range of levels will be based on the vertical coordinate type, with the exception

of MAN or VAS which always refer to a pressure coordinate system.

Note that data are ignored within superadiabatic layers found working upwards from

the surface when interpolations are performed to isentropic coordinates.

3.189 VERCEN

VERCEN allows the GDGRIB user to specify the contents of bytes 4, 5, 6, and 26 of

the GRIB PDS. The input is given as follows:

```
byte_4/byte_5/byte_6/byte_26
```
Values may be omitted in specifying VERCEN. For example, to omit the first and third

values, specifying only the second and fourth, enter the following:

```
/byte_5//byte_26
```
If values are omitted, the following defaults are inserted:

```
BYTE Default value Meaning
```
```
4 2 Parameter Table Version #
5 7 CENTER # (7->NCEP)
6 0 Generating Process ID
26 5 Sub-center (5->HPC)
```
See NCEP OFFICE NOTE 388 for more information on how to set these parameters.

Below is a table of center numbers that may be used in byte number 7 of the PDS.


ID# NAME ABBREV

007 US Weather Svc - Nat. Cntrs for Envir. Prediction NCEP

008 US Weather Svc - NWS Telecommunications Gateway NCEP

009 US Weather Svc - Field Stations NCEP

034 Japanese Meteorological Agency - Tokyo JMA

052 US Weather Svc (NCEP/TPC) Nat. Hurricane Cntr - Miami NHC

054 Canadian Meteorological Service - Montreal CMS

057 US Air Force - Global Weather Central GWC

058 US Navy - Fleet Numerical Oceanography Center FNOC

059 NOAA Forecast Systems Laboratory - Boulder, CO FSL

060 Nat. Center for Atmos. Research (NCAR) - Boulder, CO NCAR

074 UK Meteorological Office - Bracknell UKMO

085 French Weather Service - Toulouse FWS

097 European Space Agency (ESA) ESA

098 Eur. Cntr for Medium-range Wthr Frcsts - Reading ECMWF

099 DeBilt, Netherlands DEBILT

3.190 VGFILE

VGFILE is the name of the Vector Graphics File (VGF) to be displayed or processed.

It also specifies the name of the scale file and attribute file that are used by GPMAP to

alter objects in the VGF when displayed or stored in the output device, e.g., XW, FAX,

GIF, etc. These files allow the VGF object attributes to be tailored to meet the display

requirements of FAX, GIF, and other supported product formats. The scale and

attribute file names are specified using the "|" character as a delimiter in the following

format:

```
vgfile | scale file | attribute file
```
Specifying the scale file allows a limited set of object attributes in the VGF to be

changed in the output device by multiplying their values by numbers defined in the

scale file. The following parameters can be scaled:

```
Parameter Name Description
-------------- -----------
```

```
PIPSIZE Front Pip Size
FRONTSTRENGTH Frontal strength
SYMWIDTH Symbol width
SYMSIZE Symbol size
WINDWIDTH Wind barb/arrow width
WINDSIZE Wind barb/arrow size
WINDHDSIZ Wind barb head size
LINEWIDTH Line width
SPLWIDTH Special line width
SPLSIZE Special line pattern size
TEXTSIZE Text size
TEXTWIDTH Text width
SPTXTSIZE Special text size
SPTXTWIDTH Special text width
```
The parameter name must appear in the scale file followed by an equal sign, "=", and a

real value. See the table $GEMTBL/pgen/scale.fax for examples.

Specifying the attribute file allows VGF object attributes to be modified for output

devices by setting their values. Attributes such as color, size, thickness, etc. can be

changed for particular object sub-types, e.g., cold fronts. Also, sub-types can be

eliminated from the output device by setting their major color number to -1.

The value of the attribute for an object in the attribute file overrides the value in the

VGF with the following exceptions:

1. If the value for an attribute is set to 0, then it is not
    changed except in the case of smoothing.
2. If the attribute file value for smoothing is -1, then
    smoothing remains unchanged.
3. Rotation values in the attribute file are ignored.

```
The attribute file can also be used to change the object sub-type.
For example, all underlined text objects can be changed to non-underlined
text objects.
```
```
The attribute file uses a similar format as the table
$GEMTBL/pgen/setting.tbl. The file, $GEMTBL/pgen/uattribd.tbl
provides a sample template for the attribute file and additional
instructions.
```
3.191 WARN

WARN is the ending valid time for the warnings, the colors for the severe

thunderstorm, tornado and flash flood warnings, a flag for plotting the start and stop

times, a flag for plotting the county names for the warning on the map, and a flag to

outline the county.


```
End time|T-storm color;Tornado color;Flash flood color|Time flag|
Label flag|Outline flag
```
Warnings that are valid at the ending time will be plotted on the map. The ending time

is given as a GEMPAK date/time string. Any missing items from the string will be

filled in by the system time. The ending time may also be LAST and ALL. LAST will

use the system time and plot all current warnings. ALL will plot all the warnings in the

data files, whether they are active, cancelled or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are cyan

for thunderstorm, red for tornado, and green for flash flood.

The time flag is YES or NO and controls whether or not to plot the start and stop times

of the warning on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the county name of the

warning on the map. The default is NO.

The outline flag is YES or NO and controls whether to outline the warned county

instead of plotting a marker. The default is NO. The outline colors are those specified

previously. If a warning is a "test" only, a hollow marker will be plotted at the county

centroid to indicate this, and no outlining will be done.

3.192 WATCH

WATCH is the ending valid time for the watches, the colors for the severe thunderstorm

and tornado watches and a flag for plotting the start and stop times for the watch on the

map.

```
End time | Watch colors | Time flag | Label flag | Color code
```
Watches that are valid at the ending time will be plotted on the map. The ending time

is given as a GEMPAK date/time string. Any missing items from the string will be

filled in by the system time. The ending time may also be LAST and ALL. LAST will

use the system time and plot all current watches. ALL will plot all the watches in the

data files, whether they are active, cancelled or expired.

If a status message was issued for the watch, a line will be plotted dividing the expired

portion of the watch from the active portion.

The colors are separated by a semi-colon. If any color is set to 0, that type of line will

not be plotted. If any color is missing, a default will be used. Defaults are cyan for

thunderstorm, red for tornado, and yellow for status line.


The time flag is YES or NO and controls whether or not to plot the start and stop times

of the watch on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the watch box number

on the map. The default is NO.

The color code flag is YES or NO, if YES, the colors are associated with the last digit

of the watch number, otherwise, based on the weather type ( tornado or thunderstorm ).

3.193 WAVLEN

WAVLEN is the wavelength for the gravity or lee wave in kilometers. If no wavelength

is required, use 99999.

3.194 WAVSPD

WAVSPD is the wave speed in m/s. This is used for calculating the SCORER

parameter.

3.195 WCN

WCN is the ending valid time for the watch county notification(WCN), the colors for

the county bounds, a flag for plotting the start and stop times, a flag for plotting the

county names for the WCN on the map, and a flag to outline the county.

```
End time | county bounds color | Time flag | Label flag |
Watch number | Outline flag | Color code flag
```
WCNs that are valid at the ending time will be plotted on the map. The ending time is

given as a GEMPAK date/time string. Any missing items from the string will be filled

in by the system time. The ending time may also be LAST and ALL. LAST will use

the system time and plot all current WCNs. ALL will plot all the WCN in the data files,

whether they are active, cancelled or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are cyan

for thunderstorm, red for tornado.

The time flag is YES or NO and controls whether or not to plot the start and stop times

of the WCNs on the map. The default is NO.


The label flag is YES or NO and controls whether or not to plot the county name of the

WCNs on the map. The default is NO.

The watch number flag is YES or NO and controls whether or not to plot the watch

number of the WCNs on the map. The default is NO.

The outline flag is YES or NO and controls whether to outline the county instead of

plotting a marker. The default is NO. The outline colors are those specified previously.

If a WCN is a "test" only, and markers are selected, then a hollow marker will be plotted

at the county centroid to indicate this. If the outline flag is YES, there is no difference

when plotting regular reports and test reports.

The color code flag is YES or NO, if YES, the colors are associated with the last digit

of the watch number, otherwise, based on the weather type ( tornado or thunderstorm ).

3.196 WEIGHT

WEIGHT is the Barnes weighting parameter. Typical values are between 20 and 50.

A value of 20 is the default.

3.197 WIND

WIND specifies the wind symbol, size, width, type, and head size separated by slashes:

```
wind symbol / size / width / type / arrow head size
```
The wind symbol contains a letter for symbol type, a letter for symbol units and a color

number with no separators. The character meanings are:

### TYPE: B = BARB A = ARROW D = DIRECTIONAL

### ARROW

```
UNITS: K = KNOTS M = m/s
```
```
COLOR: Color number 0 = no wind plotted
```
The default is BM1, i.e., barbs in meters/sec plotted in color number 1. If a partial

specification is given, the remaining characteristics will be taken from the default.

The wind size is a real number which will be used as a multiplier for the default wind

symbol size. If this number is negative, zero, or missing, the current size will be used.

The sizes for barbs and arrows are independent.

The width is an integer specifying the line width to use in drawing the arrows or barbs.


The type is a four-digit number, FABC, interpreted digit by digit as follows:

### ABC

```
1 = plot calm wind 1 = start at stn 1 = not filled
2 = ignore calm wind 2 = center at stn 2 = filled
3 = start on sky symbol 3 = not filled with box
4 = filled with box
```
F is a flag which specifies whether to plot the wind barb on the opposite side from

where it would normally occur. If, F is not equal to zero, the wind barb is flipped.

Calm winds are plotted as a circle or an arrowhead for barbs and arrows, respectively.

Arrowheads and 50-unit barb flags are filled if C=2 or 4. A box is plotted with

background color if C = 3 or 4. For example, if TYPE=132, a barb will be plotted as a

circle if the wind is calm; it will begin on the edge of the cloud cover symbol, and 50-

unit flags will be filled. The default is 111.

The head size is a real valued multiplier used for the length of the arrow head. This

variable is not used for wind barbs.

3.198 WINPOS

WINPOS specifies the position for plotting winds for vertical profile plots. Up to three

separate wind profiles may be plotted. The stability indices will also be positioned at

the top of the plot according to WINPOS. Position 1 is the leftmost position. The value

of WINPOS will be incremented modulo 3 until the screen is cleared or the program is

run again.

3.199 WMOHDR

WMOHDR allows specification of a WMO header for a GRIB message.

```
WMO_ID / Origin_ID / DDHHMM
```
The first six bytes of the header (WMO_ID) must be given. The four-character

originating center identifier (Origin_ID) defaults to KWBC. The six-character

reference day, hour, and minute (DDHHMM) defaults to values from the reference time

in the GRIB PDS (bytes 15, 16, and 17, respectively).

Bytes 7 and 12 of the WMO header are always blank.

Only the 21-byte version of the WMO header is generated.


3.200 WOU

WOU is the ending valid time for the watch outline update(WOU), the colors for the

county bounds, a flag for plotting the start and stop times, a flag for plotting the county

names for the WOU on the map, and a flag to outline the county.

```
End time | county bounds color | Time flag | Label flag |
Watch number | Outline flag | Color code flag
```
WOUs that are valid at the ending time will be plotted on the map. The ending time is

given as a GEMPAK date/time string. Any missing items from the string will be filled

in by the system time. The ending time may also be LAST and ALL. LAST will use

the system time and plot all current WOUs. ALL will plot all the WOU in the data files,

whether they are active, cancelled or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are cyan

for thunderstorm, red for tornado.

The time flag is YES or NO and controls whether or not to plot the start and stop times

of the WOUs on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the county name of the

WOUs on the map. The default is NO.

The watch number flag is YES or NO and controls whether or not to plot the watch

number of the WOUs on the map. The default is NO.

The outline flag is YES or NO and controls whether to outline the county instead of

plotting a marker. The default is NO. The outline colors are those specified previously.

If a WOU is a "test" only, and markers are selected, then a hollow marker will be plotted

at the county centroid to indicate this. If the outline flag is YES, there is no difference

when plotting regular reports and test reports.

The color code flag is YES or NO, if YES, the colors are associated with the last digit

of the watch number, otherwise, based on the weather type ( tornado or thunderstorm ).

3.201 WSTM

WSTM is the ending valid time for the winter storms, the colors for the storm warning,

watch and advisory, a flag for plotting the start and stop times, a flag for plotting the

zone names for the storms on the map, and a flag to outline the zone.

```
End time|Warn color;Watch color;Advisory color|Time flag|Label flag|
Outline flag
```

Winter storms that are valid at the ending time will be plotted on the map. The ending

time is given as a GEMPAK date/time string. Any missing items from the string will

be filled in by the system time. The ending time may also be LAST and ALL. LAST

will use the system time and plot all current storm messages. ALL will plot all the

messages in the data files, whether they are active, cancelled or expired.

The colors are separated by a semi-colon. If any color is set to 0, that type of symbol

will not be plotted. If any color is missing, a default will be used. Defaults are red for

warning, orange for watch, and yellow for advisory.

The time flag is YES or NO and controls whether or not to plot the start and stop times

of the storm message on the map. The default is NO.

The label flag is YES or NO and controls whether or not to plot the zone name of the

storm message on the map. The default is NO.

The outline flag is YES or NO and controls whether to outline the storm zone instead

of plotting a marker. The default is NO. The outline colors are those specified

previously. If a message is a "test" only, a hollow marker will be ploteed at the zone

centroid to indicate this, and no outlining will be done.

3.202 XAXIS

XAXIS contains the left bound, right bound, label increment, and frequency

information separated by slashes in the form:

```
left/right/increment/lbfq;gdfq;tkfq
```
The frequencies for labels, grid lines, and tick marks follow the last slash and are

separated with semicolons. Appropriate defaults are provided if values are not

specified. If the increment is positive, all the label values will be divisible by the

increment. If it is negative, the label values will begin with the left value and be

separated by the increment. The bounds and increment are for the scaled data.

3.203 YAXIS

YAXIS contains the lower bound, upper bound, label increment, and frequency

information separated by slashes in the form:

```
lower/upper/increment/lbfq;gdfq;tkfq
```

The frequencies for labels, grid lines, and tick marks follow the last slash and are

separated with semicolons. If the increment is positive, all the label values will be

divisible by the increment. If it is negative, the label values will begin with the lower

value and be separated by the increment. The bounds and increment are for the scaled

data.

Defaults will be supplied if no specification is given. The following defaults will be

used when appropriate:

```
vcoord lower upper defaults
------ ----- ----- ----------------
PRES 1020 100 Mandatory levels
THTA 270 400 10
HGHT 0 20000 1000
```


