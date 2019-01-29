# Surface Data Mapping

The program `sfmap` plots any parameters which can be derived from the data in a GEMPAK surface data file.

## SFMAP Input Parameters

<pre>
<a href=/software/gempak/man/parm/area.html>AREA</a>      Data area
<a href=/software/gempak/man/parm/garea.html>GAREA</a>     Graphics area
<a href=/software/gempak/man/parm/satfil.html>SATFIL</a>    Satellite image filename(s)
<a href=/software/gempak/man/parm/radfil.html>RADFIL</a>    Radar image filename(s)
<a href=/software/gempak/man/parm/imcbar.html>IMCBAR</a>    Color/ornt/anch/x;y/ln;wd/freq
<a href=/software/gempak/man/parm/sfparm.html>SFPARM</a>    Surface parameter list
<a href=/software/gempak/man/parm/dattim.html>DATTIM</a>    Date/time
<a href=/software/gempak/man/parm/sffile.html>SFFILE</a>    Surface data file
<a href=/software/gempak/man/parm/colors.html>COLORS</a>    Color list
<a href=/software/gempak/man/parm/map.html>MAP</a>       Map color/dash/width/filter flag
<a href=/software/gempak/man/parm/mscale.html>MSCALE</a>    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
<a href=/software/gempak/man/parm/latlon.html>LATLON</a>    Line color/dash/width/freq/inc/label/format
<a href=/software/gempak/man/parm/title.html>TITLE</a>     Title color/line/title
<a href=/software/gempak/man/parm/clear.html>CLEAR</a>     Clear screen flag
<a href=/software/gempak/man/parm/panel.html>PANEL</a>     Panel loc/color/dash/width/regn
<a href=/software/gempak/man/parm/device.html>DEVICE</a>    Device|name|x size;y size|color type
<a href=/software/gempak/man/parm/proj.html>PROJ</a>      Map projection/angles/margins|drop flag
<a href=/software/gempak/man/parm/filter.html>FILTER</a>    Filter data factor
<a href=/software/gempak/man/parm/text.html>TEXT</a>      Size/fnt/wdth/brdr/N-rot/just/hw flg
<a href=/software/gempak/man/parm/lutfil.html>LUTFIL</a>    Enhancement lookup table filename
<a href=/software/gempak/man/parm/stnplt.html>STNPLT</a>    Txtc/txt attr|marker attr|stnfil#col
<a href=/software/gempak/man/parm/clrbar.html>CLRBAR</a>    Color/ornt/anch/x;y/ln;wd/freq|text_info
<a href=/software/gempak/man/parm/lstprm.html>LSTPRM</a>    Filtered parm|x;y|TEXT info
</pre>


To list the variable definitions in the same format as they are entered, type `list` or `l` for short:
    
     GEMPAK-SFMAP>l
     AREA     = WV
     GAREA    = WV
     SATFIL   =
     RADFIL   =
     IMCBAR   =
     SFPARM   = SKYC;TMPF;WSYM;PMSL;;DWPF;BRBK
     DATTIM   = LAST
     SFFILE   = $GEMPAK/data/hrcbob.sfc
     COLORS   = 1
     MAP      = 1
     MSCALE   = 0
     LATLON   =
     TITLE    = 1
     CLEAR    = YES
     PANEL    = 0
     DEVICE   = XW
     PROJ     = MER
     FILTER   = YES
     TEXT     = 1
     LUTFIL   =
     STNPLT   =
     CLRBAR   =
     LSTPRM   =
     GEMPAK-SFMAP>


What are looking at? **hrcbob.sfc**? **WV**?

These are the program defaults and allow us to list and plot data immediately when starting to use GEMPAK.

Run `sfmap` now:

    GEMPAK-SFMAP>r
    Creating process: xw for queue 9797635
    SFMAP PARAMETERS:
    
    Data area:          WV
    Graphics area name: WV
    Valid parameters:    SKYC TMPF WSYM PMSL BLNK DWPF BRBK
    Parameter colors:     1    1    1    1    0    1    1
    Time:              910820/0600
    File:              $GEMPAK/data/hrcbob.sfc
    Map:               1
    Title:             1
    Device:            XW
    Projection:        MER
    Clear screen:      YES
    Filter:            YES
    Filter factor:      1.00
    Panel:             0 
    Enter &lt;cr> to accept parameters or type EXIT:

Press <enter> once more to display the map:

<img src="sfmap.gif" alt="image" />

The X window will display surface observations from "Hurricane Bob" in 1991, centered around West Virginia.

## Adjusting AREA and GAREA

`GAREA` is the graphics area: the part of the map that will be plotted. `AREA` is the data area: the area from which to select the data to plot. These areas exist separately for when you might want individual control over both the region of the map that is plotted (`GAREA`) and the region that is covered by data (`AREA`).

 Both are defined by the same syntax (below), but can be defined a number of different ways:


### Lat/Lon

in the form `LowerLeft (1); UpperRight (2)`.

    AREA = 25;-125;50;-65

> Be aware that South and West are negative!

### Center/Offset

in the form `#Center lat; Center lon; Delta lat; Delta lon`

    AREA = #37.5;-95;12.5;30

### Geographic Area
 
 defined in the GEMPAK geography table `$GEMTBL/stns/geog.tbl`

    AREA = US      the region around the United States.  
    AREA = MX      the region around Mexico      
    AREA = CO      the region around Colorado.
    AREA = @CO     only information inside Colorado.
    AREA = @AU:C   Australia (use :C syntac for countries
                      other than US, CN and MX)

## Station Area and Station Name

    AREA = LAX     the region around Los Angelos
    AREA = @DEN    only information from the Denver station
    AREA = @DEN;LAX;PDX
                     only Denver, LA and Portland stations
             
## Full Data Set

for all stations in the file

    AREA = DSET     

## Contracted and Expanded Areas

    AREA = CO+      AREA is contracted around CO
    AREA = CO++     AREA is contracted further around CO
    AREA = CO-      AREA is expanded beyond CO
    AREA = @CO--    AREA is expanded even more, but only includes
                      information inside Colorado

## Integer Value

Stations are selected if they fall into the range indicated

     AREA = SELV:0:2000   Selects stations with elevations
                          between 0 and 2000 meters                    
     AREA = SLAT:-45:45   Selects stations located between 45 
                          degrees south and 45 north           
     AREA = SLON:-45:45   Between 45 west and 45 east


Any station information can be used in this way:

* `STID` (Station ID-letters)
* `STNM` (Station Number)
* `SLAT` (Station Lat)
* `SLON` (Station Lon)
* `STIM` (Station Report Time)


> <img src=tip.gif width=40 align=left> If you want to plot a map with complete  data coverage, including the edges, you should make `AREA` bigger than `GAREA`. This can be done by adding a minus `-` to `AREA` or a plus `+` to `GAREA`:

`    GEMPAK-SFMAP> area = wv-
    GEMPAK-SFMAP> garea = wv
    GEMPAK-SFMAP> r
`

<img src=warning.gif width=30 align=left> **MAKE NOTE** `GAREA` can be specified in some of the same ways as `AREA`, with one caveat: **You can not use the `@` symbol to specify `GAREA`**.

Because `GAREA` defines the boundaries of the graph, adding `@` would force an irregular boundary. Think of the @ sign as a way to restrict the selection of data.

## SFPARM Surface Parameters

Multiple surface parameters may be defined in `SFPARM` by entering a list separated by semicolons:

    sfparm = tmpc;pmsl;skyc

GEMPAK can also calculate a number of parameters if the requested parameter is not present in the data set, for example, such as with relative humidity, which GEMPAK determines from a function defined
internally as `RELH=FUNC (TMPC;DWPT)`.

    sfparm = RELH

Arithmetic functions and upper and lower limits may be used in the paramater definition:

    sfparm = tmpc/2
    sfparm = pmsl>1000

where `pmsl>1000` restricts sea-level pressure records to those below 1000 mb,

You can manipulate the values of parameters using the following arithmetic functions:

* Multiplication `*`
* Division `/`
* Addition `+`
* Subtraction `-`
* Greater than `>`
* Less than `<`


Raw hourly and special reports (if stored in a file) can be accessed as well:

    sfparm = text
    sfparm = spcl
    sfparm = text;spcl
    sfparm = pmsl;tmpc;wthr;text

For now, to continue with our `SFMAP` example, set `sfparm` back to:

    sfparm = SKYC;TMPF;WSYM;PMSL;;DWPF;BRBK

## Date and Time

`DATTIM` is date and time, formatted in a number of ways:

**Explicit**

for all observations at time YYMMDD/HHMM:

    DATTIM = 101025/1200        

**Abbreviation**

for the last available date and/or time specified:

    DATTIM = 25/1200      DD/HHMM        
    DATTIM = 25/12        DD/HH    
    DATTIM = /1200        /HHMM
    DATTIM = 12           HH

**Range**

for all observations between the two date/time ranges:

    DATTIM = 17/00-17/1200

**Specific Times**

for all available observations that match the exact date/times listed:

    DATTIM = 17/00;17/06;17/12   

**List All**

for all times in the file, useful for when you don`t know the times when the
data was collected:

    DATTIM = list     

**Last**

for observations from the last date/time in the file:

    DATTIM = last                 

**All**

for observations from every date/time in the file, useful for comprehensive analyses:

     DATTIM = all

What happens when we change `DATTIM = all` for `sfmap`:

<img src="sfmap4.gif" alt="image" />

> <img src=tip.gif width=40 align=left> If  `DEVICE = xw` you should see an animation through all times in the file.  If `DEVICE = gif` you'll see stations plotted multiple times to the map if `CLEAR = no` is defined.

## Clearing the Map Window

`CLEAR` is a logical variable which determines whether the graphics screen is cleared before plotting.

* `CLEAR = YES` (or `CLEAR = y` for short) will clear the window of all previously-drawn graphics before plotting what has been specified.
* `CLEAR = NO` (or `CLEAR = n`) will draw what is currently specified over any previously-drawn graphics.


With `CLEAR = n`, you have the ability to overlay observations, images, maps and various vector and scalar quantities, depending on the GEMPAK program being used.  Additionally, because GEMPAK mapping programs operate within one `gplt` process (until `gpend` is called from the command line), you can overlay fields on the same map projection using multiple programs!

## Exercise #2 (Overlay Satellite Image)

Display the visible satellite image from 18Z and overlay the surface data
for the area covered by the image. Plot a standard station model for each location in the data set. The surface data to plot include:


* <font color=#00b0e8>sky cover symbol</font>
* <font color=#f80000>temperature in Fahrenheit</font>
* <font color=#f800f8>weather symbol</font>
* <font color=#1890f8>coded mean sea level pressure</font>
* <font color=#e8e800>pressure tendency with symbol</font>
* <font color=#00c800>dewpoint in Fahrenheit</font>
* <font color=#c88000>station ID</font>
* <font color=#00f8f8>wind barbs in knots</font>



Therefore, the surface parameters used would be

<tt>
SFPARM   =  <font color=#00b0e8>skyc</font>; <font color=#f80000>tmpf</font>; <font color=#f800f8>wsym</font>; <font color=#1890f8>smsl</font>; <font color=#e8e800>ptnd</font>; <font color=#00c800>dwpf</font>; <font color=#c88000>stid</font>;; <font color=#00f8f8>brbk</font>
</tt>

and colors

<tt>
COLORS   =  <font color=#00b0e8>26</font>; <font color=#f80000>2</font>; <font color=#f800f8>7</font>; <font color=#1890f8>25</font>; <font color=#e8e800>20</font>; <font color=#00c800>22</font>; <font color=#c88000>18</font>; <font color=#00f8f8>6</font>
</tt>
    
        AREA     =  us
        GAREA    =  dset
        SATFIL   =  $GEMPAK/data/VIS_910819_1801
        RADFIL   =  
        IMCBAR   =  
        SFPARM   =  skyc;tmpf;wsym;smsl;ptnd;dwpf;stid;;;brbk
        DATTIM   =  910819/1800
        SFFILE   =  $GEMPAK/data/hrcbob.sfc
        COLORS   =  26;2;7;25;20;22;18;6
        MAP      =  1
        LATLON   =  
        TITLE    =  1
        CLEAR    =  yes
        PANEL    =  0
        DEVICE   =  xw
        PROJ     =  sat
        FILTER   =  1
        TEXT     =  0.75/22//hw
        LUTFIL   =
        STNPLT   =
        CLRBAR   =
        LSTPRM   = 

`COLORS`, `MAP`, `TITLE`, `FILTER` and `TEXT` control the graphic colors, line thickness,
style, text fonts and titles. These map aesthetics allow you to generate very specific graphics.  Check the parameter definition pages in the <a href=/software/gempak/manual/variables_index>manual</a> for more detail, or with the `phelp` command at the GEMPAK prompt.

After running `sfmap`, you can see how the colors match the surface parameters:

     Valid parameters:    SKYC TMPF WSYM SMSL BLNK DWPF STID BLNK BLNK BRBK
     Parameter colors:    26    2    7   25   20   22   18    0    0    6 

<img src="example2.gif" alt="image" />

>> <img src=tip.gif width=40 align=left> **Save your session!** In the `sfmap` command prompt, you can save the current definitions with the command

    GEMPAK-SFMAP>save bob.nts

then exit the program and check that the file exists

    GEMPAK-SFMAP>e
    ls -la
    ...
    -rw-r--r--    1 gempak  staff      3151 Oct 29 14:38 bob.nts

You can reload this file in any other GEMPAK program, at any time, to retrieve the saved definitions, though only the variables which were saved in `sfmap` will be reloaded:

    GEMPAK-SFMAP> restore bob.nts

## Exercise #3 (Live Data)

Now let's look at data from today. Using an alias for real-time metar observations `SFFILE = metar`, and `DATTIM = 1200`, we can redisplay the map for this morning.

Make note of what the two new definitions are doing to retreive observations:

* The `SFFILE = metar` definition uses an **alias** to the latest GEMPAK metar surface file (typically `$GEMDATA/surface/YYYYMMDD_sao.gem`, where `YYYYMMDD` is the current year, month and day, such as `20121028`).
* `DATTIM = 1200` uses only the 1200 UTC records in `SFFILE`

But how to know the latest available? Exit `SFMAP` and return to the terminal, and execute the command line program `sfctime metar` and you will see surface reports in today's GEMPAK surface file for every twenty minutes:

    GEMPAK-SFMAP>e
    
    > sfctime metar
    ...     
    121028/1740     
    121028/1800     
    121028/1820     
    121028/1840     
    121028/1900 
    >

Inside our `sfmap` session, let's define a new `AREA`, `GAREA`, and `PROJ`:
    
     AREA     = us-
     GAREA    = us
     SFFILE   = metar
     DATTIM   = last
     SATFIL   = 
     CLEAR    = y
     PROJ     = STR/90;-100;0
     GEMPAK-SFMAP>r

<img src="sfmap5.gif" alt="image" />

>> <img src=tip.gif width=40 align=left> **Also note:** the position of parameters in a list,

    SFPARM   = skyc;tmpf;wsym;smsl;ptnd;dwpf;stid;;;brbk
                1    2    3    4    5    6   7       10

correspond to chart position:

    18      14      8       16      22
    19      2       10      4       23
    12      3       1       5       13
    20      6       11      7       24
    21      15      9       17      25


which also corresponed to the standard ordering of data on a surface chart:

<img src="standardchart.gif" alt="image" />

To overlay the current GOES satellite visible image on your map, you must explicitly define `SATFIL`.  To find the latest EAST-CONUS visible satellite image,  list the contents of `$SAT/EAST-CONUS/1km/VIS/` with the command:

    ls $SAT/EAST-CONUS/1km/VIS/ | tail -1
    VIS_20121029_2132

then use the entire path name for `SATFIL` (evironmental variables allowed):

    GAREA    = dset
    PROJ     = sat
    SATFIL   = $SAT/EAST-CONUS/1km/VIS/VIS_20121029_2132
    DATTIM   = last
    GEMPAK-SFMAP>r

<img src=warning.gif width=30 align=left> **Make Note**:


* When using `SATFIL` or `RADFIL`, you must set `PROJ` either to `sat` or `rad`.
* with `DATTIM = last`, there may not be any surface observations available in the `last` timeslot in the file referenced by `metar`.  If this is the case, explicitly define `DATTIM` using the a previous 20 minutes interval (i.e. `1140`, `1200`, `1220`).
* This method of finding a recent satellite image to overlay is cumbersome! We will soon explore how shell scripting can manage the latest files and images for your GEMPAK programs with ease.
* **Always** run `gpend` after finishing a GEMPAK graphics program session.

