# GPNEXR2

GPNEXR2 displays NEXRAD level II products.

### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    RADFIL    Radar image filename(s)
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    LUTFIL    Enhancement lookup table filename
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    TILT      Radar beam elevation/tilt angle
    RADPARM   Radar parameter (dz, vr, sw)
    RADTIM    Radar composite current/dattim
 
 

### Program Description
 
GPNEXR2 displays NEXRAD Level II products in ARCHIVE2 format.

RADFIL specifies the input level II file. A template may be
specified, with an optional site name (eg NEXRII|KDDC).
If a site name is provided, it will be used to replace the %SITE%
alias in the template name. The NEXRII template is provided
for ARCHIVE2 format files.

RADPARM is the Radar parameter to be displayed. Valid values are
dz (reflectivity), vr (radial velocity), sw (spectrum width).

TILT is the Radar beam elevation/tilt angle. TILT is a real number
that specifies the mean sweep evelation that is closest to
the specified value. If TILT is LIST, then the available levels will
be displayed and the user will be prompted for a value.

RADTIM is a valid GEMPAK date/time string or abbreviation. A time
range may be specified.

LUTFIL specifies the color enhancement table.

 
### Examples
 
1.  Display the most recent reflectivity for KDDC using the sweep with
    mean elevation angle is closest to .5 degrees.

        MAP      = 6/1/1+3/1/2
        GAREA    = dset
        PROJ     = rad
        RADFIL   = NEXRII|KDDC
        LATLON   = 0
        PANEL    = 0
        TITLE    = 31
        TEXT     = 1
        CLEAR    = YES
        DEVICE   = xw
        LUTFIL   = default
        IMCBAR   = 31/V/LL/.005;.05/.85;.01|.7/1/1/hw
        TILT     = .5
        RADPARM  = dz
        RADTIM   = last

2.  Create an animation sequence for KDDC using data from
    2330Z yesterday (July 29th) to 0030Z today (July 30th).

        GAREA    = dset
        PROJ     = rad
        RADFIL   = NEXRII|KDDC
        LATLON   = 0
        PANEL    = 0
        TITLE    = 31
        TEXT     = 1
        CLEAR    = YES
        DEVICE   = xw
        LUTFIL   = default
        IMCBAR   = 31/V/LL/.005;.05/.85;.01|.7/1/1/hw
        TILT     = .5
        RADPARM  = dz
        RADTIM   = 29/2330-30/0030

### Error Messages
 
    [GPNEXR2  -1]   Fatal error initializing TAE.
    [GPNEXR2  -2]   Fatal error reading TAE parameters.
    [GPNEXR2  -3]   Fatal error initializing GEMPLT.
