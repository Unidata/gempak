# NEXR2RHI

NEXR2RHI displays NEXRAD level II vertical cross sections.

### Input Parameters
 
    CXSTNS    Cross-section station line
    GVCORD    Grid vertical coordinate
    PTYPE     Plot type/h:w ratio/margins
    YAXIS     Ystrt/ystop/yinc/lbl;gln;tck
    CINT      Contour interval/min/max
    SCALE     Scalar scale / vector scale
    LINE      Color/type/width/label/smth/fltr
    BORDER    Background color/type/width
    TITLE     Title color/line/title
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    PANEL     Panel loc/color/dash/width/regn
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    CONTUR    Subbox/smooth
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    CTYPE     Contour type:  C/F
    RADFIL    Radar image filename(s)
    RADTIM    Radar composite current/dattim
    RADPARM   Radar parameter (dz, vr, sw)
    INTERP    Interpolation flag
 

### Program Description
 
NEXR2RHI displays NEXRAD Level II vertical cross sections for
data in ARCHIVE2 format. This program is adopted from GDCROSS to
create a volume rendering of NEXRAD data along a specified
axis.

RADFIL specifies the input Level II file. A template may be
specified, with an optional site name (eg NEXRII|KDDC).
If a site name is provided, it will be used to replace the %SITE%
alias in the template name. The NEXRII template is provided
for ARCHIVE2 format files.

RADPARM is the Radar parameter to be displayed. Valid values are
dz (reflectivity), vr (radial velocity), sw (spectrum width).

RADTIM is a valid GEMPAK date/time string or abbreviation. A time
range may be specified.

INTERP is a logical variable which determines whether interpolation
between sweeps will occur.

CXSTNS is the cross section axis (as in GDCROSS). A grid coordinate
can be used with row and colums 1 to 920.

GVCORD is the vertical coordinate of the radar data (always HGHT).

PTYPE is the plot type. A value of "LIN" is generally the only
useful setting.

Other parameters as in GDCROSS.

 
### Examples
 
1. Display an East-West RHI which passes through the radar location
for reflectivity data from KLVX. Display the RHI from 0 to 20KM
using color filled contours. Use a contour interval of 4 dBZ
with a minimum value of -12 dBZ. Grid point 460;460 is the radar center. Interpolate contours between beam scans.

        CXSTNS   = @1;460>@920;460
        GVCORD   = hght
        PTYPE    = lin
        YAXIS    = 0/20000
        CINT     = 0
        SCALE    = 0
        LINE     = 1
        BORDER   = 1
        TITLE    = 1/-2/RHI Base Reflectivity Level II ^
        CLEAR    = YES
        DEVICE   = xw
        TEXT     = .8/1/1/111/hw
        PANEL    = 0
        CLRBAR   = 1
        CONTUR   = 3/2
        FINT     = 4
        FLINE    = 0;30-7
        CTYPE    = f
        RADFIL   = NEXRII|KLVX
        RADPARM  = dz
        RADTIM   = last
        INTERP   = y

2. Display a cross section of radial velocity from 38.6N;86.8W to
37.4N;84.8W. Plot filled contours at 4 m/s intervals. Display data
from 0 to 20km. Allow interpolation between beam scans. Use
line countours, with negative values contoured by a dotted line,
positive values contoured with a solid line.

        CXSTNS   = 38.6;-86.8>37.4;-84.8
        GVCORD   = hght
        PTYPE    = lin
        YAXIS    = 0/20000
        CINT     = 4
        SCALE    = 0
        LINE     = 5/-10
        BORDER   = 1
        TITLE    = 1/-2/RHI Radial Velocity Level II ^
        CLEAR    = YES
        DEVICE   = xw
        TEXT     = .8/1/1/111/hw
        PANEL    = 0
        CLRBAR   =
        CONTUR   = 3/2
        FINT     =
        FLINE    =
        CTYPE    = c
        RADFIL   = NEXRII|KLVX
        RADPARM  = vr
        RADTIM   = last
        INTERP   = y


### Error Messages
 
    [NEXR2RHI  -1]  Fatal error initializing TAE.
    [NEXR2RHI  -2]  Fatal error reading TAE parameters.
    [NEXR2RHI  -3]  Fatal error initializing GEMPLT.
