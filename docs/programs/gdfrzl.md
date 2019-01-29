# GDFRZL

GDFRZL generates GFA FZLVLs in VG format from a scalar grid.

### Input Parameters
 
    GDATTIM   Grid date/time
    GLEVEL    Grid level
    GVCORD    Grid vertical coordinate
    GFUNC     Scalar grid
    GDFILE    Grid file
    CINT      Contour interval/min/max
    LINE      Color/type/width/label/smth/fltr/scflg
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    TITLE     Title color/line/title
    DEVICE    Device|name|x size;y size|color type
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    PROJ      Map projection/angles/margins|drop flag
    GAREA     Graphics area
    IJSKIP    Iskp;Istrt;Istp/Jskp;Jstrt;Jstp
    CLEAR     Clear screen flag
    PANEL     Panel loc/color/dash/width/regn
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    SCALE     Scalar scale / vector scale
    LATLON    Line color/dash/width/freq/inc/label/format
    HILO      Color/symbol/rng/rad/cnt/intp
    HLSYM     HILO txt size/posn/font/wdth/hw
    CLRBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    CONTUR    Subbox/smooth
    SKIP      Skip_cntr/skip_plt_x;skip_plt_y
    FINT      Fill interval/min/max
    FLINE     Fill colors/fill types
    CTYPE     Contour type:  C/F
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
    CYCLE     GFA cycle
    FHR       GFA forecast hour
    TAG       GFA tag
    STAT      GFA issuing status
    LVLINCR   GFA freezing level increment
 
 

### Program Description
 
GDFRZL create a VG file that contains GFA FZLVL elements.
It draws contours from the grid and save the contours and
labels into a temporary vg file. The the elements in the vg
file are converted to GFA FZLVLs.

The only valid device is VG. For example:

    DEVICE = VG|gfa.vgf

The default vg file is vgf.vgf.

The FHR parameter specifies the forecast hour of the GFA.

The Tag parameter is the GFA tag value.

The STAT parameter is the GFA issuing statues.

The LVLINCR parameter is used to round freezing level ranges.

The freezing level ranges are computed on the first run. The
following runs will use the same range information.

 
### Examples
 
1.  Create a VG file gfa.vgf that contains freezing levels
0;4000;8000;12000;16000 from gfs grid.

        GDATTIM	 =  f00
        GLEVEL	 =  0
        GVCORD	 =  frzl
        GFUNC	 =  mul(hght,3.28)
        GDFILE	 =  gfs
        CINT	 =  0;4000;8000;12000;16000
        LINE	 =  3///-1
        MAP	 =  1
        MSCALE	 =  0
        TITLE	 =  0
        DEVICE	 =  vg|gfa.vgf
        SATFIL	 =
        RADFIL	 =
        IMCBAR   =
        PROJ	 =  STR/90.0;-97.0;0.0
        GAREA	 =  29.3;-105.8;43.6;-84.1
        CLEAR	 =  yes
        PANEL	 =  0
        TEXT	 =  1
        SCALE	 =  0
        LATLON	 =
        HILO	 =
        HLSYM	 =
        CLRBAR	 =
        CONTUR	 =  0
        SKIP	 =
        FINT	 =  0
        FLINE	 =  10-20
        CTYPE	 =  C
        LUTFIL   =
        STNPLT   =
        FHR	 = 0-6
        TAG	 = 1W
        STAT	 = NRML
        LVLINCR	 = 500

### Error Messages
 
    [GDFRZL  +2]    WARNING:  ... not found.  CONTINUING---
    [GDFRZL  +1]    WARNING.  There are no contour levels.
    [GDFRZL  -1]    Fatal error initializing TAE.
    [GDFRZL  -2]    Fatal error reading TAE parameters.
    [GDFRZL  -3]    Fatal error initializing GEMPLT.
    [GDFRZL  -4]    Grid requested is not available.
    [GDFRZL  -5]    Error setting grid navigation for file ....
    [GDFRZL  -6]    There are no grids in grid file.
    [GDFRZL -13]    There are no times in the grid file
    [GDFRZL -14]    Couldn't open the temporary VG file ...
    [GDFRZL -15]    Out of boundary when reading grid
    [GDFRZL -16]    Invalid device
