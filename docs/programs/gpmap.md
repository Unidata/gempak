# GPMAP

GPMAP draws a map, latitude/longitude lines, and various image and
graphic products.


### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    MSCALE    fgc;bgc;mask/units/lat;hide/values/anch/x;y/ln;wd/freq|text_info|t
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    LUTFIL    Enhancement lookup table filename
    STNPLT    Txtc/txt attr|marker attr|stnfil#col
    VGFILE    Vgfile | scale file | attribute file | filter
    AFOSFL    AFOS Graphics File
    AWPSFL    AWIPS Graphics File
    LINE      Color/type/width/label/smth/fltr/scflg
    WATCH     End time|Wtch clrs|Wtch Tm;Status Ln Tm|Watch Num;
    WARN      End time|TS;TN;FF clrs|Tm|Lb|Outline|Poly
    HRCN      End time|colors|syms|Tm|Lb|Mt|Qw|F12|F24|F36|F48|F72|F96|F120|Name
    ISIG      End time|colors|Sym|Tm|Id|Mv|Fl
    LTNG      End time|time ints/colors|markers
    ATCF      Time|colors|models|Tm|Id|Mv|Mkr|Name
    AIRM      Airmet Plotting Attributes
    GAIRM     GAirmet Plotting Attributes
    NCON      NCON attributes
    CSIG      End time|0_Hr;1_Hr;2_Hr;OL clrs|Seq0|Tm|Mv|Fl|Insty|Seq1|Seq2
    SVRL      End time|SVRL clrs|Tm|Lb|Outline|Clr
    BND       Bnd name/color/fillsiz/fillpat/filt/minpts!lincol/linpat/linwid!mr
    TCMG      End time|colors|center
    QSCT      QuickScat Plotting Attributes
    WSTM      End time|WN;WT;AD clrs|Tm|Lb|Outline/WN;WT;AD lwidth
    WOU       End time|Mrkr and Otln clrs|Tm|Lb|Wt|Clr|Mrkr|Otln|Fill/Fill clrs|
    WCN       End time|Mrkr and Otln clrs|Tm|Lb|Wt|Clr|Mrkr|Otln|Fill/Fill clrs|
    WCP       End time|Wtch clrs|Tm|Lb|Clr
    ENCY      Time|colors|models|Tm|Prs|Mkr|Ccd;wlevs|Fcst_Hour
    FFA       End time|FF;FA clrs|Tm|Lb|Ic|Outline/FF;FA lwidth
    WSAT      WindSAT Plotting Attributes
    ASCT      AScat Plotting Attributes
    OSCT      OScat Plotting Attributes
    TRAK      Altimetric Satellite Ground Track Prediction Plotting Attributes
    SGWH      Significant Wave Heights Plotting Attributes
    ASDI      End time|Mode|DepArr|time increment/clrs or ht range/clrs|time lmt
    EDR       End time|ht ranges/clrs|time lmt|Edr ranges/clrs|symb1/symb2|aoa18
    WSPDALT   Altimeter-derived Windspeed Plotting Attributes
 
 
### Program Description
 
GPMAP draws a map and/or latitude/longitude lines for a specified
graphics area.  Plots may be drawn in any GEMPAK projection and
may be overlaid on images.  GPMAP also post-processes NMAP
Vector Graphics Files (VGFs) to create products in any format
supported by GEMPAK device drivers.  In addition, GPMAP displays
various types of graphics products including AWIPS, AFOS, watches,
warnings, tropical or hurricane storm tracks, international
SIGMETs, lightning, ATCF tracks, airmets, non-convective
sigmets, severe local storm watches, winter storm warning,
watch and advisory, convective sigmets, and ensemble cyclone tracks.

Images are animated if more than one image file is specified in
SATFIL or RADFIL.  Images are sampled to correspond to the
geographic area specified by GAREA.  The geographic area may be
defined graphically by setting GAREA using the CURSOR command.
This command allows for interactive zooming of images and the
corresponding map and latitude/longitude plots.

GPMAP can post-process NMAP VGFs to create products in different
formats including AFOS, AWIPS Redbook, GIF, TIFF, 6 bit FAX, and
PostScript.  The VGFILE variable specifies the input VGF and the
DEVICE variable specifies the desired output format.  VGF object
attributes may be modified when creating products by specifying
a table (attribute file) in the VGFILE variable.  This table uses
the same format as the `$GEMTBL/pgen/setting.tbl` to alter object
attributes.  For example, the color, width, and smoothing flag
attributes can be changed for a solid line by setting them in the
attribute file.  The table, `$GEMTBL/pgen/uattribd.tbl`, provides a
template for this capability.  Refer to this table for additional
details.  GPMAP can also scale attributes for an entire class of
objects by specifying a table (scale file) in the VGFILE
variable.  Refer to the scale.fax table in `$GEMTBL/pgen` for
additional information.

GPMAP can also plot the current thunderstorm and tornado watches.
The parameter WATCH sets the ending time and the colors for
plotting the watches.  The parameter WARN sets the ending time
and the colors for plotting warnings.  The parameter WSTM sets the
ending time and the colors for plotting winter storm warning, watch,
and advisory. The parameter FFA sets the ending time and the colors
for plotting flash flood and areal flood watches. The parameter CSIG
sets the ending time and the colors for plotting convective sigmets.
The parameter HRCN sets the ending time and the colors for plotting
tropical depressions, storms, and hurricane positions.  Optionally,
a specific storm name may be entered to display only that specific
tropical disturbance. The parameter ISIG sets the ending time and
the colors for plotting international SIGMETs.  The parameter LTNG
sets the ending time, time increments, colors and markers for
plotting lightning data. The parameter ATCF sets the time, models
and colors for plotting ATCF (Automated Tropical Cyclone Forecast)
tracks.  As with HRCN, a specific storm name may be entered.  The
parameter AIRM sets the ending time and the colors for plotting
airmets.  The parameter NCON sets the ending time and the colors for
plotting non-convective sigmets.  The parameter SVRL sets the ending
time and the colors for plotting severe local storms watches. The
parameter WOU sets the ending time and the colors for plotting the
watch outline update (WOU). The parameter WCN sets the ending time
and the colors for plotting the watch county notification(WCN).  The
parameter WCP sets the ending time and the colors for plotting the
watch corner product.  The parameter ENCY sets the initial time,
colors, model names, and date/time, pressure and marker flags for
plotting of the ensemble cyclone tracks.

GPMAP can also plot QuikScat and ASCAT ocean vector wind data.
The parameter QSCT sets the type of QuikScat data (QSCT, QSCT_HI,
and four possible ambiguities, AMBG1, AMBG2, AMBG3, and AMBG4).
This parameter also sets the ending time, windspeed increments,
colors for plotting, wind barb/arrow attributes, timestamp
attributes and various plotting-related flags. The parameter ASCT
does the same for the ASCAT winds (ASCT, ASCT_HI, and the four
ambiguities: AAMBG1, AAMBG2, AAMBG1_HI, AAMBG2_HI).  The parameter
OSCT does the same for the OSCAT winds (OSCT, OSCT_HI, and the four
ambiguities: OAMBG1_HI, OAMBG2_HI, OAMBG3_HI, OAMBG4_HI).

GPMAP can also plot altimeter-derived wind speeds. The parameter
WSPDALT sets the source of the wind speeds: WSPDA (Altika),
WSPDC (Cryosat), and WSPD2 (Jason-2); the ending time of the wind
speed data, as well as the wind speed increments, color for each
increment, skip value, time stamp interval, and time stamp color.

 
### Examples
 
1.  Draw a Polar Stereographic map of the Northern Hemisphere.
Draw the map in color 1 and lat/lon lines in color 2 every
15 degrees.

        MAP	     =  1
        GAREA	 =  -10;-130;-10;50
        PROJ	 =  str/90;-80;0
        SATFIL	 =
        RADFIL	 =
        IMCBAR	 =
        LATLON	 =  2/10/1/1/15;15
        PANEL	 =  0
        TITLE	 =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw
        LUTFIL   =
        STNPLT   =
        VGFILE	 =
        AFOSFL	 =
        AWPSFL	 =
        LINE	 =
        WATCH    =
        WARN     =
        HRCN     =
        ISIG     =
        LTNG     =
        ATCF     =
        AIRM     =
        NCON     =
        CSIG     =
        SVRL     =
        WSTM	 =
        WOU      =
        WCN      =
        WCP      =
        ENCY     =
        FFA      =
        WSAT     =
        ASCT     =
        OSCT     =
        TRAK     =
        SGWH     =
        ASDI     =
        EDR      =
        WSPDALT  =

2.  Display two infrared satellite images in a second window
named "IR".  Overlay the VGF test.vgf.  In addition, display
the latest watches, warnings, international SIGMETs, lightning,
ATCF tracks, airmets and non-convective and convective sigmets,
SLS watches, winter storm messages, and all tropical
disturbances.

        MAP	     =  1/7
        GAREA	 =  dset
        PROJ	 =  sat
        SATFIL	 =  $GEMDATA/IR_910819_\18\01;19
        RADFIL	 =
        IMCBAR   =  1/V/LL/0.001;0.04/0.925;0.0125/1
        LATLON	 =  2/10/1/1/10;10
        PANEL	 =  0
        TITLE	 =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw|IR
        LUTFIL   =
        STNPLT   =
        VGFILE	 =  test.vgf
        AFOSFL	 =
        AWPSFL   =
	    LINE	 =
        WATCH    =  last
        WARN     =  last
        HRCN     =  all
        ISIG     =  last
        LTNG     =  last
        ATCF     =  last
        AIRM     =  last
        NCON     =  last
        CSIG     =  last
        SVRL     =  last
        WSTM	 =  last
        WOU      =
        WCN 	 =
        WCP 	 =
        ENCY     =
        FFA      =
        WSAT     =
        ASCT     =
        OSCT     =
        TRAK     =
        SGWH     =
        ASDI     =
        EDR      =
        WSPDALT  =

3.  Display two visible satellite images in a third window
named "VIS".  Overlay the AFOS test product T01 in
color 6 with line width 2.

        MAP	     =  1/7
        GAREA	 =  dset
        PROJ	 =  sat
        SATFIL	 =  $GEMDATA/VIS_910819_\18\01;19
        RADFIL	 =
        IMCBAR   =  1/V/LL/0.001;0.04/0.925;0.0125/1
        LATLON	 =  2/10/1/1/10;10
        PANEL	 =  0
        TITLE	 =  1
        TEXT	 =  1
        CLEAR	 =  yes
        DEVICE	 =  xw|VIS
        LUTFIL   =
        STNPLT   =
        VGFILE	 =
        AFOSFL	 =  NMCGPHT01
        AWPSFL   =
	    LINE	 =  6//2
        WATCH    =
        WARN     =
        HRCN     =
        ISIG     =
        LTNG     =
        ATCF     =
        AIRM     =
        NCON     =
        CSIG     =
        SVRL     =
        WSTM	 =
        WOU      =
        WCN      =
        WCP 	 =
        ENCY     =
        FFA      =
        WSAT     =
        ASCT     =
        OSCT     =
        TRAK     =
        SGWH     =
        ASDI     =
        EDR      =
        WSPDALT  =

4.  Postprocess an NMAP VGF to create an AWIPS Redbook Graphics file.
Change selected object attributes by using the table attrib.xmp
    
        MAP	     =  1
        GAREA    =  us
        PROJ     =
        SATFIL   =
        RADFIL   =
        IMCBAR   =
        LATLON   =  1
        PANEL    =  0
        TITLE    =  1
        TEXT     =  1
        CLEAR    =  y
        DEVICE   =  RBK
        LUTFIL   =  grey
        STNPLT   =
        VGFILE   =  symbols_big.vgf||attrib.xmp
        AFOSFL   =
        AWPSFL   =
        LINE     =  3
        WATCH    =
        WARN     =
        HRCN     =
        ISIG     =
        LTNG     =
        ATCF     =
        AIRM     =
        NCON     =
        CSIG     =
        SVRL     =
        WSTM	 =
        WOU	     =
        WCN      =
        WCP 	 =
        ENCY     =
        FFA      =
        WSAT     =
        ASCT     =
        OSCT     =
        TRAK     =
        SGWH     =
        ASDI     =
        EDR      =
        WSPDALT  =

Where `attrib.xmp` contains:
    
    CLASS_SYMBOLS  -99        -99    3   3     0    2.0
    CLASS_SYMBOLS  SPSYM_ELM   12    0   0     0    5.0

which means set all symbols to GEMPAK color 3
and size 2.0 except for symbol type SPSYM_ELM,
sub-type 12 (High symbol) which will retain
default color and be size 5.

### Error Messages
 
    [GPMAP  -1]     Fatal error initializing TAE.
    [GPMAP  -2]     Fatal error reading TAE parameters.
    [GPMAP  -3]     Fatal error initializing GEMPLT.
