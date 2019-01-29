# GPWARN

GPWARN is a version of GPMAP that plots filled county/zone regions from reports
which use the univeral generic county/zone identifier lines.

### Input Parameters
 
    MAP       Map color/dash/width/filter flag
    GAREA     Graphics area
    PROJ      Map projection/angles/margins|drop flag
    SATFIL    Satellite image filename(s)
    RADFIL    Radar image filename(s)
    LATLON    Line color/dash/width/freq/inc/label/format
    PANEL     Panel loc/color/dash/width/regn
    TITLE     Title color/line/title
    TEXT      Size/fnt/wdth/brdr/N-rot/just/hw flg
    CLEAR     Clear screen flag
    DEVICE    Device|name|x size;y size|color type
    SVRL      End time|TS;TN clrs|Tm|Lb
    WWFIL     Input source text file
    WWS       Valid Plot time {current or dattim}
    WWATT     Time range (min_prior/min_future)
    LUTFIL    Enhancement lookup table filename
    IMCBAR    Color/ornt/anch/x;y/ln;wd/freq|text_info
    OUTPUT    Output device/filename
 
 

### Program Description
 
GPWARN is derived from the standard GEMPAK program GPMAP.
The behaviour is similar to gpmap, with the addition of
plotting filled county/zones defined in a UGC specifier
with the map area specified. Satellite and radar data may
be used as backgrounds.

NWSTG Products which use the UGC identifier line include:

                                    PIL
    Tornado Warnings                TOR
    Severe Thunderstorm Warnings    SVR
    Flood Watches                   FLA|FFA
    Flood Warnings                  FLW|FFW
    Winter Weather Bulletins        WSW
    Non-precipitation Bulletins     NPW
    Special Weather Statements      SPS

GPWARN finds the UGC line following the pil line in the
bulletin which looks like:

    ALC129-131-MSC041-046>052-281030-

The program determines the counties/zone which are included in the
bulletin, along with the expiration time. The map information
for the county or zone to be plotted is read from the $GEMTBL/bounds
files. Each county is filled and outlined with a color code based
on bulletin type and expiration time.

The following gempak color codes are used:

    Type            Current         Expired
                   fill/outline    fill/outline
    Tornado         2/5             13/10
    Tstorm          14/5            13/10
    Flood Watch     21/5            19/18
    Flood Warn      23/5            19/18
    Winter          4/5             26/10
    Non-precip      7/5             29/10
    Special         6/5             24/26

The bulletins are read from the text bulletins which are
also used with NWX. These bulletins are stored under the
$TEXT_DATA tree under watch_warn/torn_warn, watch_warn/tstrm_warn,
watch_warn/winter, watch_warn/noprcp, watch_warn/special,
fflood/watch, fflood/warn.

The program allows the user to specify an input file list using the
WWFIL parameter. Multiple file names or template aliases may be
entered by separating each entry with a semicolor `;`. File templates
should be defined in `$GEMTBL/config/datatype.tbl`.

GPWARN will search for the files by checking for a template, and
then using the file name given, as well as
relative to $TEXT_DATA, eg.:

    WWFIL = tstrmwarn
    WWFIL = $TEXT_DATA/watch_warn/tstrm_warn/1997012800.warn
    WWFIL = watch_warn/tstrm_warn/1997012800.warn
    WWFIL = /fullpath/nwx/watch_warn/tstrm_warn/1997012800.warn

each of the above entries are valid.

The time for the plot may be spacified as current or a valid GEMPAK
format YYMMDD/HHMM time using the parameter WWS, eg:

    WWS = cur
    WWS = 970128/0000

GPWARN will use the time in WWS to determine if a bulletin
has expired by comparing against the time on the UGC line.

The time range for plotted bulletins is set using the WWATT
parameter. This parameter allows the user to specify how
long after the bulletin has expired that it will still be
plotted, and how far in the future bulletins will be viewed.
The format is WWATT = min_prior/min_future,
where min_prior and min_future are integers in minutes.
No display will occure for bulletins which have expired
greater than min_prior minutes before the time in WWS.

For bulletins which have not yet expired, min_future will
determine if the bulletin is to be plotted. This feature
is useful when plotting historical data, where products
exist at later times than were available at the time which
the plot represents, or in the case of advisories such
as winter storm bulletins which may be issued well in
advance of the expected conditions.

If min_future is negative (eg -60), then no display will
occur for bulletins which expire greater than 60 minutes
in the future. If min_future is positive (eg 60), then
bulletins that expire greater than 60 minutes in the
future will be displayed in white, so that they are not
confused with those regions which are more immenent.

For product generation scripts, you may which to cat
each of the desired file types to be plotted into a
single input file, otherwise, you may run the program
a sperate time for each input file desired.

The default it WWATT is blank is 1440/1440. If only
one value is specified, it is used for both.

In addition to plotting warnings, GPWARN also provides the
option of plotting counties included within watched using
the SVRL parameter as used in GPMAP. The files for SVRL are
generated by the dcsvrl decoder.

Color and line types for the areas drawn by GPWARN are specified
in the gpwarn.config file. If the file does not exist, a default
set of colors will be used. The config file will be searched for
in the standard locations, including the current working directory
in order to allow the user to tailor colors for the individual
application.


### Examples
 
1.  Draw a Lambert Conformal map of the US.
    Display current tornado, severe thunderstorm, flood,
    winter weather and non-precipitation warnings.
    Show counties/zones for up to 120 minutes after tha
    warning has expired. Begin to show warnings 1440
    minutes before they take affect.

         GAREA    = uslcc
         PROJ     = lcc
         SATFIL   =
         RADFIL   =
         LATLON   = 0
         PANEL    = 0
         TITLE    = 2/-2/Current Warnings
         TEXT     = 1.3/22/1/hw
         CLEAR    = yes
         DEVICE   = xw
         SVRL     =
         WWFIL    = nonpcpwarn;winterwarn;floodwarn;tstormwarn;tornadowarn
         WWS      = current
         WWATT    = 120/1400
         OUTPUT   = t


### Error Messages
 
    [GPWARN  -1]    Fatal error initializing TAE.
    [GPWARN  -2]    Fatal error reading TAE parameters.
    [GPWARN  -3]    Fatal error initializing GEMPLT.
