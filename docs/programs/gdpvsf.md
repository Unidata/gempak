# GDPVSF

GDPVSF vertically interpolates grid data to an arbitrary surface.

### Input Parameters 

    GDFILE    Input grid file  
    GDOUTF    Output grid file    
    GFUNC     Grid function           
    GDATTIM   Grid date/time                   
    GVCORD    Grid vertical coordinate                
    STARTL    Level to start search        
    STOPL     Level to stop search          
    DESIRE    Desired interpolation surface        
    GDOUTL    Output grid level (m)   
    GVOUTC    Output vertical coordinate
    GPACK     Number of bits/packing type
    GLIST     List of grids to interpolate    
    PMAX      Maximum pressure to search

### Program Description 

GDPVSF interpolates grid data vertically to an arbitrary 
functional surface.  In the following discussion, it will
be assumed that the functional surface is a surface of 
constant potential vorticity.

GDFILE is the input grid to use for interpolation, and it 
must contain the necessary grids at the specified times and
levels.  For example, for `GFUNC = MUL(AVOR(WND),STAP)`, GDFILE
must contain the grids `UREL (UWND), VREL (VWND) and STAP at
the vertical coordinate given by GVCORD.

GDOUTF must be an already-existing grid file.  GDPVSF will 
not create a new grid file if GDOUTF does not exist.  You 
can use GDDIAG to create the necessary grids for GDPVSF.

The grid function (potential vorticity) is specified in 
GFUNC.  DESIRE specifies the value of the potential 
vorticity surface to which the grids should be interpolated.
The vertical coordinate of the input grids is given by 
GVCORD.  The output vertical coordinate and grid level are
specified separately and arbitrarily by the user through
GVOUTC and GDOUTL, respectively.  GVOUTC must be defined 
either as PVBL (ascending) or PVAB (descending).

GDPVSF will not produce an interpolated grid of the input
vertical coordinate unless it is also specified in GLIST.
For example, if GVCORD is THTA but GLIST contains only UWND,
VWND and PRES, no output grid for the parameter THTA will
be written.  It is good practice to always include GVCORD
in GLIST. Grid functions can not be interpolated.

The program begins the search for the desired pv surface
at STARTL.  If the surface is not encountered by the time
STOPL is reached, the values at the grid point are set to
missing.  The first time GFUNC=DESIRE is encountered, with 
GFUNC increasing upward, the interpolation is performed and 
the search is started at the next grid point.  To prevent 
searching the lower troposphere while in THTA coordinates, 
PMAX may be set to a non-zero value.

Switching STARTL and STOPL causes the search to be made in 
the opposite direction.  Locations where the output is 
different for a different search direction imply that 
the surface is present at more than one level, i.e. that 
the tropopause is folded.

### Examples 

1.  Use an input grid file in THTA coordinates which 
    contains PRES, UWND, VWND, STAP!10, and RELH between 260 K
    and 350 K, interpolate THTA, UWND, VWND, and PRES to the 
    1.5 PVU surface. Assign the output grids to the nominal 
    15 m grid level in the preexisting file PV.GRD.  Look for 
    the lowest occurrence of the 1.5 PVU surface, assuming that it 
    occurs above 700 mb.  Call the output vertical coordinate pvbl,
    which represents the pv surface found from below.
                                                                
        GDFILE   =  input.grd                            
        GDOUTF   =  pv.grd                         
        GFUNC    =  mul(avor(obs),stap)     
        GDATTIM  =  last             
        GVCORD   =  thta
        STARTL   =  260 
        STOPL    =  350 
        DESIRE   =  0.00000015
        GDOUTL   =  15 
        GVOUTC   =  pvbl
        GPACK    =      
        GLIST    =  thta;uwnd;vwnd;pres    
        PMAX     =  700

2.  Repeat the search in the opposite direction.  Change
    the vertical coordinate to distinguish between this
    set of grids and the previous set of grids.

        GDFILE   =  input.grd                          
        GDOUTF   =  pv.grd                       
        GFUNC    =  mul(avor(obs),stap)   
        GDATTIM  =  last                                       
        GVCORD   =  thta                             
        STARTL   =  350 
        STOPL    =  260    
        DESIRE   =  0.00000015   
        GDOUTL   =  15    
        GVOUTC   =  pvab
        GPACK    =      
        GLIST    =  thta;uwnd;vwnd;pres    
        PMAX     =  700

