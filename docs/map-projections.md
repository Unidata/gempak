
# Map Projections

`PROJ` determines the map projection, and directly affects the way the map looks on the screen.  `PROJ` and `GAREA` together give you the flexibility to generate very specific map projections.  Both  **simple map projections** and **full map projections** are avaiable to select:

## Simple Map Projections

Simple map projections only require the three-character identifier to be defined:

     proj = mer

 Available simple map projections:

     MER     Mercator
     NPS     North Polar Stereographic
     SPS     South Polar Stereographic
     LCC     Northern Hemisphere Lambert Conic Conformal
     SCC     Southern Hemisphere Lambert Conic Conformal
     CED     Cylindrical Equidistant
     MCD     Modified Cylindrical Equidistant
     UTM     Universal Transverse Mercator
     NOR     North Orthographic 
     SOR     South Orthographic 

## Full Map Projections

 For full map projections, each definition **MUST** be followed by three specified angles based on the projection class (CYL,AZM, CON), which are defined like so:
    
     proj = STR/a1;a2;a3
     proj = STR/90;-100;0
    
Where

* `a1` (90) is the standard latitude through which the projection plane passes
* `a2` (-100) is the central longitude (i.e. the longitude that lies parallel to the    grid columns).
* `a3` (0) angle to skew the projection, defined as zero for these examples.

 You can read about these angles and projections in more detail on the <a href=/software/gempak/man/parm/proj>`PROJ` parameter page</a>.

 Available full map projections:

     MER (CYL)   Mercator
     CED (CYL)   Cylindrical Equidistant
     MCD (CYL)   Modified Cylindrical Equidistant
     STR (AZM)   Polar Stereographic
     AED (AZM)   Azimuthal Equidistant
     ORT (AZM)   Orthographic
     LEA (AZM)   Lambert equal area
     GNO (AZM)   Gnomonic
     LCC (CON)   Northern Hemisphere Lambert Conic Conformal
     SCC (CON)   Southern Hemisphere Lambert Conic Conformal

