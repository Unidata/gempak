# APPENDIX A

## GEMPAK PARAMETERS

## This appendix contains a list of the GEMPAK parameters. Algorithms

## used in computing these parameters are also included. The

## following constants are used in the computations:

```
KAPPA = Poisson's constant = 2 / 7
```
```
G = Gravitational constant = 9.80616 m/sec/sec
```
```
GAMUSD = Standard atmospheric lapse rate = 6.5 K/km
```
```
RDGAS = Gas constant for dry air = 287.04 J/K/kg
```
```
PI = Circumference / diameter = 3.
```
## References for some of the algorithms:

```
Bolton, D., 1980: The computation of equivalent potential
temperature., Monthly Weather Review, 108, pp 1046-1053.
```
```
Miller, R.C., 1972: Notes on Severe Storm Forecasting
Procedures of the Air Force Global Weather Central,
AWS Tech. Report 200.
```
```
Wallace, J.M., P.V. Hobbs, 1977: Atmospheric Science, Academic
Press, 467 pp.
```
### TEMPERATURE PARAMETERS

## TMPC - Temperature in Celsius

## TMPF - Temperature in Fahrenheit

## TMPK - Temperature in Kelvin

## STHA - Surface potential temperature in Kelvin

## STHK - Surface potential temperature in Kelvin


## STHC - Surface potential temperature in Celsius

## STHE - Surface equivalent potential temperature in Kelvin

## STHS - Surface saturation equivalent pot. temperature in Kelvin

## THTA - Potential temperature in Kelvin

## THTK - Potential temperature in Kelvin

## THTC - Potential temperature in Celsius

## THTE - Equivalent potential temperature in Kelvin

## THTS - Saturation equivalent pot. temperature in Kelvin

## TVRK - Virtual temperature in Kelvin

## TVRC - Virtual temperature in Celsius

## TVRF - Virtual temperature in Fahrenheit

## THTV - Virtual potential temperature in Kelvin

## TDXC - Maximum 24 hour temperature in Celsius

## TDNC - Minimum 24 hour temperature in Celsius

## TDXF - Maximum 24 hour temperature in Fahrenheit

## TDNF - Minimum 24 hour temperature in Fahrenheit

## T6XC - Maximum 6 hour temperature in Celsius

## T6NC - Minimum 6 hour temperature in Celsius

## T6XF - Maximum 6 hour temperature in Fahrenheit

## T6NF - Minimum 6 hour temperature in Fahrenheit


## DMAX - Daily weather map maximum temperature in Fahrenheit

## DMIN - Daily weather map minimum temperature in Fahrenheit

## SSTC - Sea surface temperature in Celsius

## SSTF - Sea surface temperature in Fahrenheit

## LTMP - Temp. in Celsius of surface air lifted to 500 or !x mb

#### TMPC = ( TMPF - 32 ) * 5 / 9

#### TMPC = TMPK - 273.

#### THTA = TMPK * ( 1000 / PRES ) ** KAPPA

#### STHA = TMPK * ( 1000 / PALT ) ** KAPPA

#### TVRK = TMPK * ( 1 + ( .001 * MIXR ) / .62197) ) /

#### ( 1 + ( .001 * MIXR ) )

#### THTV = TVRK * ( 1000 / PRES ) ** KAPPA

#### THTE = THTM * EXP [ ( 3.376 / TLCL - .00254 ) *

#### ( MIXR * ( 1 + .81 * .001 * MIXR ) ) ]

```
THTM = potential temperature for moist air
= TMPK * ( 1000 / PRES ) ** E
E = 2. / 7. * ( 1 - ( .28 * .001 * MIXR ) )
TLCL = temperature at the LCL in Kelvin
```
### MOISTURE PARAMETERS

## DWPC - Dewpoint in Celsius

## DWPF - Dewpoint in Fahrenheit

## DWPK - Dewpoint in Kelvin

## DPDC - Dewpoint depression in Celsius

## DPDF - Dewpoint depression in Fahrenheit


## DPDK - Dewpoint depression in Kelvin

## MIXR - Mixing ratio in g/kg

## MIXS - Saturated mixing ratio in g/kg

## SMXR - Surface mixing ratio in g/kg

## SMXS - Surface saturated mixing ratio in g/kg

## RELH - Relative humidity in percent

## TMWK - Wet bulb temperature in Kelvin

## TMWC - Wet bulb temperature in Celsius

## TMWF - Wet bulb temperature in Fahrenheit

## VAPR - Vapor pressure in millibars

## VAPS - Saturation vapor pressure in millibars

## LHVP - Latent heat of vaporization in J/kg

## PWTR - Precipitable water (mm) at any given level

```
DPDx = TMPx - DWPx
```
#### MIXR = .62197 * ( E / ( PRES - E ) ) * 1000

#### E = VAPR * [ 1.001 + ( PRES - 100) / 900 * .0034 ]

#### VAPR = 6.112 * EXP ( ( 17.67 * DWPC ) / ( DWPC + 243.5 ) )

```
MIXS = .62197 * ( Es / ( PRES - Es ) ) * 1000
Es= VAPS * [ 1.001 + ( PRES - 100 ) / 900 * .0034 ]
VAPS = 6.112 * EXP ( ( 17.67 * TMPC ) /( TMPC + 243.5 ) )
```
#### SMXR = .62197 * ( E / ( PALT - E ) ) * 1000

#### E = VAPR * [ 1.001 + ( PALT - 100 ) / 900 * .0034 ]

#### VAPR = 6.112 * EXP ( ( 17.67 * DWPC ) /\( DWPC + 243.5 ) )

```
SMXS = .62197 * ( Es / ( PALT - Es ) ) * 1000
Es= VAPS * [ 1.001 + ( PALT - 100 ) / 900 * .0034 ]
```

#### VAPS = 6.112 * EXP ( ( 17.67 * TMPC ) / ( TMPC + 243.5 ) )

#### RELH = VAPR / VAPS * 100

```
( TMPK - TMWK ) * Cp - ( Rsat (TMWK) - RMIX ) * LVAP = 0
Implicit equation solved by Newton's method for TMWK.
Cp - Specific heat at constant pressure
Rsat - Saturation mixing ratio at TMWK
RMIX - Mixing ratio
LVAP - Latent heat of vaporization
```
#### VAPR = 6.112 * EXP [ ( 17.67 * DWPC ) / ( DWPC + 243.5 ) ]

#### VAPS = 6.112 * EXP [ ( 17.67 * TMPC ) / ( TMPC + 243.5 ) ]

#### LHVP = ( 2.501 - .00237 * TMPC ) * 10E

### HEIGHT PARAMETERS

## HGHT - Height in meters

## HGTM - Height in meters

## HGTK - Height in kilometers

## HGTD - Height in decameters

## HGFT - Height in feet (3.28084 * HGHT)

## HGFH - Height in hundreds of feet

## HGFK - Height in thousands of feet

## HGML - Height in miles (6.2137E-04 * HGHT)

## DHGT - Dry hydrostatic height in meters

## MHGT - Moist hydrostatic height in meters

## STDZ - Character standard height convention used on u.-a. charts


## RSTZ - Numeric standard height convention used on u.-a. charts

```
For data below 500 mb, the standard height is the last three
digits of the height. For data at and above 500 mb, the
height is the last three digits of the height in decameters.
```
## ZMSL, Z000, Z900, Z850, Z800 - Estimated height at a pressure level

```
DHGT and MHGT are computed using the hypsometric
equation and integrating from the surface pressure
to the desired level. DHGT (MHGT) is computed
without (with) the influence of moisture.
```
#### DHGT = HB + ( RDGAS / G ) * LN ( PBOT / PTOP ) * TAVE

```
HB= height of the bottom of a layer
PBOT = pressure at bottom of a layer
PTOP = pressure at the top of a layer
TAVE = average temperature of a layer
```
#### MHGT = HB + ( RDGAS / G ) * LN ( PBOT / PTOP ) * TVAVE

```
HB = height of the bottom of a layer
PBOT = pressure at bottom of a layer
PTOP = pressure at the top of a layer
TVAVE = average virtual temperature of a layer
```
```
Zxxx = [ To * ( 1 - ( PRES / ALTM ) ** ( RDGAS * GAMUSD / G ) ] / GAMUSD
Zxxx = height at the pressure level
ZMSL : 1013.25 mb
Z000 : 1000 mb
Z900 : 900 mb
Z850 : 850 mb
Z800 : 800 mb
To= sea level temperature in U.S. Std. Atmos. = 288 K
```
### PRESSURE AND ALTIMETER PARAMETERS

## PRES - Station pressure in millibars

```
PRES is the actual pressure at a level as reported with upper air data.
```
## PMSL - Mean sea level pressure

```
PMSL is reported with surface data.
```
## PALT - Surface pressure in millibars from ALTI


## ALTI - Altimeter setting in inches of mercury

```
ALTI is reported with surface data.
```
## ALTM - Altimeter setting converted to millibars

## SALT - Abbreviated standard altimeter setting

## SMSL - Abbreviated mean sea level pressure in millibars

## SALI - Abbreviated altimeter setting in inches of mercury

## RMSL - First 3 digits left of decimal of PMSL * 10

## RSLI - First 3 digits left of decimal of ALTI * 100

## RSLT - First 3 digits left of decimal of ALTM * 10

## PTND - Pressure tendency: graphics symbol with numeric change

## PTSY - Graphics symbol for pressure tendency

## P03C - 3-h numeric pressure change

## P03D - Pressure tendency and change group, appp

## P24C - 24-h numeric pressure change

#### PMSL = PRES * EXP ( ( G * SELV ) / ( RDGAS * TVAVE ) )

```
SELV = station elevation
TVAVE = average virtual temperature between station and sea level
= TVRK + ( DELTV / 2 )
DELTV = GAMUSD * SELV / 1000
```
#### PALT = ALTM *

```
( 1 - ( ( SELV / 1000 ) * GAMUSD / To ) ) **
( G / ( GAMUSD * RDGAS ) * 1000 )
SELV = station elevation in meters
To = sea level temperature in U.S. Std. Atmos. = 288 K
```
#### ALTM = ALTI * ( 1013.25 / 29.921 )


#### SALT = MOD ( ALTM * 10, 1000 )

## PANY - Returns PMSL, if avaliable, if not, returns ALTM

## RANY - Computes the 3 digit display of pressure

## SANY - Creates a 3 character string from integral part of PMSL or ALTM

### WINDS

## UWND - U-component of the wind in meters/second

## VWND - V-component of the wind in meters/second

## UKNT - U-component of the wind in knots

## VKNT - V-component of the wind in knots

## DRCT - Wind direction in degrees

## SPED - Wind speed in meters/second

## SKNT - Wind speed in knots

## SMPH - Wind speed in miles/hour

## PSPD - Packed direction and speed in meters/second (ddfff)

## PKNT - Packed direction and speed in knots (ddfff)

## GUST - Wind gusts in knots

## GUMS - Wind gusts meters/second

## PWDR - Peak wind direction in degrees

## PWSP - Peak wind speed in meters/sec


## PWHR - Hour of peak wind

## PWMN - Minutes of peak wind

## WNML - Wind component toward a direction 90 degrees counter-clock-

## wise from a specified direction.

## WCMP - Wind component toward a specified direction

## BARB - Barb feathered in m/s

## BRBM - Barb feathered in m/s

## BRBK - Barb feathered in knots

## BRBS - Barb feathered in mi/hr

## ARRW - Arrows scaled in m/s

## ARRM - Arrows scaled in m/s

## ARRK - Arrows scaled in knots

## DARR - Wind direction arrows of uniform length

#### UWND = - SIN ( DRCT ) * SPED

#### VWND = - COS ( DRCT ) * SPED

#### DRCT = ATAN2 ( -UWND, -VWND ) * 180 / PI

#### SPED = SQRT ( UWND ** 2 + VWND ** 2 )

#### SPED = SKNT / 1.

#### PSPD = JDRCT * 100 + JSPED

#### JDRCT = NINT ( DRCT / 5 )

#### JSPED = NINT ( SPED )

#### PKNT = JDRCT * 100 + JSKNT

#### JDRCT = NINT ( DRCT / 5 )

#### JSKNT = NINT ( SKNT )


#### WCMP = - COS ( DRCT - SPCD ) * SPED

```
SPCD is the specified direction
```
#### WNML = - COS ( DRCT - ( SPCD - PI/2 ) ) * SPED

```
SPCD is the specified direction
```
### LIFTED CONDENSATION LEVEL (LCL)

## TLCL - Temperature in Kelvin at the LCL from the given level

## PLCL - Pressure in millibars at the LCL from the given level

#### TLCL = [1 / ( 1 / ( DWPK - 56 ) + LN ( TMPK / DWPK ) / 800 )] + 56

#### PLCL = PRES * ( TLCL / ( TMPC + 273.15 ) ) ** ( 1 / KAPPA )

```
Poisson's equation
```
### STABILITY INDICES

```
Note: Default depths given in the definitions below are preceded
by an! and may be changed using the in-line depth
specification. Negative depths reset to the default values.
Zero depth will yield missing values for layer parameters.
dz in the definitions below defaults to the layer thickness
in the sounding.
```
## BRCH - Bulk Richardson number

#### BRCH = CAPE / ( 0.5 * U**2 )

```
CAPE = Convective Available Potential Energy
U = magnitude of shear ( u2 - u1, v2 - v1 )
u1,v1 = average u,v in the lowest !500 m
u2,v2 = average u,v in the lowest !6000 m
```
## BRCV - BRCH computed by using CAPV

#### BRCH = CAPV / ( 0.5 * U**2 )

```
CAPV = CAPE computed by using virtual temperature
U = magnitude of shear ( u2 - u1, v2 - v1 )
u1,v1 = average u,v in the lowest !500 m
u2,v2 = average u,v in the lowest !6000 m
```

## BVFQ - Brunt-Vaisala frequency in a layer

#### BVFQ = SQRT ( ( G / THTA ) * STAB )

## BVPD - Brunt-Vaisala period in a layer

#### BVPD = 2. * PI / BVFQ

## BVSQ - Brunt-Vaisala frequency squared in a layer

#### BVSQ = BVFQ ** 2

## CAPE - Convective Available Potential Energy

#### CAPE = GRAVTY * SUMP ( DELZ * ( TP - TE ) / TE )

```
SUMP = sum over sounding layers from LFCT to EQLV for which ( TP - TE ) is
greater than zero
DELZ = incremental depth
TP= temperature of a parcel from the lowest !500 m of the atmosphere, raised dry
adiabatically to the LCL and moist adiabatically thereafter
TE= temperature of the environment
```
## CAPV - CAPE computed by using virtual temperature

## CAPV = GRAVTY * SUMP ( DELZ * ( TVP - TVE ) / TVE )

```
SUMP = sum over sounding layers from LFCV to EQTV for which ( TVP - TVE ) is greater
than zero
DELZ = incremental depth
TVP = virtual temperature of a parcel from the lowest !500 m of the atmosphere,
raised dry adiabatically to the LCL and moist adiabatically thereafter
TVE = virtual temperature of the environment
```
## CINS - Convective Inhibition

#### CINS = GRAVTY * SUMN ( DELZ * ( TP - TE ) / TE )

```
SUMN = sum over sounding layers from top of the mixed layer to LFCT for which (
TP - TE ) is less than zero.
DELZ = incremental depth
TP= temperature of a parcel from the lowest !500 m of the atmosphere, raised dry
adiabatically to the LCL and moist adiabatically thereafter
TE= temperature of the environment
```
## CINV - CINS computed by using virtual temperature

#### CINV = GRAVTY * SUMN ( DELZ * ( TVP - TVE ) / TVE )

```
SUMN = sum over sounding layers from top of the mixed layer to LFCV for which (
TVP - TVE ) is less than zero.
```

```
DELZ = incremental depth
TVP = virtual temperature of a parcel from the
lowest !500 m of the atmosphere, raised
dry adiabatically to the LCL and moist
adiabatically thereafter
TVE = virtual temperature of the environment
```
## CTOT - Cross Totals index

#### CTOT = TD850 - T

```
TD850 = Dewpoint in Celsius at 850 mb
T500 = Temperature in Celsius at 500 mb
```
## EQLV - Equilibrium level

```
EQLV = level at which a parcel from the lowest !500 m of the atmosphere is raised dry
adiabatically to the LCL and moist adiabatically to a level above which the virtual temper-
ature of the parcel is the same as the environment. If more than one Equilibrium Level
exists, the highest one is chosen.
```
## EQTV - EQLV computed by using virtual temperature

## KINX - K index

#### KINX = ( T850 - T500 ) + TD850 - ( T700 - TD700 )

```
T850 = Temperature in Celsius at 850 mb
T500 = Temperature in Celsius at 500 mb
TD850 = Dewpoint in Celsius at 850 mb
T700 = Temperature in Celsius at 700 mb
TD700 = Dewpoint in Celsius at 700 mb
```
## LAPS - Temperature lapse rate in a layer

```
LAPS = d (TMPK) / dz = d (TMPC) / dz
```
## LCLP - Pressure in millibars at the LCL from the surface

#### LCLP = PRES * ( LCLT / ( TMPC + 273.15 ) ) ** ( 1 / KAPPA )

```
Poisson's equation
```
## LCLT - Temperature in Kelvin at the LCL from the surface

#### LCLT = [1 / ( 1 / ( DWPK - 56 ) +

#### LN ( TMPK / DWPK ) / 800 )] + 56

## LFCT - Level of Free Convection by comparing temperature between


```
a parcel and the environment
```
```
LFCT = level at which a parcel from the lowest !500 m of the atmosphere is raised dry
adiabatically to LCL and moist adiabatically to the level above which the parcel is posi-
tively buoyant. If more than one LFCT exists, the lowest level is chosen. If the parcel is
positively bouyant throughout the sounding, the LFCT is set to be the same as the LCLP.
If the parcel is negatively bouyant throughout the sounding, the LFCT is set to missing.
```
## LFCV - LFCT computed by using virtual temperature

## LIFT - Lifted index

```
LIFT = T500 - Tparcel
T500 = temperature in Celsius of the environment at 500 mb
Tparcel = 500 mb temperature in Celsius of a lifted parcel with the average pres-
sure, temperature, and dewpoint of the layer !100 mb above the surface
```
## LFTV - LIFT computed by using virtual temperature

## LHAN - Low elevation Haines Index

## MHAN - Middle elevation Haines Index

## HHAN - High elevation Haines Index

## MLMR - Mean mixed layer MIXR

```
MLMR = average MIXR in the lowest !500 m
```
## MLTH - Mean mixed layer THTA

```
MLTH = average THTA in the lowest !500 m
```
## PWAT - Precipitable water (mm) for the entire sounding

## RICH - Richardson number in a layer

#### RICH = BFVQ ** 2 / SHRM ** 2

## SEPA - Isentropic pressure thickness in a layer

```
SEPA = pressure difference over a isentropic layer !5 K deep
```
## SHOW - Showalter index


```
SHOW = T500 - Tparcel
T500 = Temperature in Celsius at 500 mb
Tparcel = Temperature in Celsius at 500 mb of a parcel lifted from 850 mb
```
## SHRD - Wind shear direction in a layer

```
SHRD = direction of [ du/dz, dv/dz ]
```
## SHRM - Wind shear magnitude in a layer

```
SHRM = magnitude of [ du/dz, dv/dz ]
```
## STAB - THTA lapse rate in a layer

```
STAB = d (THTA) / dz
```
## STAP - THTA change with pressure in a layer

```
STAP = - d (THTA) / dp
```
## SWET - SWEAT index

#### SWET = 12 * TD850 + 20 * TERM2 + 2 * SKT850 +

#### SKT500 + SHEAR

```
TD850 = Dewpoint in Celsius at 850 mb
TERM2 = MAX ( TOTL - 49, 0 )
TOTL= Total totals index
SKT850 = 850 mb wind speed in knots
SKT500 = 500 mb wind speed in knots
SHEAR = 125 * [ SIN ( DIR500 - DIR850 ) + .2 ]
DIR500 = 500 mb wind direction
DIR850 = 850 mb wind direction
```
```
If TD850 is negative, then TD850 is set to 0.
SHEAR is set to 0 if any of the following
conditions are met:
wind direction at 850mb is < 130 or > 250
wind direction at 500mb is < 210 or > 310
DIR500 - DIR850 <= 0
SPD500 <= 15 or SPD850 <= 15
```
## TOTL - Total Totals index

#### TOTL = ( T850 - T500 ) + ( TD850 - T500 )

```
T850 = Temperature in Celsius at 850 mb
TD850 = Dewpoint in Celsius at 850 mb
```

```
T500 = Temperature in Celsius at 500 mb
```
## VTOT - Vertical Totals index

#### VTOT = T850 - T

```
T850 = Temperature in Celsius at 850 mb
T500 = Temperature in Celsius at 500 mb
```
### CLOUD PARAMETERS

## Cloud coverage may be defined using a cloud code, short code, fractional

## coverage or numeric value. The valid values of these param-

## eters ordered from least to greatest cloud coverage, are:

## The following lists the GEMPAK parameter definitions with an example

## using the sample AIRWAYS cloud report:

## 22SCT 80-BKN 250OVC

## Note that the character x may be replaced by L, M, or H, indicating low,

## mid or high clouds. Also note that the character T indicates

## the value of the parameter at the level of maximum cloud

## coverage.

```
Cloud
Coverage
```
```
Cloud
xCLD
Short
Fractional
xCLO
```
```
Numeric
CLCx
```
```
Symbol
CFRT
```
```
missing - 0.00 0 -
```
```
clear CLR C 0.00 1 0
```
```
thin scattered -SCT -S 0.25 6 2
```
```
scattered SCT S 0.40 2 3
```
```
thin broken -BKN -B 0.60 7 5
```
```
broken BKN B 0.75 3 6
```
```
thin overcast -OVC -O 0.90 8 7
```
```
overcast OVC O 1.00 4 8
```
```
thin obscured -X -X 0.00 9 0
```
```
obscured X X 1.00 5 9
```

## xCLD - Character cloud coverage code

```
Examples: LCLD = SCT
MCLD = -BKN
HCLD = OVC
```
## TCLD - xCLD at maximum cloud coverage

```
Example: TCLD = OVC
```
## xCLO - Fractional cloud coverage

```
Examples: LCLO = 0.
MCLO = 0.
HCLO = 1.
```
## TCLO - xCLO at maximum cloud coverage

```
Example: TCLO = 1.
```
## CLCx - Numeric cloud coverage

```
Examples: CLCL = 2
CLCM = 7
CLCH = 4
```
## CLCT - CLCx at maximum cloud coverage

```
Example: CLCT = 4
```
## The next two parameters combine cloud coverage values from the three

## cloud levels.

## CLDS - Combined cloud coverage short code from three levels

```
Example: CLDS = S-BO
```
## CMBC - Combined cloud coverage numeric from three levels

```
Example: CMBC = 274
```
## The next set of parameters are combined cloud height and cloud coverage.

## CLHx - Cloud height in hundreds of feet

```
Examples: CLHL = 22.
CLHM = 80.
CLHH = 250.
```
## CLDx - Combined cloud height and short code

```
Examples: CLDL = 22S
CLDM = 80-B
CLDH = 250O
```

## CLDT - CLDx at maximum coverage level

```
Example: CLDT = 250O
```
## COMx - Numeric combined cloud height and coverage combined as

```
CLHx * 10 + CLCx
Examples: COML = 222.
COMM = 807.
COMH = 2504
Note: In the case when the sky is partially obscured, the
value of 10000 is added on to the lowest reporting level.
For example, if AIRWAYS report is -X M5 BKN 19 BKN,
COML would equal 10053.
```
## COMT - COMx at maximum coverage level

```
Example: COMT = 2504.
```
## The next two parameters combine the cloud height and coverage allowing

## up to three reports which do not necessarily correspond to

## low, middle and high level clouds. These parameters allow a

## means of storing cloud reports where there may be more than

## one report at a single level.

## CHCx - Numeric combined cloud height and coverage combined as

```
CLHx * 10 + CLCx, where x is the cloud report group
number from 1 to 3.
Examples: CHC1 = 222.
CHC2 = 807.
CHC3 = 2504
Note: In the case when the sky is partially obscured, the
value of 10000 is added on to the first reporting level.
For example, if AIRWAYS report is -X M5 BKN 19 BKN,
CHC1 would equal 10053.
```
## CHDx - Combined cloud height and short code

```
Examples: CHD1 = 22S
CHD2 = 80-B
CHD3 = 250O
```
## The next parameter is the ceiling, defined as the height above the earth's

## surface of the lowest cloud layer that is reported as broken or

## overcast, or the vertical visibility into an indefinite ceiling.

## CEIL - Ceiling in hundreds of feet

## The following set of cloud parameters is the numeric WMO codes which

## are reported by airways data.


## CFRL - Fraction of celestial dome covered by all low and mid

```
level clouds from WMO Code 2700
```
## CTYL - Low-level cloud genera from WMO Code 0513

## CTYM - Mid-level cloud genera from WMO Code 0515

## CTYH - High-level cloud genera from WMO Code 0509

## CBAS - Cloud base height from WMO Code 1600

## CSYL - Cloud graphics symbol for CTYL

## CSYM - Cloud graphics symbol for CTYM

## CSYH - Cloud graphics symbol for CTYH

## CSYT - Cloud graphics symbol for first level reporting clouds

## CFRT - Cloud coverage number from CLCT (maximum clouds)

## SKYC - Cloud coverage graphics symbol for CFRT

## SKYM - Sky coverage symbol with wind barbs in m/s

## SKYK - Sky coverage symbol with wind barbs in knots

## XVFR - Categorical identification of flight rules

## 0 = Low Instrument Flight Rules (LIFR)

## 1 = Instrument Flight Rules (IFR)

## 2 = Marginal Visual Flight Rules (MVFR)

## 3 = Visual Flight Rules (VFR)

## The flight categories and corresponding ceiling and visibility values are

## listed below.

```
Flight CEILING VISIBILITY
Category (feet) (statute miles)
===================================================================
LIFR < 500 ft and/or < 1 SM
IFR >= 500 to < 1,000 and/or >= 1 to < 3
```

```
MVFR >= 1,000 to <= 3,000 and/or >= 3 to <= 5
VFR > 3,000 or none and > 5
===================================================================
```
### WEATHER CODES

## WCOD - Character weather code

## WNUM - Numeric weather code

```
The weather code WCOD may also be accessed as WTHR. The
weather number consists of 3 parts, A, B, C where
WNUM = A * 80 * 80 + B * 80 + C.
Each part corresponds to one of the values:
```
```
0 (no value)
1 R (mod rain) 41 UP(unknown prcp)
2 L (mod drizzle) 42
3 S (mod snow) 43
4 A (mod hail) 44
5 T (thunder) 45
6 H (haze) 46
7 K (smoke) 47
8 D (dust) 48
9 F (fog) 49 ZR- (lt frz rain)
10 Q (squalls) 50 ZR+ (hvy frz rain)
11 V (volcanic ash) 51 RW- (lt rain shwr)
12 52 RW+ (hvy rain shwr)
13 R- (lt rain) 53 ZL- (lt freezing drizzle)
14 R+ (hvy rain) 54 ZL+ (hvy freezing drizzle)
15 ZR (mod frz rain) 55 SW- (lt snow shwr)
16 RW (mod rain shwr) 56 SW+ (hvy snow shwr)
17 L- (lt drizzle) 57 IP- (lt ice pellets)
18 L+ (hvy drizzle) 58 IP+ (hvy ice pellets)
19 ZL (frz drizzle) 59 SG- (lt snow grains)
20 S- (lt snow) 60 SG+ (hvy snow grains)
21 S+ (hvy snow) 61 SP- (lt snow pellets)
22 SW (mod snow shwr) 62 SP+ (hvy snow pellets)
23 IP (mod ice pellet) 63 IPW (mod ice pellet shwr)
24 SG (mod snow grain) 64 IC- (lt ice crystals)
25 SP (mod snow pellet) 65 IC+ (hvy ice crystals)
26 A- (lt hail) 66 TRW (mod thunder shwr)
27 A+ (hvy hail) 67 SPW (snow pellet shwr)
28 T- (lt thunder) 68 BD+ (hvy blowing dust)
29 T+ (hvy thunder) 69 BN+ (hvy blowing sand)
30 IF (ice fog) 70 BS+ (hvy blowing snow)
31 GF (ground fog) 71
32 BS (blowing snow) 72
```

```
33 BD (blowing dust) 73
34 BY (blowing spray) 74
35 BN (blowing sand) 75 IPW- (lt ice pellet shwr)
36 IC (mod ice crystals) 76 IPW+ (hvy ice pellet shwr)
37 IN (ice needles) 77 TRW- (lt rain thunder shwr)
38 AP (small hail) 78 TRW+ (hvy rain thunder shwr)
39 KH (smoke, haze) 79
40 PO (dust whirls)
```
```
The following correspond to a single character code:
```
```
-1 TORNA (tornado) -3 WATER (water spout)
-2 FUNNE (funnel cloud)
```
## WNUM - Numeric weather code, as computed from the METAR codes

```
The weather number consists of 3 parts, A, B, C where
WNUM = A * 80 * 80 + B * 80 + C.
Each part corresponds to one of the values:
```
```
-1 +FC (tornado or -2 FC (funnel cloud) waterspout)
```
```
0 (no value)
1 RA (mod rain) 41 UP (unknown prcp)
2 DZ (mod drizzle) 42
3 SN (mod snow) 43
4 GR (mod hail) 44
5 TS (thunder) 45
6 HZ (haze) 46
7 FU (smoke) 47
8 DU (dust) 48
9 FG (fog) 49 -FZRA (lt frz rain)
10 SQ (squalls) 50 +FZRA (hvy frz rain)
11 VA (volcanic ash) 51 -SHRA (lt rain shwr)
12 52 +SHRA (hvy rain shwr)
13 -RA (lt rain) 53 -FZDZ (lt frz drizzle)
14 +RA (hvy rain) 54 +FZDZ (hvy frz drizzle)
15 FZRA (mod frz rain) 55 -SHSN (lt snow shwr)
16 SHRA (mod rain shwr) 56 +SHSN (hvy snow shwr)
17 -DZ (lt drizzle) 57 -PL (lt ice pellets)
18 +DZ (hvy drizzle) 58 +PL (hvy ice pellets)
19 FZDZ (frz drizzle) 59 -SG (lt snow grains)
20 -SN (lt snow) 60 +SG (hvy snow grains)
21 +SN (hvy snow) 61 -GS (lt snow pellets)
22 SHSN (mod snow shwr) 62 +GS (hvy snow pellets)
23 PL (mod ice pellet) 63SHPL (mod ice pellet shwr)
24 SG (mod snow grain) 64
25 GS (mod snow pellet) 65
26 66TSRA (mod thunder shwr)
27 SHGR (hvy hail) 67SHGS (snow pellet shwr)
28 68 +BLDU (hvy blowing dust)
29 69 +BLSA (hvy blowing sand)
```

```
30 FZFG (ice fog) 70 +BLSN (hvy blowing snow)
31 BR (ground fog) 71
32 BLSN (blowing snow) 72
33 BLDU (blowing dust) 73
34 BLPY (blowing spray) 74
35 BLSA (blowing sand) 75 -SHPL (lt ice pellet shwr)
36 IC (mod ice crystals) 76 +SHPL (hvy ice pellet shwr)
37 77 -TSRA (lt rain thunder shwr)
38 78 +TSRA (hvy rain thunder shwr)
39 79
40 PO (dust whirls)
```
## WTMO - Character WMO weather code

## WWMO - Numeric WMO weather code

## WSYM - Graphics weather symbol corresponding to WWMO

```
The transformation is:
```
#### 0 = 34 = BD+ 67 = ZR

#### 1 = 35 = BD+ 68 = R-S-

#### 2 = 36 = BS 69 = RS

#### 3 = 37 = BS+ 70 = S-

#### 4 = K 38 = BS 71 = S-

#### 5 = H 39 = BS+ 72 = S

#### 6 = D 40 = 73 = S

#### 7 = BD 41= F 74 = S+

#### 8 = PO 42 = F 75 = S+

#### 9 = 43 = F 76 = IN

#### 10 = F 44 = F 77 = SG

#### 11 = GF 45 = F 78 = IC

#### 12 = GF 46 = F 79 = IP

#### 13 = 47 = F 80 = RW-

#### 14 = 48 = IF 81 = RW

#### 15 = 49 = IF 82 = RW+

#### 16 = 50 = L- 83 = RW-SW-

#### 17 = T 51 = L- 84 = RWSW

#### 18 = Q 52 = L 85 = SW-

#### 19 = FUNNEL 53 = L 86 = SW

#### 20 = 54 = L+ 87 = IPW-

#### 21 = 55 = L+ 88 = IPW

#### 22 = 56 = ZL- 89 = A-

#### 23 = 57 = ZL 90 = A

#### 24 = 58 = R-L- 91 = R-

#### 25 = 59 = RL 92 = R

#### 26 = 60 = R- 93 = RS

#### 27 = 61 = R- 94 = R+S+

#### 28 = 62 = R 95 = TRW-

#### 29 = 63 = R 96 = TRW-A

#### 30 = BD 64 = R+ 97 = TRW+


#### 31 = BD 65 = R+ 98 = TD

#### 32 = BD 66 = ZR- 99 = TRW+A

#### 33 = BD+

#### 105 = TSW- 107 = TSW+

#### 201 = V 202 = BY 203 = UP

## PWTH - Character past weather WMO code or graphics symbol for it

## PWWM - Numeric past weather WMO code

```
The past weather WMO numeric codes are:
```
```
0 = Cloud covering less than 1/2 sky
1 = Cloud covering more than 1/2 during part of
period and less than 1/2 sky during part
2 = Cloud covering more than 1/2 sky
3 = Sandstorm, duststorm or blowing snow
4 = Fog, ice fog, thick haze or thick smoke
5 = Drizzle
6 = Rain
7 = Snow, rain and snow mixed or ice pellets
8 = Showers
9 = Thunderstorm with or without precipitation
```
```
The translation is:
```
#### 0 = 5 = L

#### 1 = 6 = R

#### 2 = 7 = S

#### 3 = BD 8 = RW

#### 4 = F 9 = T

### STATION PARAMETERS

## STID - Character station identifier

## STNM - Station number

```
STNM is the 5-digit WMO identifier for upper air data; 6
digits for surface data, usually the WMO identifier with a
zero appended.
```
## SLAT - Station latitude in degrees


## SLON - Station longitude in degrees; West longitude is negative

## SELV - Station elevation in meters

## RANG - Range in kilometers (specialized use)

## AZIM - Azimuth in kilometers (specialized use)

## LATI - Latitude in degrees from range/azimuth

## LONG - Longitude in degrees from range/azimuth

```
LATI and LONG are calculated from the RANG and AZIM using
equations developed for AOIPS/RADPAK.
```
## DELT - Delta time in seconds (specialized use)

### MODEL OUTPUT STATISTICS

## MXMN - Maximum or minimum temperature in Fahrenheit

## TNTF - Night temperature fcst in Fahrenheit

## TNCF - Night temperature climatology in Fahrenheit

## TNAF - Night temperature anomaly in Fahrenheit

## TDYF - Day temperature fcst in Fahrenheit

## TDCF - Day temperature climatology in Fahrenheit

## TDAF - Day temperature anomaly in Fahrenheit

## TCNT - Night cloud coverage fcst

## CL12 - Prevailing total sky cover fcst for a 12-hr period

## 1 = CL = mostly clear

## 4 = OV = mostly overcast

## 7 = PC = mixed clouds and clear skies


## TCDY - Day cloud coverage fcst

## SKNN - Night wind speed fcst in knots

## SKND - Day wind speed fcst in knots

## SK12 - Maximum sustained surface wind speed fcst for a 12-hr period

```
5 = light = 0 - 12 knots
15 = moderate = 13 - 21 knots
25 = strong = 22 - 33 knots
40 = high = greater than or equal to 34 knots
```
## PP06 - Probability of precipitation fcst in a 6-hr period

## PP12 - Probability of precipitation fcst in a 12-hr period

## PP1C - Probability of precipitation climatology in a 12-hr period

## PP1A - Probability of precipitation anomaly in a 12-hr period

## PPNT - Probability of precipitation fcst for night

## PPNC - Probability of precipitation climatology for night

## PPNA - Probability of precipitation anomaly for night

## PPDY - Probability of precipitation fcst for day

## PPDC - Probability of precipitation climatology for day

## PPDA - Probability of precipitation anomaly for day

## PP24 - Probability of precipitation fcst in a 24-hr period

## PP2C - Probability of precipitation climatology in a 24-hr period

## PP2A - Probability of precipitation anomaly in a 24-hr period


## QP06 - Quantitative precipitation fcst in a 6-hr period

```
1 = 0.01 - 0.09 inches
2 = 0.10 - 0.24 inches
3 = 0.25 - 0.49 inches
4 = 0.50 - 0.99 inches
5 = 1.00 - 1.99 inches
```
## QPX2 - Maximum amount of precipitation in inches fcst in a 12-hr period.

## Values are same as QP12.

## QP12 - Quantitative precipitation fcst in a 12-hr period

```
0 = no precipitation expected
1 through 5 same as QP06
6 = greater than or equal to 2.00 inches
```
## QP24 - Quantitative precipitation fcst in a 24-hr period

```
0 through 5 same as QP12
6 = 2.00 - 2.99 inches
7 = greater than or equal to 3.00 inches
```
## TS06 - Unconditional probability of thunderstorms occurring in a 6-hr

## period

## TS12 - Unconditional probability of thunderstorms occurring in a 12-hr

## period

## TS24 - Unconditional probability of thunderstorms occurring in a 24-hr

## period

## TC06 - Conditional probability of severe weather occurring in a 6-hr

## period

## TC12 - Conditional probability of severe weather occurring in a 12-hr

## period

## PCPT - Categorical forecast of precipitation

```
0=R=rain
1=S=snow
2=Z=freezing
```
## POZP - Conditional probability of freezing precipitation

## (not included during the warm season)


## POSN - Conditional probability of snow

## (not included during the warm season)

## PSNT - Conditional probability of snow for night

## (not included during the warm season)

## PSDY - Conditional probability of snow for day

## (not included during the warm season)

## SN06 - Categorical forecast of snow amount falling in a 6-hr period

```
0 = no snow
1 = trace - less than 2 inches
2 = greater than or equal to 2 inches
```
## SN12 - Categorical forecast of snow amount falling in a 12-hr period

```
0 and 1 same as SN06
2=2toless than 4 inches
4=4toless than 6 inches
6 = greater than 6 inches
```
## (not included during the warm season)

## SN24 - Categorical forecast of snow amount falling in a 24-hr period

```
0 = no snow or a trace
1 = greater than a trace to less than 2 inches
2=2toless than 4 inches
4=4toless than 6 inches
6=6toless than 8 inches
8 = greater than 8 inches
```
## (not included during the warm season)

## PZ12 - Conditional probability of freezing precipitation in a 12-hr period

## PS12 - Conditional probability of snow in a 12-hr period

## PR12 - Conditional probability of mixed liquid/frozen precipitation in a

## 12-hr period

## PC12 - Categorical forecast of precipitation type in a 12-hr period

```
0=R=liquid
1=S=frozen
2=Z=freezing
3 = RS = mixed liquid and frozen precipitation
4=RZ
5=SZ
6 = RSZ
```

## FCIG - Categorical forecast of ceiling height conditions

```
1 = less than 200 feet
2 = 200 - 400 feet
3 = 500 - 900 feet
4 = 1000 - 3000 feet
5 = 3100 - 6500 feet
6 = 6600 - 12,000 feet
7 = greater than 12,000 feet
```
## FVIS - Categorical forecast of visibility conditions

```
1 = less than 0.5 miles
2 = 0.5 - 0.875 miles
3 = 1.0 - 2.75 miles
4 = 3.0 - 5.0 miles
5 = greater than 5.0 miles
```
## FVSA - Categorical forecast of visibility conditions (for new MOS)

```
1 = less than 0.25 miles
2 = 0.25 to less than .5 mile
3 = .5 mile to less than 1.0 mile
4 = 1.0 to less than 3.0 miles
5 = 3.0 to 5.0 miles
6 = 6.0 miles
7 = greater than 6.0 miles
```
## OVIS - Categorical forecast in plain language of obstructions to vision

```
0=N=none of the following:
1 = FG or F = fog or ground fog (vis. less than .625 mile)
2 = HZ or H = haze, smoke, dust
3 = BR = mist (fog with visibility greater than .625 mile)
4 = BL = blowing dust, sand, snow
```
## WXPB - Categorical weather precipitation probability or areal coverage

## determined by the precipitation parameter having the highest

## probability or areal coverage in WNUM.

```
For probability:
1 = slight chance
2 = chance
3 = likely
4 = occasional
5 = definite
```
```
For areal coverage:
1 = isolated
2 = widely scattered
3 = scattered
4 = numerous
```

```
5 = widespread
```
### TERMINAL AERODROME FORECAST (TAF) PARAMETERS

## TDRC - Temporary/probability wind direction in degrees

## TSKN - Temporary/probability wind speed in knots

## TGST - Temporary/probability wind gusts in knots

## BRGK - Gust barb feathered in knots

## TCHx - Temporary/probability numeric combined cloud height and cov-

## erage, as for CHCx

## TCEL - Temporary/probability ceiling in hundreds of feet, as for CEIL

## TSKC - Temporary/probability cloud coverage graphics symbol, as for

## SKYC

## TXVF - Temporary/probability categorical identification of flight rules, as

## for XVFR

## TWNM - Temporary/probability numeric weather code, as for WNUM

## TWSY - Temporary/probability graphics weather symbol corresponding

## to TWNM, as for WSYM

## TVSB - Temporary/probability visibility in statute miles

## PPRB - Probability for TAF forecast change indicator

```
30 = PROB30 - 30 percent probability condition
40 = PROB40 - 40 percent probability condition
50 = TEMPO - temporary condition
```
## VWNM - Vicinity numeric weather code, as for WNUM

## VWSY - Vicinity graphics weather symbol corresponding to VWNM, as


## for WSYM

## TVWN - Temporary/probability vicinity numeric weather code, as for

## WNUM

## WSKC - Worst case cloud coverage graphics symbol, as for SKYC

## WXVF - Worst case categorical identification of flight rules, as for XVFR

## TPWN - Temporary/probability/vicinity numeric weather code, as for

## WNUM

## TPWS - Temporary/probability/vicinity graphics weather symbol corre-

## sponding to TPWN, as for WSYM

## AWNM - Prevailing/temporary/probability/vicinity numeric weather

## code, as for WNUM

## AWSY - Prevailing/temporary/probability/vicinity graphics weather sym-

## bol corresponding to AWNM, as for WSYM

## LLWS - Low level wind shear forecast flag

## MOTV - Mountain obscuration threshold value in hundreds of feet

## CMSL - Ceiling converted to mean sea level in hundreds of feet

## MOBS - Mountain obscuration threshold met indicator

## TCMS - Temporary/probability ceiling converted to mean sea level in

## hundreds of feet

## TMOB - Temporary/probability mountain obscuration threshold met indi-

## cator

## WCMS - Worst case ceiling converted to mean sea level in hundreds of

## feet

## WMOB - Worst case mountain obscuration threshold met indicator


### MARINE PARAMETERS

## WHGT - Wave height in meters

## WHFT - Wave height in feet

## WPER - Wave period in seconds

## HOWW - Height of wind wave in meters

## POWW - Period of wind wave in seconds

## HOSW - Height of predominant swell wave in meters

## POSW - Period of predominant swell wave in seconds

## DOSW - Direction of predominant swell wave in degrees

## HOS2 - Height of secondary swell wave in meters

## POS2 - Period of secondary swell wave in seconds

## DOS2 - Direction of secondary swell wave in degrees

## WAV2 - Combined wind wave period and height in feet ("2 group")

## WAV3 - Combined predominant and secondary swell wave direction in

## tens of degrees ("3 group")

## WAV4 - Combined predominant swell wave period and height in feet

## ("4 group")

## WAV5 - Combined secondary swell wave period and height in feet

## ("5 group")

## WPHM - Combined wave period and height in half meters

## WVSW - Combined swell wave direction, period and height in half

```
meters
```
## SWEL - Character combined swell wave direction, period and


```
height in half meters
```
## DAWV - Swell wave direction arrows of uniform length

## IDTH - Thickness of ice on ship in meters

## ROIA - Rate of ice accretion on ship from WMO Code 3551

```
0 = Ice not building up
1 = Ice building up slowly
2 = Ice building up rapidly
3 = Ice melting or breaking up slowly
4 = Ice melting or breaking up rapidly
```
## IGRO - Rate of ice accretion on vessel in salt water in inches per three

## hours

#### IGRO = ( A*PR + B*PR*PR + C*PR*PR*PR ) * CVFAC

#### A = 2.73 * 10E-2

#### B = 2.91 * 10E-4

#### C = 1.84 * 10E-6

#### PR = ( SPED * ( -1.7 - TMPC ) ) /

#### ( 1 + 0.4 * ( SSTC + 1.7 ) )

```
(priesendorfer regression)
CVFAC = 1.1811, to convert cm/hr to in/3hr
```
## DIGR - Character rate of ice accretion in inches per three hours

## SHPD - True direction from which ship is moving (for 3 hours before obs)

## in degrees

## SHPK - Ship's average speed (for 3 hours before obs) in knots

## DASH - Ship's true direction arrows of uniform length


### AIRCRAFT PARAMETERS

## TURB - Amount of turbulence

```
0 = No turbulence
2 = Light turbulence
3 = Light to moderate turbulence
4 = Moderate turbulence
5 = Moderate to severe turbulence
6 = Severe turbulence
8 = Extreme turbulence
```
## TBSE - Base of turbulence in feet

## TTOP - Top of turbulence in feet

## HBOT - Base of turbulence in meters

## HTOT - Top of turbulence in meters

## FQOT - Frequency of turbulence

```
1 = Occasional
2 = Intermittent
3 = Continuous
```
## TPOT - Type of turbulence

```
1 = Clear air turbulence
2 = Chop
3 = Low level wind shear
4 = Turbulence in cloud
```
## TBSY - Graphics symbol for turbulence

## ICNG - Amount of airframe icing

```
0 = No icing
1 = Trace icing
2 = Trace to light icing
3 = Light icing
4 = Light to moderate icing
5 = Moderate icing
7 = Moderate to severe icing
8 = Severe icing
```
## IBSE - Base of icing in feet

## ITOP - Top of icing in feet


## HBOI - Base of icing in meters

## HTOI - Top of icing in meters

## TPOI - Type of icing

```
1 = Rime
2 = Clear
3 = Mixed
4 = Rime in cloud
5 = Clear in cloud
6 = Mixed in cloud
7 = Rime in precipitation
8 = Clear in precipitation
9 = Mixed in precipitation
10 = Frost
11 = Non-persistent contrails
12 = Persistent contrails
```
## ICSY - Graphics symbol for icing

## WBSE - Base of weather in feet

## WTOP - Top of weather in feet

## HBWX - Base of weather in meters

## HTWX - Top of weather in meters

## CLC1 - Numeric cloud coverage 1

## CBS1 - Cloud base 1 in feet

## CTP1 - Cloud top 1 in feet

## CB1M - Cloud base 1 in meters

## CT1M - Cloud top 1 in meters

## CLC2 - Numeric cloud coverage 2

## CBS2 - Cloud base 2 in feet

## CTP2 - Cloud top 2 in feet


## CB2M - Cloud base 2 in meters

## CT2M - Cloud top 2 in meters

## ACRT - Aircraft report type

```
1 = AIREP - Aircraft report
2 = PIREP - Pilot report
3 = RECCO - Reconnaissance flight report
4 = AMDAR - Aircraft report (aircraft meteorological data relay)
```
## SELV - Flight level in meters

## FELV - Flight level in hundreds of feet

## ITSY - Icing type symbol

## TTSY - Turbulence type symbol

## TFSY - Turbulence frequency symbol

## ACTP - Character aircraft type

## ATP1 - Numeric aircraft type

```
The numeric aircraft type is a real representation of
up to four characters from the character aircraft type.
ATP1 = v4 * 40 * 40 * 40 + v3 * 40 * 40 + v2 * 40 + v1.
Each character corresponds to one of the values:
```
#### 1 = - 9 = 5 17 = D 25 = L 33 = T

#### 2 =. 10 = 6 18 = E 26 = M 34 = U

#### 3 = / 11 = 7 19 = F 27 = N 35 = V

#### 4 = 0 12 = 8 20 = G 28 = O 36 = W

#### 5 = 1 13 = 9 21 = H 29 = P 37 = X

#### = 2 14 = A 22 = I 30 = Q 38 = Y

#### 7 = 3 15 = B 23 = J 31 = R 39 = Z

#### 8 = 4 16 = C 24 = K 32 = S

```
Any character not defined above is treated as a /.
```

### MISCELLANEOUS PARAMETERS

## VSBY - Visibility in statute miles

## VSBK - Visibility in kilometers

## VSBN - Visibility in nautical miles

## VSBF - Character visibility in fractions of statute miles

## VSBC - Character visibility in fractions of statute miles for all visibility

## numbers

## PnnI - Precipitation over last nn hours in inches

```
nn = 01, 03, 06, 09, 12, 18 or 24
```
## PnnM - Precipitation over last nn hours in millimeters

```
nn = 01, 03, 06, 09, 12, 18 or 24
```
## DPRC - Character daily weather map precipitation in inches

## PR24 - Precipitation over last 24 hours in inches, as sum of four succes-

## sive 6-hour precip amounts

## SNOW - Snow depth in inches

## SNEW - Amount of new snow in inches

## SNRT - Forecast snow and ice pellet accumulation to watch threshold

## ratio

## SI12 - Forecast snow and ice pellet 12-h accumulation in inches

## SNIP - Snow and ice pellet watch threshold in inches

## FZRT - Forecast freezing rain accumulation to watch threshold ratio

## FZ12 - Forecast Freezing rain 12-h accumulation in inches


## FZRN - Freezing rain watch threshold in inches

## WEQS - Water equivalent of snow on the ground in inches

## HAIL - Hail flag

## HLSZ - Hail size in centimeters

## DDEN - Density of dry air in kg/(m**3)

## PSYM - Montgomery stream function in m**2/(100*s**2)

## HEAT - Heat index in Fahrenheit

## HMTR - Humiture (apparent temperature) in Fahrenheit

## WCEQ - Wind chill equivalent temperature in Fahrenheit

## WCHT - Revised wind chill temperature in Fahrenheit

## MSUN - Duration of sunshine in minutes

## FFnn - Flash flood guidance for next nn hours in inches

```
nn = 01, 03, 06, 12 or 24
```
## ITSO - Indicator for type of station operation and for present and past

## weather

```
Operation Present and past weather data
```
```
1 = manned included
2 = manned omitted (no significant phenomena)
3 = manned omitted (no observation, data N/A)
4 = automatic included, using WMO Code 4677 and 4561
5 = automatic omitted (no significant phenomena)
6 = automatic omitted (no observation, data N/A)
7 = automatic included, using WMO Code 4680 and 4531
```
## TOST - Type of station (manned or automatic)

```
0 = automatic
1 = manned
```

## STIM - Report hour and minutes as hhmm

## TEXT - Undecoded data

## SPCL - Undecoded special reports

## MARK - Markers

## FOSB - Fosberg Index, also called Fire Weather Index

### SPACING PARAMETERS

## BLNK - Plot a blank, not accounted for in FILTER

## SPAC - Plot a space, accounted for in FILTER



