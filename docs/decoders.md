# GEMPAK Decoders

* [dcacars](decoders/dcacars) is a decoder for **NetCDF ACARS data files from NOAA/FSL**, and writes aircraft reports of altitude, temperature, wind speed, direction and RH (reported by a few aircraft) into GEMPAK surface ship format files. 
 
* [dcacft](decoders/dcacft) decodes **raw AIREP, PIREP, RECCO and AMDAR reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcairm](decoders/dcairm) decodes **AIRMET reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dccosmic](decoders/dccosmic) decodes **NetCDF format COSMIC data** from UCAR. COSMIC (Constellation of Observing System for Meteorology, Ionosphere and Climate) data come from a series of US/JPL satellites used for GPS-based atmospheric soundings of refractivity and bending angle based on radio occultation.

* [dccsig](decoders/dccsig) decodes **convective sigmet and convective outlook reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcffa](decoders/dcffa) decodes **flash flood watch reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file. Only flash flood watch reports with VTEC lines will be decoded. 

* [dcffg](decoders/dcffg) decodes **flash flood guidance data** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file. 

* [dcgmos](decoders/dcgmos) decodes **Global Forecast System (GFS) MOS data** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcgrib](decoders/dcgrib) decodes **GRIB format grids** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK grid file. 

* [dcgrib2](decoders/dcgrib2) decodes **GRIB2 format grids** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to GEMPAK grid files.

* [dchrcn](decoders/dchrcn) decodes **forecast/advisory reports for tropical depressions, tropical storms and hurricanes** for the Atlantic and Pacific Oceans from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file. 

* [dcidft](decoders/dcidft) This program decodes **sea ice reports** and writes the output to a GEMPAK surface file.
                              
* [dcigdr](decoders/dcigdr) decodes **IGDR data in BUFR format** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file. 

* [dcisig](decoders/dcisig) decodes **international SIGMET reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dclsfc](decoders/dclsfc) decodes **land surface synoptic reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file. 

* [dcmetr](decoders/dcmetr) decodes **raw SAO and METAR reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file. 

* [dcmsfc](decoders/dcmsfc) decodes raw buoy, ship, C-MAN, and Coast Guard reports from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcncon](decoders/dcncon) decodes **non-convective SIGMET reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcncprof](decoders/dcncprof) decodes **NetCDF format profiler and RASS reports** provided by NOAA/FSL from a real-time data feed through standard input, or a NetCDF file on disk, and writes the data to a GEMPAK merged upperair file. FSL profiler and RASS data is provided in 6 minute observations as well as hourly summaries (currently available on the Unidata IDD FSL2 feed). 

* [dcnexr2](decoders/dcnexr2) decodes **NEXRAX Level 2 products** in the NEXRAD2 (or CRAFT) feed, received in BZIP2 compressed pieces from a stream fed to the program through standard input, and appended to an output file for use with GEMPAK display programs. 

* [dcnldn](decoders/dcnldn) decodes **NLDN lightning data** reports from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK ship format file.

* [dcnmos](decoders/dcnmos) decodes **NGM Model Output Statistics** data from a a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcprof](decoders/dcprof) decodes **BUFR format profiler reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK merged upperair file.

* [dcrdf](decoders/dcrdf) decodes **Regional Digital Forecast (RDF) reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcreanal](decoders/dcreanal) will convert a **NCAR/NCEP reanalysis Netcdf data** into a GEMPAK gridded data file. COARDS data conventions are assumed.

* [dcredbook](decoders/dcredbook) (and *dcredbook_gf*, *dcredbook_gif*, *dcredbook_vg*, *dcredbook_ps*) creates displays of **Redbook graphic format products** from a real-time data feed, or from a file fed to the program through standard input.

* [dcscd](decoders/dcscd) decodes **Supplemental Climatological Data reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dcshef](decoders/dcshef) decodes **raw SHEF reports** from a real-time data feed (via an LDM), or from a file containing raw SHEF reports. The data is written to a GEMPAK surface file, or a BUFR file and an ASCI listing file based on the options provided by the user.

* [dcstorm](decoders/dcstorm) decodes **Severe Storm reports from the SPC** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK ship format file. 

* [dcsuomi](decoders/dcsuomi) is a decoder for use with the NetCDF format **SUOMINET data files** from Unavco/Unidata. places the individual GPS & Metstation reports into GEMPAK surface format files.

* [dcsvrl](decoders/dcsvrl) decodes **severe local storm reports** (tornado and severe thunderstorm watch reports) from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file. 

* [dctaf](decoders/dctaf) decodes raw **TAF (terminal aerodrome forecast) reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file.

* [dctama](decoders/dctama) decodes **TAMDAR data in BUFR format** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK sounding file.

* [dctrop](decoders/dctrop) decodes **Hurricane/Tropical storm reports reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK ship format file.

* [dcuair](decoders/dcuair) decodes **upper air sounding data** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK sounding file.

* [dcuspln](decoders/dcuspln) decodes **USPLN lightning data reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK ship format file.

* [dcwarn](decoders/dcwarn) decodes **flash flood, tornado and severe thunderstorm warning reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcwatch](decoders/dcwatch) decodes **WWUS40 format Severe Thunderstorm and Tornado watch box bulletins** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK ship format file.

* [dcwcn](decoders/dcwcn) decodes **watch county notification reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcwcp](decoders/dcwcp) decodes **tornado and severe thunderstorm** Watch Corner Points (WCP) reports from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcwmostrip](decoders/dcwmostrip) strips the wmo header from a GIF, PNG, or FAX/G3 graphics file. The header is considered to be anything before the magic words `GIF`, `\211PNG`, and `DFAX`. The program takes two command line entries: 
    
        wmostrip input_file output_file
        
    Where input_file is the file with the WMO header before the graphic data, and output_file is the file to be created without the header.

* [dcwou](decoders/dcwou) decodes **watch outline update reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcwstm](decoders/dcwstm) decodes **winter storm reports** from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcwtch](decoders/dcwtch) decodes **tornado and severe thunderstorm watch box reports** and watch status reports from a real-time data feed, or from a file fed to the program through standard input, and writes the data to an ASCII file.

* [dcxmos](decoders/dcxmos) decodes **Global Forecast System Extended** (GFSX) MOS data from a real-time data feed, or from a file fed to the program through standard input, and writes the data to a GEMPAK surface file. 
