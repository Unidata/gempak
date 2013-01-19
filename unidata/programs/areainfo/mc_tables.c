/* $Id: mc_tables.c,v 1.2 90/01/08 22:16:45 davis Exp $ */

/*
 *  From McIDAS Reference Manual
 *     Introduction - pg 21
 *     Table 1, UD-14 9/88,
 *
 * Given the "SS Code" (word three of the area directory)
 *   return a string describing the sensor source.
 *
 */
char *
mc_sensor(sscode)
unsigned long sscode ;
{
	switch (sscode) {
	case 0 : return("Non-Image Derived Data") ;
	case 1 : return("Test patterns") ;
	case 2 : return("SSEC Graphics") ;
	case 3 : return("Miscellaneous") ;
	case 4 : return("PDUS Meteosat Visible") ;
	case 5 : return("PDUS Meteosat Infrared") ;
	case 6 : return("PDUS Meteosat Water Vapor") ;
	case 7 : return("Radar") ;
	case 8 : return("Miscellaneous Aircraft Data (MAMS)") ;
	case 12 : return("GMS Visible") ;
	case 13 : return("GMS Infrared") ;
	case 14 : return("ATS 6 Visible") ;
	case 15 : return("ATS 6 Infrared") ;
	case 16 : return("SMS-1 Visible") ;
	case 17 : return("SMS-1 Infrared") ;
	case 18 : return("SMS-2 Visible") ;
	case 19 : return("SMS-2 Infrared") ;
	case 20 : return("GOES-1 Visible") ;
	case 21 : return("GOES-1 Infrared") ;
	case 22 : return("GOES-2 Visible") ;
	case 23 : return("GOES-2 Infrared") ;
	case 24 : return("GOES-3 Visible") ;
	case 25 : return("GOES-3 Infrared") ;
	case 26 : return("GOES-4 Visible (VAS)") ;
	case 27 : return("GOES-4 Infrared and Water Vapor (VAS)") ;
	case 28 : return("GOES-5 Visible (VAS)") ;
	case 29 : return("GOES-5 Infrared and Water Vapor (VAS)") ;
	case 30 : return("GOES-6 Visible") ;
	case 31 : return("GOES-6 Infrared") ;
	case 32 : return("GOES-7 Visible") ;
	case 33 : return("GOES-7 Infrared") ;
	case 34 :
	case 36 :
	case 37 :
	case 38 :
	case 39 :
	case 40 : return("NOAA Series Satellites") ;
	case 41 : return("TIROS-N") ;
	case 42 : return("NOAA-6") ;
	case 43 : return("NOAA-7") ;
	case 44 : return("NOAA-8") ;
	case 45 : return("NOAA-9") ;
	case 46 : return("Venus") ;
	case 47 : return("Voyager 1") ;
	case 48 : return("Voyager 2") ;
	case 50 : return("Hubble St.") ;
	case 60 : return("NOAA-10") ;
	case 61 : return("NOAA-11") ;
	case 70 : return("GOES-I (IMAGER)") ;
	case 71 : return("GOES-I (SOUNDER)") ;
	case 72 : return("GOES-J (IMAGER)") ;
	case 73 : return("GOES-J (SOUNDER)") ;
	case 74 : return("GOES-K (IMAGER)") ;
	case 75 : return("GOES-K (SOUNDER)") ;
	case 76 : return("GOES-L (IMAGER)") ;
	case 77 : return("GOES-L (SOUNDER)") ;
	case 78 : return("GOES-M (IMAGER)") ;
	case 79 : return("GOES-M (SOUNDER)") ;
	case 80 : return("ERBE") ;
	case 90 : return("RAW METEOSAT") ;
	case 180 : return("GOES-N (IMAGER)") ;
	case 181 : return("GOES-N (SOUNDER)") ;
	case 182 : return("GOES-O (IMAGER)") ;
	case 183 : return("GOES-O (SOUNDER)") ;
	case 184 : return("GOES-P (IMAGER)") ;
	case 185 : return("GOES-P (SOUNDER)") ;
	case 186 : return("GOES-Q (IMAGER)") ;
	case 187 : return("GOES-Q (SOUNDER)") ;
	}
	return("Unknown") ;
}
