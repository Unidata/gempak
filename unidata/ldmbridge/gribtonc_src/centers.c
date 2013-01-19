/*
 *	Copyright 1992 University Corporation for Atmospheric Research
 *	   Not for Direct Resale. All copies to include this notice.
 */
/* $Id: centers.c,v 1.1 1995/02/23 23:56:06 russ Exp $ */

#include "centers.h"

char *
centername(ii)
{
	switch(ii){
	  case CENTER_NMC:	
	    return "US Weather Service - National Met. Center";
	  case CENTER_NWSTG:
	    return "US NWS Telecomms Gateway";
	  case CENTER_NWSFS:
	    return "US NWS Field Stations";
	  case CENTER_JMA:
	    return "Japanese Meteorological Agency - Tokyo";
	  case CENTER_NHC:
	    return "US National Hurricane Center, Miami";
	  case CENTER_CMS:
	    return "Canadian Meteorological Service - Montreal";
	  case CENTER_USAF:
	    return "US Air Force - Global Weather Center";
	  case CENTER_FNOC:
	    return "US Navy  - Fleet Numerical Oceanography Center";
	  case CENTER_FSL:
	    return "NOAA Forecast Systems Lab, Boulder CO";
	  case CENTER_UKMET:
	    return "U.K. Met Office - Bracknell";
	  case CENTER_FR:
	    return "French Weather Service - Toulouse";
	  case CENTER_ESA:
	    return "European Space Agency (ESA)";
	  case CENTER_ECMWF:
	    return "European Center for Medium-Range Weather Forecasts";
	  case CENTER_NL:
	    return "De Bilt, Netherlands";

				/* National subcenters */
	  case CENTER_REANA:
	    return "NMC Re-Analysis Project";
	  case CENTER_ABRFC:
	    return "ABRFC - Arkansas-Red River RFC, Tulsa OK";
	  case CENTER_AKFC:
	    return "Alaska RFC, Anchorage, AK";
	  case CENTER_CBRFC:
	    return "CBRFC - Colorado Basin RFC, Salt Lake City, UT";
	  case CENTER_CNRFC:
	    return "CNRFC - California-Nevada RFC, Sacramento, CA";
	  case CENTER_LMRFC:
	    return "LMRFC - Lower Mississippi RFC, Slidel, LA";
	  case CENTER_MARFC:
	    return "MARFC - Middle Atlantic RFC, State College, PA";
	  case CENTER_MBRFC:
	    return "MBRFC - Missouri Basin RFC, Kansas City, MO";
	  case CENTER_NCRFC:
	    return "NCRFC - North Central RFC, Minneapolis, MN";
	  case CENTER_NERFC:
	    return "NERFC - Northeast RFC, Hartford, CT";
	  case CENTER_NWRFC:
	    return "NWRFC - Northwest RFC, Portland, OR";
	  case CENTER_OHRFC:
	    return "OHRFC - Ohio Basin RFC, Cincinnati, OH";
	  case CENTER_SERFC:
	    return "SERFC - Southeast RFC, Atlanta, GA";
	  case CENTER_WGRFC:
	    return "WGRFC - West Gulf RFC, Fort Worth, TX";
	  case CENTER_OUN:
	    return "OUN   - Norman OK WFO";
	}
	/* default */
	return "unknown";
}
