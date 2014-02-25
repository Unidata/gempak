/*
 *	Copyright 1994, University Corporation for Atmospheric Research.
 *	All copies to include this notice.
 */
/* $Id: models.c,v 1.3 1995/10/04 16:40:55 russ Exp $ */

/* GRIB models */

#include <stdio.h>
#include <string.h>
#include "centers.h"
#include "models.h"

char *
modelname(center, model)
{
    switch(center) {
    case CENTER_ECMWF:
	switch(model) {		/* does anyone know names for these? */
	case MODEL_T213:
	    return "t213 31-level forecast model";
	default: {
	    static char ecmwf_model[] = "ECMWF model xxxxxxxxxxxx";
	    
	    sprintf(ecmwf_model, "ECMWF model %d", model);
	    return ecmwf_model;
	    }
	}
    case CENTER_NMC:
	switch(model){
	case MODEL_UVPI:
	    return "Ultra Violet Potential Index Model";
	case MODEL_SAT:
	    return "Satellite Derived Precipitation and temperatures, from IR";
	case MODEL_SWF:
	    return "Global Wind-Wave Forecast Model";
	case MODEL_LFM:
	    return "Limited-area Fine Mesh (LFM)";
	case MODEL_SCA:
	    return "Snow Cover Analysis";
	case MODEL_NGM:
	    return "Nested Grid forecast Model (NGM)";
	case MODEL_GOI:
	    return "Global Optimum Interpolation Analysis (GOI)";
	case MODEL_FGOI:
	    return "Final Global Optimum Interpolation Analysis (GOI)";
	case MODEL_SST:
	    return "Sea Surface Temperature Analysis";
	case MODEL_LFM4:
	    return "LFM-Fourth Order Forecast Model";
	case MODEL_ROI:
	    return "Regional Optimum Interpolation Analysis";
	case MODEL_AVN:
	    return "80 Wave, 18 Layer Spectral Model Aviation Run";
	case MODEL_MRF:
	    return "80 Wave, 18 Layer Spectral Model Medium Range Forecast";
	case MODEL_QLM:
	    return "Quasi-Lagrangian Hurricane Model (QLM)";
	case MODEL_FOG:
	    return "Fog Forecast model - Ocean Prod. Center";
	case MODEL_GMW:
	    return "Gulf of Mexico Wind/Wave";
	case MODEL_GAW:
	    return "Gulf of Alaska Wind/Wave";
	case MODEL_MRFB:
	    return "Bias Corrected Medium Range Forecast";
	case MODEL_AVN1:
	    return "126 Wave, 28 Layer Spectral Model Aviation Run";
	case MODEL_MRF1:
	    return "126 Wave, 28 Layer Spectral Model Medium Range Forecast";
	case MODEL_BCK:
	    return "Backup from Previous Run of AVN";
	case MODEL_T62:
	    return "62 wave triangular, 18 layer Spectral Model";
	case MODEL_ASSI:
	    return "Aviation Spectral Statistical Interpolation";
	case MODEL_FSSI:
	    return "Final Spectral Statistical Interpolation";
	case MODEL_ETA:
	    return "ETA mesoscale forecast model 80km res";
	case MODEL_ETA40:
	    return "ETA Model - 40 km version";
	case MODEL_ETA30:
	    return "ETA Model - 30 km version";
	case MODEL_MAPS:
	    return "MAPS Model, FSL (Isentropic; 60km at 40N)";
	case MODEL_ENSMB:
	    return "CAC Ensemble Forecasts from Spectral (ENSMB)";
	case MODEL_PWAV:
	    return "Ocean Wave model with additional physics (PWAV)";
	case MODEL_ETA48:
	    return "ETA Model - 48 km version";
	case MODEL_NWSRFS:
	    return "NWS River Forecast System (NWSRFS)";
	case MODEL_NWSFFGS:
	    return "NWS Flash Flood Guidance System (NWSFFGS)";
	case MODEL_W2PA:
	    return "WSR-88D Stage II Precipitation Analysis";
	case MODEL_W3PA:
	    return "WSR-88D Stage III Precipitation Analysis";
	case MODEL_T213:
	    return "T213 31-level Forecast Model";
	default:
	    return "unknown NMC model";
	}
    default:
	return "model for unknown center";
    }
}
