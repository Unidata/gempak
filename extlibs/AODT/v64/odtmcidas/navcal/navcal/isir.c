/* isir.f -- translated by f2c (version 19990503).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Copyright(c) 1997, Space Science and Engineering Center, UW-Madison */
/* Refer to "McIDAS Software Acquisition and Distribution Policies" */
/* in the file  mcidas/data/license.txt */
/* *** $Id: isir.f,v 1.29 1999/05/13 20:25:42 rickk Rel $ *** */
/* $ LOGICAL FUNCTION ISIR(IDSAT,CSENS,IBAND)          (BTR) */
/* $ ISIR - determine if band of sensor on satellite sees infrared */
/* $ Input: */
/* $     IDSAT = (I) satellite ID */
/* $     CSENS = (C) sensor (instrument) */
/* $     IBAND = (I) band number */
/* $ Output description: */
/* $    True if infrared (or microwave) sensor, false otherwise */
/* $$ ISIR = COMPUTAT, SOUNDER, TOVS */
/*     SATELLITE ID'S GLEANED FROM MCIDAS REFERENCE MANUAL & "MRTVANOT" */
/* $ Name: */
/* $      isir    - Determines if the detector specified by a */
/* $                satellite/sensor/band combination is infrared. */
/* $ */
/* $ Interface: */
/* $      logical function */
/* $      isir(integer idsat,  character*(*) csens, integer iband) */
/* $ */
/* $ Input: */
/* $      idsat   - The SSEC satellite id, a number. */
/* $      csens   - Instrument on the satellite, */
/* $                i.e., 'MSU', 'AMSU', 'HIR3', HIRS', */
/* $			'QTIR', 'AVHR', 'AVH3', or 'TIRO'. */
/* $      iband   - The band. */
/* $ */
/* $ Input and Output: */
/* $      none */
/* $ */
/* $ Output: */
/* $      none */
/* $ */
/* $ Return values: */
/* $      .false. - The band is not for IR sensing. */
/* $      .true.  - The detector is for IR sensing. */
/* $ */
/* $ Remarks: */
/* $      The sensor is only used when verifying Tiros or DMSP */
/* $      instruments. */
/* $ */
/* $ Categories: */
/* $      met/science */
logical isir_(idsat, csens, iband, csens_len)
integer *idsat;
char *csens;
integer *iband;
ftnlen csens_len;
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer s_cmp();

    /* Local variables */
    extern logical irmet_(), irgvar_(), irtiro_();

/*     Function to return Meteosat IR flag */
/*     Function to return GVAR IR flag */
/*     Band 14 is interleaved VIS band for Pathfinder */
    if (*idsat < 0) {
/*        UNDEFINED */
	ret_val = FALSE_;
    } else if (*idsat <= 3) {
/*        NON-IMAGE DATA,GRAPHICS, MISCELLANEOUS */
	ret_val = FALSE_;
    } else if (*idsat <= 6) {
/*        METEOSAT */
	if (*idsat == 4) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    } else if (*idsat <= 11) {
/*        RADAR, MAMS */
	ret_val = FALSE_;
    } else if (*idsat <= 19) {
/*        GMS, ATS, SMS */
	if (*idsat % 2 == 0) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    } else if (*idsat <= 33) {
/*        GOES 1 THRU 7 */
	if (*iband == 14) {
/*           INTERLEAVED VIS FOR PATHFINDER (ALWAYS USING ODD SS#) */
	    ret_val = FALSE_;
	} else {
/*           JUST USE THE OLD ODD=IR, EVEN=VIS (WORKS FOR PF AND NON_PF) */
	    if (*idsat % 2 == 0) {
		ret_val = FALSE_;
	    } else {
/*              ALWAYS ODD SS FOR P_FINDER, ALL OTHER BANDS */
/*              WILL BE IR */
		ret_val = TRUE_;
	    }
	}
    } else if (*idsat <= 40) {
/*        UNDEFINED */
	ret_val = FALSE_;
    } else if (*idsat <= 45) {
/*        TIROS-N, NOAA-6 ... NOAA-9 */
	ret_val = irtiro_(csens, iband, (ftnlen)4);
    } else if (*idsat >= 54 && *idsat <= 58) {
/*        Meteosat-3 -4 -5 -6 -7 */
	ret_val = irmet_(iband);
    } else if (*idsat <= 59) {
/*        SPACECRAFT, RADAR, UNDEFINED */
	ret_val = FALSE_;
    } else if (*idsat <= 69) {
/*        NOAA-10, NOAA-11, NOAA-12 */
	ret_val = irtiro_(csens, iband, (ftnlen)4);
    } else if (*idsat <= 79) {
/*        GOES-I ... GOES/M */
	ret_val = irgvar_(idsat, iband);
    } else if (*idsat <= 81) {
	ret_val = FALSE_;
    } else if (*idsat <= 86) {
/*        GMS-4, -5, ... */
	ret_val = irmet_(iband);
    } else if (*idsat <= 94 && s_cmp(csens, "OLS", (ftnlen)4, (ftnlen)3) == 0 
	    && *iband == 2) {
/*        DMSP -- OLS BAND 2 IS VISIBLE */
	ret_val = FALSE_;
    } else if (*idsat <= 94) {
/*        DMSP -- ALL BANDS ARE "IR" FOR THE MICROWAVE CHANNELS */
	ret_val = TRUE_;
    } else {
/*        ERBE, RAW METEOSAT, UNDEFINED */
	ret_val = FALSE_;
    }
    return ret_val;
} /* isir_ */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
logical irtiro_(csens, iband, csens_len)
char *csens;
integer *iband;
ftnlen csens_len;
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    integer s_cmp();

/* $ LOGICAL FUNCTION IRTIRO(CSENS,IBAND)               (BTR) */
/* $ IRTIRO - determine if band of sensor on TIROS sat. sees infrared */
/* $ Input: */
/* $     CSENS = (C) sensor (instrument) */
/* $     IBAND = (I) band number */
/* $ Output description: */
/* $    True if infrared sensor, false otherwise */
/* $$ ISIR = COMPUTAT, SOUNDER, TOVS */
    if (s_cmp(csens, "HIRS", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(csens, "HIR3"
	    , (ftnlen)4, (ftnlen)4) == 0) {
	if (*iband == 20) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    } else if (s_cmp(csens, "MSU ", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(csens,
	     "AMSU", (ftnlen)4, (ftnlen)4) == 0) {
/*     COMPUTATIONS FOR MICROWAVE JUST LIKE IR */
	ret_val = TRUE_;
/*     Quick cal changes to QTIR instead of TIRO */
    } else if (s_cmp(csens, "TIRO", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(csens,
	     "QTIR", (ftnlen)4, (ftnlen)4) == 0 || s_cmp(csens, "AVH3", (
	    ftnlen)4, (ftnlen)4) == 0 || s_cmp(csens, "AVHR", (ftnlen)4, (
	    ftnlen)4) == 0) {
	if (*iband <= 2 || *iband == 6) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    } else {
	ret_val = FALSE_;
    }
    return ret_val;
} /* irtiro_ */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
logical irmet_(iband)
integer *iband;
{
    /* System generated locals */
    logical ret_val;

/* $ LOGICAL FUNCTION IRMET(CSENS,IBAND)               (JWH) */
/* $ IRMET - determine if band of sensor on MET sat. sees infrared */
/* $ Input: */
/* $     IBAND = (I) band number */
/* $ Output description: */
/* $    True if infrared or wv sensor, false otherwise */
/*     Band 8 is IR, Band 10 is water vapor...Band 1 is VIS. */
/*     WV is considered an IR channel */
    if (*iband == 1) {
	ret_val = FALSE_;
    } else {
	ret_val = TRUE_;
    }
    return ret_val;
} /* irmet_ */

/* CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC */
logical irgvar_(idsat, iband)
integer *idsat, *iband;
{
    /* System generated locals */
    logical ret_val;

/* $ LOGICAL FUNCTION IRGVAR(IDSAT, IBAND)               (CWJ) */
/* $ IRGVA - DETERMINE IF BAND OF SENSOR ON GVAR SAT. SEES INFRARED */
/* $ Input: */
/* $     IDSAT = (I) SAT NUMBER */
/* $     IBAND = (I) band number */
/* $ Output description: */
/* $    True if infrared sensor, false otherwise */
/* $$ IRGVAR = COMPUTAT, GVAR */
/* SEE IF WE ARE LOOKING AT THE SOUNDER OR IMAGER */
/*  FOR THE IMAGER BAND 1 IS VISIBLE */
    if (*idsat % 2 == 0) {
	if (*iband == 1) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    } else {
/*  FOR THE SOUNDER BAND 19 IS VISIBLE */
	if (*iband == 19) {
	    ret_val = FALSE_;
	} else {
	    ret_val = TRUE_;
	}
    }
    return ret_val;
} /* irgvar_ */

