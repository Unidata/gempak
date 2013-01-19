/************************************************************************
 * na.h									*
 *									*
 * NA library common area.						*
 **									*
 * Log:									*
 * S. Jacobs/EAI	12/93						*
 * M. Li/SAIC		4/04	Added mhzrmp, and mdrct to NAPARM	*
 * R. Tian/SAIC		 7/06	Converted to C header file		*
 ************************************************************************/

#ifndef NA_H_
#define NA_H_

#include "geminc.h"
#include "gemprm.h"

/*
 * NA public APIs.
 */
#include "proto_na.h"

/*
 * NA private APIs.
 */
void na_levl ( const int *jvcord, const int *jlevel1, const int *jlevel2,
               int *ivcord, int *level1, int *level2, int *iret );
void na_rprm ( const char *prmtbl, int *iret );
void wnacmn  ( int *iret );

#define MAXNUM		255

/*
 * This structure is for the orginating center information.
 */
struct nacent {
    int  meditn;		/* GRIB edition number			*/
    char cenfil[129];		/* Current center file name		*/
    char mcnam[MAXNUM][65];	/* Array of center names		*/
    char mcabb[MAXNUM][17];	/* Array of center abbreviations	*/
};

/*
 * This structure is for the vertical coordinate information.
 */
struct navcrd {
    char vrtfil[129];		/* Current vertical coord file name	*/
    char mvcnam[MAXNUM][33];	/* Array of vert coord names		*/
    char mvunit[MAXNUM][21];	/* Array of vert coord units		*/
    char mvcord[MAXNUM][5];	/* Array of GEMPAK vert coord names	*/
    int  mvscal[MAXNUM];	/* Array of vert coord scales		*/
};

/*
 * This structure is for the parameter information.
 */
struct naparm {
    char wmofil[129];		/* Current WMO file name		*/
    char prmfil[129];		/* Current orig center file name	*/
    int  mcodtbl;		/* Parm table version number		*/ 
    int  mcenter;		/* Originating center number		*/
    char mprnam[MAXNUM][33];	/* Array of parameter names		*/
    char mpunit[MAXNUM][21];	/* Array of parameter units		*/
    char mparms[MAXNUM][13];	/* Array of GEMPAK parameter names	*/
    int  mpscal[MAXNUM];	/* Array of parameter scales		*/
    float rmssvl[MAXNUM];	/* Array of parm missing values		*/
    int  mhzrmp[MAXNUM];	/* Array of parm horizontal remap flags */
    				/*	0 = Regular area averaging for	*/
				/*	    continuous gribs		*/
				/*	1 = Discrete field, use nearest	*/
				/*	    neighbor			*/
				/*	2 = Force bilinear interpolation*/
    int  mdrct[MAXNUM];		/* Array of parm directional flags	*/
    				/*	0 = NOT a directional field	*/
				/*	1 = IS a directional field in	*/
				/*	    degrees			*/
				/*	2 = IS a directional field in	*/
				/*	    radians			*/
};

/*
 * Global variables.
 */
#ifdef NA_GLOBAL
    struct nacent		_nacent;
    struct navcrd		_navcrd;
    struct naparm		_naparm;
#else
    extern struct nacent	_nacent;
    extern struct navcrd	_navcrd;
    extern struct naparm	_naparm;
#endif

#endif
